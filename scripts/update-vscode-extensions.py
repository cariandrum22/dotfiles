#!/usr/bin/env python3
"""Update VSCode extensions from marketplace (functional style).

This script fetches VSCode extension metadata from the official marketplace and
generates Nix expressions with proper sha256 hashes for each extension.

Usage:
    ./update-vscode-extensions.py                   # Read from extensions file
    ./update-vscode-extensions.py --from-installed  # Read from installed VSCode

    The script will:
    1. Get extension list from file or installed VSCode
    2. Query the marketplace for each extension metadata
    3. Download and calculate sha256 hashes
    4. Generate Nix expressions for all extensions

Files:
    Input:  config/home-manager/programs/vscode/extensions
    Output: config/home-manager/programs/vscode/extensions.nix

Extension ID format:
    publisher.extension-name (e.g., ms-python.python)

Environment:
    SCRIPT_USER_AGENT: Set custom User-Agent header (default: Python-urllib/X.X)
"""

from __future__ import annotations

import argparse
import http.client
import json
import platform
import subprocess  # noqa: S404 - needed for code CLI and nix-prefetch-url
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any, TypedDict

if TYPE_CHECKING:
    from collections.abc import Iterable, Mapping

import common

# ----- Constants -------------------------------------------------------------------

MARKETPLACE_DOMAIN = "marketplace.visualstudio.com"
EXTENSION_QUERY_ENDPOINT = "/_apis/public/gallery/extensionquery"
ASSETS_DOMAIN = "{publisher}.gallery.vsassets.io"
DOWNLOAD_ENDPOINT = (
    "/_apis/public/gallery/publisher/{publisher}"
    "/extension/{extension}/{version}"
    "/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage{qs}"
)

# Build API headers dynamically
_api_headers_base = {
    "Content-Type": "application/json",
    "Accept": "application/json;api-version=3.0-preview.1",
}
if common.DEFAULT_USER_AGENT:
    _api_headers_base["User-Agent"] = common.DEFAULT_USER_AGENT
API_HEADERS: Mapping[str, str] = _api_headers_base

HTTP_TIMEOUT = 30  # seconds
HTTP_SERVER_ERROR_THRESHOLD = 500
NIX_PREFETCH_TIMEOUT = 60  # seconds
DEFAULT_WORKERS = 3
DEFAULT_RETRIES = 3

ARCH_MAP: Mapping[str, str] = {
    "x86_64": "x64",
    "i686": "x86",
    "aarch64": "arm64",
    "arm64": "arm64",
    "armv7l": "armhf",
}

# ----- Errors ---------------------------------------------------------------------


class VscodeExtensionError(RuntimeError):
    """Base error for VSCode extension operations."""


class ExtensionFetchError(VscodeExtensionError):
    """Error fetching extension information."""

    def __init__(self, ext: str, error: Exception) -> None:
        super().__init__(f"Failed to get {ext}: {error}")


class ExtensionInstalledError(VscodeExtensionError):
    """Error getting installed extensions."""

    def __init__(self, error: Exception) -> None:
        super().__init__(f"Failed to get installed extensions: {error}")


class ExtensionServerError(http.client.HTTPException):
    """Server error from marketplace."""

    def __init__(self, status: int, reason: str) -> None:
        super().__init__(f"Server error: {status} {reason}")


# ----- Data types -----------------------------------------------------------------


class Extension(TypedDict):
    """Extension identifier (publisher.name)."""

    publisher: str
    name: str


class QueryResult(TypedDict):
    """Marketplace query result."""

    status: int
    reason: str
    body: dict[str, Any]


@dataclass(frozen=True, slots=True)
class PrefetchResult:
    """Resolved VSIX URL and sha256 for a single extension version."""

    publisher: str
    name: str
    version: str
    sha256: str
    arch: str | None


# ----- Pure helpers ----------------------------------------------------------------


def _split_ext_id(ext_id: str) -> Extension:
    """Split 'publisher.name' into dict."""
    publisher, name = ext_id.split(".", maxsplit=1)
    return {"publisher": publisher, "name": name}


def _extensions_from_file(path: Path) -> list[str]:
    """Read non-empty lines as extension IDs."""
    return [
        ln.strip() for ln in path.read_text(encoding="utf-8").splitlines() if ln.strip()
    ]


def _payload_for(extension: Extension) -> dict[str, Any]:
    """Build marketplace query payload for one extension."""
    return {
        "filters": [
            {
                "criteria": [
                    {"filterType": 8, "value": "Microsoft.VisualStudio.Code"},
                    {
                        "filterType": 7,
                        "value": f"{extension['publisher']}.{extension['name']}",
                    },
                ],
                "pageNumber": 1,
                "pageSize": 100,
                "sortBy": 0,
                "sortOrder": 0,
            },
        ],
        "assetTypes": [],
        "flags": 0x200,  # include latest version only
    }


def _marketplace_request(payload: dict[str, Any]) -> tuple[int, str, bytes]:
    """POST to marketplace and return (status, reason, raw_bytes)."""
    conn = http.client.HTTPSConnection(MARKETPLACE_DOMAIN, timeout=HTTP_TIMEOUT)
    try:
        conn.request(
            "POST", EXTENSION_QUERY_ENDPOINT, json.dumps(payload), dict(API_HEADERS),
        )
        resp = conn.getresponse()
        return resp.status, resp.reason, resp.read()
    finally:
        conn.close()


def _retry_backoff(delays: Iterable[float]) -> Iterable[float]:
    """Yield a sequence of delays for retries."""
    yield from delays


def _system_arch() -> str:
    """Compute VSCode target platform (e.g., linux-x64)."""
    system = platform.system().lower()
    machine = platform.machine()
    arch = ARCH_MAP.get(machine)
    if arch is None:
        msg = f"Unsupported architecture: {machine}"
        raise ValueError(msg)
    return f"{system}-{arch}"


def _pick_version(versions: list[dict[str, Any]]) -> tuple[str, str | None]:
    """Pick best version: prefer platform-specific, else universal."""
    target = _system_arch()
    for v in versions:
        if v.get("targetPlatform") == target:
            return v["version"], v["targetPlatform"]
    for v in versions:
        if "targetPlatform" not in v:
            return v["version"], None
    msg = "No suitable version found"
    raise ValueError(msg)


def _vsix_url(publisher: str, name: str, version: str, arch: str | None) -> str:
    """Build download URL for a VSIX."""
    qs = f"?targetPlatform={arch}" if arch else ""
    return (
        "https://"
        + ASSETS_DOMAIN.format(publisher=publisher)
        + DOWNLOAD_ENDPOINT.format(
            publisher=publisher, extension=name, version=version, qs=qs,
        )
    )


def _nix_attr(pr: PrefetchResult) -> str:
    """Render a Nix attribute block."""
    base = (
        f"  {{\n"
        f'    name = "{pr.name}";\n'
        f'    publisher = "{pr.publisher}";\n'
        f'    version = "{pr.version}";\n'
        f'    sha256 = "{pr.sha256}";\n'
    )
    if pr.arch is not None:
        base += f'    arch = "{pr.arch}";\n'
    return base + "  }"


# ----- Effectful helpers (boundary) ------------------------------------------------


def get_installed_extensions() -> list[str]:
    """Return installed extensions (publisher.name)."""
    for cmd in ("code-insiders", "code"):
        try:
            proc = subprocess.run(
                [cmd, "--list-extensions"],
                capture_output=True,
                text=True,
                check=True,
            )
            exts = [ln.strip() for ln in proc.stdout.splitlines() if ln.strip()]
            print(f"✓ Found {len(exts)} installed extensions via {cmd}")
            return exts  # noqa: TRY300 - Early return pattern is clearer than else block
        except (subprocess.CalledProcessError, FileNotFoundError):
            continue
    raise ExtensionInstalledError(
        RuntimeError("Could not find VSCode command (tried code-insiders and code)"),
    )


def query_marketplace(
    extension: Extension,
    *,
    retries: int = DEFAULT_RETRIES,
    backoff_base: float = 1.0,
) -> QueryResult:
    """Query marketplace with retry/backoff; raise on final failure."""
    delays = list(
        _retry_backoff(backoff_base * (2**i) for i in range(max(retries - 1, 0))),
    )
    payload = _payload_for(extension)
    last_exc: Exception | None = None

    for attempt in range(retries):
        try:
            status, reason, body = _marketplace_request(payload)
            if status >= HTTP_SERVER_ERROR_THRESHOLD:
                raise ExtensionServerError(status, reason)
            return {"status": status, "reason": reason, "body": json.loads(body)}
        except (
            http.client.HTTPException,
            ConnectionError,
            TimeoutError,
            json.JSONDecodeError,
        ) as e:
            last_exc = e
            if attempt < len(delays):
                delay = delays[attempt]
                ext_name = f"{extension['publisher']}.{extension['name']}"
                print(
                    f"  → Retry {attempt + 1}/{retries} after {delay:.1f}s: {ext_name}",
                )
                time.sleep(delay)
            else:
                break

    ext_name = f"{extension['publisher']}.{extension['name']}"
    raise ExtensionFetchError(ext_name, last_exc or RuntimeError("unknown error"))


def prefetch_sha256(url: str, *, timeout: int = NIX_PREFETCH_TIMEOUT) -> str:
    """Run nix-prefetch-url and return sha256."""
    proc = subprocess.run(
        ["nix-prefetch-url", url],
        check=True,
        capture_output=True,
        text=True,
        timeout=timeout,
    )
    return proc.stdout.strip()


# ----- Pipeline steps --------------------------------------------------------------


def process_extensions(
    extensions: list[Extension],
    *,
    workers: int = DEFAULT_WORKERS,
    retries: int = DEFAULT_RETRIES,
) -> list[QueryResult]:
    """Fetch marketplace JSON for all extensions in parallel."""
    print(f"\nQuerying {len(extensions)} extensions with {workers} workers...")
    results: list[QueryResult] = []
    failed: list[str] = []

    with ThreadPoolExecutor(max_workers=workers) as ex:
        futures = {
            ex.submit(query_marketplace, ext, retries=retries): ext
            for ext in extensions
        }
        for fut in as_completed(futures):
            ext = futures[fut]
            name = f"{ext['publisher']}.{ext['name']}"
            try:
                res = fut.result()
                print(f"  ✓ {name}")
                results.append(res)
            except Exception as e:  # noqa: BLE001 - Catch all marketplace API errors to continue processing
                print(f"  ✗ {name}: {e}")
                failed.append(name)

    if failed:
        print(f"\n⚠️  Failed to query {len(failed)} extensions:")
        for name in failed:
            print(f"    - {name}")

    return results


def extract_prefetch(
    result: QueryResult, *, retries: int = DEFAULT_RETRIES,
) -> PrefetchResult:
    """Extract version/platform, prefetch sha256, and return structured result."""
    ext = result["body"]["results"][0]["extensions"][0]
    publisher = ext["publisher"]["publisherName"]
    name = ext["extensionName"]
    version, arch = _pick_version(ext["versions"])
    url = _vsix_url(publisher, name, version, arch)

    last_exc: Exception | None = None
    for attempt in range(retries):
        try:
            sha256 = prefetch_sha256(url, timeout=NIX_PREFETCH_TIMEOUT)
            return PrefetchResult(publisher, name, version, sha256, arch)
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            last_exc = e
            if attempt < retries - 1:
                delay = 2**attempt
                print(f"    → Retry prefetch {attempt + 1}/{retries} after {delay}s")
                time.sleep(delay)
            else:
                break

    msg = f"Failed to prefetch {publisher}.{name}: {last_exc}"
    raise RuntimeError(msg)


def write_nix_output(
    results: list[QueryResult], output_file: Path, *, retries: int,
) -> None:
    """Generate Nix list and write to file; format with nix fmt if available."""
    print("\nGenerating Nix expressions...")
    attrs: list[str] = []
    for res in results:
        try:
            pref = extract_prefetch(res, retries=retries)
            attrs.append(_nix_attr(pref))
        except (KeyError, IndexError, AttributeError, TypeError, RuntimeError) as e:
            ext = res["body"]["results"][0]["extensions"][0]
            ext_name = f"{ext['publisher']['publisherName']}.{ext['extensionName']}"
            print(f"  ✗ Failed to generate Nix for {ext_name}: {e}")

    # Deterministic ordering for stable diffs
    body = "[\n" + "\n".join(sorted(attrs)) + "\n]\n"
    output_file.write_text(body, encoding="utf-8")

    print("  → Formatting with nixfmt-rfc-style...")
    try:
        project_root = Path(__file__).parent.parent
        subprocess.run(
            ["nix", "fmt", str(output_file)],
            check=True,
            capture_output=True,
            text=True,
            cwd=str(project_root),
        )
        print("  ✓ Formatted successfully")
    except subprocess.CalledProcessError as e:
        print(f"  ⚠️  Warning: Failed to format with nixfmt-rfc-style: {e}")
        if e.stderr:
            print(f"     {e.stderr.strip()}")
    except FileNotFoundError:
        print("  ⚠️  Warning: nix command not found, skipping formatting")


# ----- CLI glue -------------------------------------------------------------------


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    """Parse CLI arguments."""
    p = argparse.ArgumentParser(description="Update VSCode extensions Nix expressions")
    p.add_argument(
        "--from-installed",
        action="store_true",
        help="Read extensions from installed VS Code instead of file",
    )
    p.add_argument(
        "--file",
        type=Path,
        help="Path to extensions file (default: ../config/.../extensions)",
    )
    p.add_argument(
        "--output",
        type=Path,
        help="Output path for extensions.nix (default: ../config/.../extensions.nix)",
    )
    p.add_argument(
        "--workers",
        type=int,
        default=DEFAULT_WORKERS,
        help=f"Number of parallel workers (default: {DEFAULT_WORKERS})",
    )
    p.add_argument(
        "--retries",
        type=int,
        default=DEFAULT_RETRIES,
        help=f"Retry attempts for requests/prefetch (default: {DEFAULT_RETRIES})",
    )
    return p.parse_args(argv)


def resolve_paths(args: argparse.Namespace) -> tuple[Path, Path]:
    """Resolve input and output paths relative to script location."""
    script_dir = Path(__file__).parent
    default_in = script_dir.parent / "config/home-manager/programs/vscode/extensions"
    default_out = (
        script_dir.parent / "config/home-manager/programs/vscode/extensions.nix"
    )
    return (args.file or default_in, args.output or default_out)


def get_extensions_list(*, from_installed: bool, extensions_file: Path) -> list[str]:
    """Return extension IDs from installed VS Code or file."""
    if from_installed:
        return get_installed_extensions()
    if not extensions_file.exists():
        msg = f"Extensions file not found: {extensions_file}"
        raise FileNotFoundError(msg)
    exts = _extensions_from_file(extensions_file)
    print(f"✓ Found {len(exts)} extensions in {extensions_file}")
    return exts


def main(argv: list[str] | None = None) -> int:
    """Program entrypoint (returns exit code)."""
    args = parse_args(argv)
    ext_path, out_path = resolve_paths(args)

    print("Checking VSCode extensions...")

    try:
        ext_ids = get_extensions_list(
            from_installed=args.from_installed, extensions_file=ext_path,
        )
    except (common.UpdateScriptError, common.SubprocessError) as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        return 1
    except FileNotFoundError as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        return 1

    exts = [_split_ext_id(x) for x in ext_ids]
    results = process_extensions(exts, workers=args.workers, retries=args.retries)
    write_nix_output(results, out_path, retries=args.retries)

    print(f"\n✅ Successfully updated {out_path}")
    print(f"  Total extensions: {len(results)}")
    return 0


def main_with_errors() -> None:
    """Wrapper to print structured errors and exit with codes."""
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        print("\n⚠️  Interrupted by user")
        sys.exit(130)
    except (
        VscodeExtensionError,
        ValueError,
        RuntimeError,
        http.client.HTTPException,
        subprocess.CalledProcessError,
    ) as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main_with_errors()
