#!/usr/bin/env python3
"""Update VSCode extensions from marketplace.

This script queries the Visual Studio Code marketplace for extensions
and generates a Nix expression with their metadata and sha256 hashes.

Usage:
    # Update from extensions file:
    ./update-vscode-extensions.py

    # Update from currently installed extensions:
    ./update-vscode-extensions.py --from-installed

    # Update from specific file:
    ./update-vscode-extensions.py --file /path/to/extensions
"""

import argparse
import http.client
import json
import platform
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path
from typing import Any, TypedDict

MARKETPLACE_DOMAIN = "marketplace.visualstudio.com"
HTTP_SERVER_ERROR_THRESHOLD = 500
ASSETS_DOMAIN = "{publisher}.gallery.vsassets.io"
EXTENSION_QUERY_ENDPOINT = "/_apis/public/gallery/extensionquery"
DOWNLOAD_ENDPOINT = (
    "/_apis/public/gallery/publisher/{publisher}"
    "/extension/{extension}/{version}"
    "/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage{queryString}"
)


class Extension(TypedDict):
    """Extension dictionary structure."""

    publisher: str
    name: str


class QueryResult(TypedDict):
    """Query result structure."""

    status: int
    reason: str
    body: dict[str, Any]


def get_installed_extensions() -> list[str]:
    """Get list of installed extensions from VSCode.

    Returns:
        List of extension identifiers in publisher.name format

    Raises:
        RuntimeError: If VSCode command fails
    """
    try:
        # Try code-insiders first, then regular code
        for cmd in ["code-insiders", "code"]:
            try:
                result = subprocess.run(
                    [cmd, "--list-extensions"],
                    capture_output=True,
                    text=True,
                    check=True,
                )
                extensions = [
                    line.strip() for line in result.stdout.splitlines() if line.strip()
                ]
                print(f"✓ Found {len(extensions)} installed extensions via {cmd}")
                return extensions
            except (subprocess.CalledProcessError, FileNotFoundError):
                continue

        msg = "Could not find VSCode command (tried code-insiders and code)"
        raise RuntimeError(msg)
    except Exception as e:
        raise RuntimeError(f"Failed to get installed extensions: {e}") from e


def read_extensions_file(file_path: Path) -> list[str]:
    """Read extension list from file.

    Args:
        file_path: Path to the extensions file

    Returns:
        List of extension identifiers in publisher.name format
    """
    with file_path.open() as f:
        return [line.strip() for line in f if line.strip()]


def to_dict(item: str) -> Extension:
    """Convert extension string to dictionary.

    Args:
        item: Extension identifier in publisher.name format

    Returns:
        Dictionary with publisher and name keys
    """
    publisher, name = item.split(".")
    return {"publisher": publisher, "name": name}


def parse_extension_list(extensions: list[str]) -> list[Extension]:
    """Parse list of extension strings into dictionaries.

    Args:
        extensions: List of extension identifiers

    Returns:
        List of extension dictionaries
    """
    return [to_dict(ext) for ext in extensions]


def _make_marketplace_request(
    headers: dict[str, str], data: dict[str, Any]
) -> tuple[int, str, bytes]:
    """Make HTTP request to marketplace.

    Args:
        headers: HTTP headers
        data: Request payload

    Returns:
        Tuple of (status_code, reason, body)
    """
    connection = http.client.HTTPSConnection(MARKETPLACE_DOMAIN, timeout=30)
    try:
        connection.request(
            "POST", EXTENSION_QUERY_ENDPOINT, json.dumps(data), headers
        )
        response = connection.getresponse()
        return response.status, response.reason, response.read()
    finally:
        connection.close()


def query(
    extension: Extension, retries: int = 3, backoff_base: float = 1.0
) -> QueryResult:
    """Query marketplace for extension information with retry logic.

    Args:
        extension: Extension dictionary with publisher and name
        retries: Number of retry attempts (default: 3)
        backoff_base: Base delay for exponential backoff in seconds (default: 1.0)

    Returns:
        Query result with status, reason, and body

    Raises:
        RuntimeError: If all retry attempts fail
    """
    headers = {
        "Content-Type": "application/json",
        "Accept": "application/json;api-version=3.0-preview.1",
    }
    data = {
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
            }
        ],
        "assetTypes": [],
        "flags": 0x200,  # Include latest version only
    }

    last_error = None
    for attempt in range(retries):
        try:
            status, reason, body = _make_marketplace_request(headers, data)

            # Check for HTTP errors
            if status >= HTTP_SERVER_ERROR_THRESHOLD:
                raise http.client.HTTPException(f"Server error: {status} {reason}")

            return {
                "status": status,
                "reason": reason,
                "body": json.loads(body),
            }

        except (http.client.HTTPException, ConnectionError, TimeoutError) as e:
            last_error = e
            if attempt < retries - 1:
                delay = backoff_base * (2**attempt)
                error_type = type(e).__name__
                print(f"  → Retry {attempt + 1}/{retries} after {delay}s: {error_type}")
                time.sleep(delay)
            continue
        except json.JSONDecodeError as e:
            last_error = e
            ext_name = f"{extension['publisher']}.{extension['name']}"
            msg = f"Invalid JSON response for {ext_name}: {e}"
            raise RuntimeError(msg) from e
        except Exception as e:
            last_error = e
            ext_name = f"{extension['publisher']}.{extension['name']}"
            msg = f"Unexpected error querying {ext_name}: {e}"
            raise RuntimeError(msg) from e

    msg = (
        f"Failed to query {extension['publisher']}.{extension['name']} "
        f"after {retries} attempts: {last_error}"
    )
    raise RuntimeError(msg) from last_error


def results_to_nix_attr(
    publisher: str,
    name: str,
    version: str,
    sha256: str,
    arch: str | None = None,
) -> str:
    """Convert extension metadata to Nix attribute.

    Args:
        publisher: Extension publisher
        name: Extension name
        version: Extension version
        sha256: Package sha256 hash
        arch: Optional architecture string

    Returns:
        Formatted Nix attribute string
    """
    if arch is not None:
        return f"""  {{
    name = "{name}";
    publisher = "{publisher}";
    version = "{version}";
    sha256 = "{sha256}";
    arch = "{arch}";
  }}"""
    return f"""  {{
    name = "{name}";
    publisher = "{publisher}";
    version = "{version}";
    sha256 = "{sha256}";
  }}"""


def convert_system_arch_representation() -> str:
    """Convert system architecture to VSCode format.

    Returns:
        Architecture string in VSCode format (e.g., linux-x64)

    Raises:
        ValueError: If architecture is unsupported
    """
    system = platform.system().lower()
    machine = platform.machine()

    arch_map = {
        "x86_64": "x64",
        "i686": "x86",
        "aarch64": "arm64",
        "arm64": "arm64",
        "armv7l": "armhf",
    }

    arch_suffix = arch_map.get(machine)
    if not arch_suffix:
        msg = f"Unsupported architecture: {machine}"
        raise ValueError(msg)

    return f"{system}-{arch_suffix}"


def extract_version_and_platform(
    versions: list[dict[str, Any]],
) -> tuple[str, str | None]:
    """Extract version and platform from version list.

    Args:
        versions: List of version dictionaries from marketplace

    Returns:
        Tuple of (version, platform) where platform may be None

    Raises:
        ValueError: If no suitable version is found
    """
    target_platform = convert_system_arch_representation()

    # First try to find platform-specific version
    for version in versions:
        if version.get("targetPlatform") == target_platform:
            return version["version"], version["targetPlatform"]

    # Fall back to universal version
    for version in versions:
        if "targetPlatform" not in version:
            return version["version"], None

    msg = "No suitable version found"
    raise ValueError(msg)


def extract_extension_info(result: QueryResult, retries: int = 3) -> str:
    """Extract extension info and fetch sha256.

    Args:
        result: Query result from marketplace
        retries: Number of retry attempts for nix-prefetch-url (default: 3)

    Returns:
        Nix attribute string for the extension
    """
    extension = result["body"]["results"][0]["extensions"][0]
    publisher = extension["publisher"]["publisherName"]
    name = extension["extensionName"]
    version, arch = extract_version_and_platform(extension["versions"])
    query_string = f"?targetPlatform={arch}" if arch else ""

    url = (
        "https://"
        + ASSETS_DOMAIN.format(publisher=publisher)
        + DOWNLOAD_ENDPOINT.format(
            publisher=publisher,
            extension=name,
            version=version,
            queryString=query_string,
        )
    )

    # Retry logic for nix-prefetch-url
    for attempt in range(retries):
        try:
            result = subprocess.run(
                ["nix-prefetch-url", url],
                check=True,
                capture_output=True,
                encoding="utf-8",
                timeout=60,
            )
            sha256 = result.stdout.strip()
            break
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            if attempt < retries - 1:
                delay = 2 ** attempt
                print(f"    → Retry prefetch {attempt + 1}/{retries} after {delay}s")
                time.sleep(delay)
            else:
                error_msg = getattr(e, "stderr", "Unknown error")
                msg = f"Failed to prefetch {name}: {error_msg}"
                raise RuntimeError(msg) from e
    return results_to_nix_attr(publisher, name, version, sha256, arch)


def parse_args() -> argparse.Namespace:
    """Parse command line arguments.

    Returns:
        Parsed arguments
    """
    parser = argparse.ArgumentParser(
        description="Update VSCode extensions Nix expressions"
    )
    parser.add_argument(
        "--from-installed",
        action="store_true",
        help="Get extensions from installed VSCode instead of file",
    )
    parser.add_argument(
        "--file",
        type=Path,
        help=(
            "Path to extensions file "
            "(default: ../config/home-manager/programs/vscode/extensions)"
        ),
    )
    parser.add_argument(
        "--output",
        type=Path,
        help=(
            "Output path for extensions.nix "
            "(default: ../config/home-manager/programs/vscode/extensions.nix)"
        ),
    )
    parser.add_argument(
        "--workers",
        type=int,
        default=3,
        help="Number of parallel workers (default: 3)",
    )
    parser.add_argument(
        "--retries",
        type=int,
        default=3,
        help="Number of retry attempts for failed requests (default: 3)",
    )
    return parser.parse_args()


def process_extensions(
    extensions: list[Extension], workers: int = 3, retries: int = 3
) -> list[QueryResult]:
    """Query marketplace for all extensions in parallel.

    Args:
        extensions: List of extensions to query
        workers: Number of parallel workers
        retries: Number of retry attempts per extension

    Returns:
        List of successful query results
    """
    results = []
    failed = []

    print(f"\nQuerying {len(extensions)} extensions with {workers} workers...")

    with ThreadPoolExecutor(max_workers=workers) as executor:
        # Submit all tasks
        future_to_ext = {
            executor.submit(query, ext, retries): ext for ext in extensions
        }

        # Process completed tasks
        for future in as_completed(future_to_ext):
            ext = future_to_ext[future]
            ext_name = f"{ext['publisher']}.{ext['name']}"

            try:
                result = future.result()
                print(f"  ✓ {ext_name}")
                results.append(result)
            except Exception as e:
                print(f"  ✗ {ext_name}: {e}")
                failed.append(ext_name)

    if failed:
        print(f"\n⚠️  Failed to query {len(failed)} extensions:")
        for name in failed:
            print(f"    - {name}")

    return results


def get_extensions_list(
    *, from_installed: bool, extensions_file: Path
) -> list[str] | None:
    """Get extensions list from file or installed VSCode.

    Args:
        from_installed: Whether to get from installed VSCode
        extensions_file: Path to extensions file

    Returns:
        List of extensions or None if error
    """
    if from_installed:
        return get_installed_extensions()
    if not extensions_file.exists():
        print(f"\n❌ Error: Extensions file not found: {extensions_file}")
        return None
    extensions_list = read_extensions_file(extensions_file)
    print(f"✓ Found {len(extensions_list)} extensions in {extensions_file}")
    return extensions_list


def write_nix_output(
    results: list[QueryResult], output_file: Path, retries: int
) -> None:
    """Generate and write Nix expressions to file.

    Args:
        results: Query results from marketplace
        output_file: Path to write output
        retries: Number of retries for prefetch
    """
    print("\nGenerating Nix expressions...")
    extensions_nix_parts = []
    for result in results:
        try:
            nix_attr = extract_extension_info(result, retries)
            extensions_nix_parts.append(nix_attr)
        except Exception as e:
            ext_info = result["body"]["results"][0]["extensions"][0]
            ext_name = (
                f"{ext_info['publisher']['publisherName']}.{ext_info['extensionName']}"
            )
            print(f"  ✗ Failed to generate Nix for {ext_name}: {e}")

    extensions_nix = "\n".join(extensions_nix_parts)
    with output_file.open("w") as file:
        file.write("[\n")
        file.write(extensions_nix)
        file.write("\n]\n")

    # Format the generated file with nixfmt-rfc-style
    print("  → Formatting with nixfmt-rfc-style...")
    try:
        # Use nix fmt with the project's formatter for consistent formatting
        script_dir = Path(__file__).parent
        project_root = script_dir.parent
        subprocess.run(
            [
                "nix",
                "fmt",
                str(output_file),
            ],
            check=True,
            capture_output=True,
            text=True,
            cwd=str(project_root),  # Run from project root to use the flake
        )
        print("  ✓ Formatted successfully")
    except subprocess.CalledProcessError as e:
        print(f"  ⚠️  Warning: Failed to format with nixfmt-rfc-style: {e}")
        if e.stderr:
            print(f"     {e.stderr.strip()}")
    except FileNotFoundError:
        print("  ⚠️  Warning: nix command not found, skipping formatting")


def main() -> int:
    """Update VSCode extensions.

    Returns:
        Exit code (0 for success)
    """
    args = parse_args()

    # Determine paths
    script_dir = Path(__file__).parent
    extensions_file = (
        args.file
        if args.file
        else script_dir.parent / "config/home-manager/programs/vscode/extensions"
    )
    output_file = (
        args.output
        if args.output
        else script_dir.parent / "config/home-manager/programs/vscode/extensions.nix"
    )

    print("Checking VSCode extensions...")

    # Get extensions list
    extensions_list = get_extensions_list(
        from_installed=args.from_installed, extensions_file=extensions_file
    )
    if extensions_list is None:
        return 1

    # Query marketplace and generate output
    extensions = parse_extension_list(extensions_list)
    results = process_extensions(extensions, args.workers, args.retries)

    # Write output
    write_nix_output(results, output_file, args.retries)

    print(f"\n✅ Successfully updated {output_file}")
    print(f"  Total extensions: {len(results)}")
    return 0


def main_with_error_handling() -> None:
    """Entry point with error handling."""
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        print("\n⚠️  Interrupted by user")
        sys.exit(130)
    except Exception as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main_with_error_handling()
