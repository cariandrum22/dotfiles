#!/usr/bin/env python3
"""Update VSCode Insiders metadata for Nix expressions.

This script automatically updates the VSCode Insiders metadata used by Nix to build
the latest version. It fetches the current build information from Microsoft's update
API and generates sha256 hashes for each supported platform.

Usage:
    ./update-vscode-insiders.py

    The script will:
    1. Check the current commit in metadata.nix
    2. Fetch the latest commit from Microsoft's API
    3. If newer, download and hash the packages for all platforms
    4. Update metadata.nix with new version info and hashes

Files modified:
    config/home-manager/programs/vscode/metadata.nix

Supported platforms:
    - x86_64-linux (Linux 64-bit)
    - aarch64-darwin (macOS Apple Silicon)

Exit codes:
    0: Success (updated or already up-to-date)
    1: Error occurred

Environment:
    SCRIPT_USER_AGENT: Set custom User-Agent header (default: Python-urllib/X.X)
"""

from __future__ import annotations

import re
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, NamedTuple

if TYPE_CHECKING:
    from collections.abc import Mapping

import common

# ----- Constants (no magic values) -------------------------------------------------

INSIDER_CHANNEL = "insider"
LATEST_TAG = "latest"
BASE_API = "https://update.code.visualstudio.com/api/update"
HTTP_TIMEOUT = 30  # seconds

PLATFORMS: Mapping[str, str] = {
    "x86_64-linux": "linux-x64",
    "aarch64-darwin": "darwin-arm64",
}

# precompiled regexes
RE_COMMIT = re.compile(r'commit\s*=\s*"([^"]+)"')


# ----- Errors ----------------------------------------------------------------------


class VscodeConfigError(common.ConfigError):
    """Error reading metadata.nix configuration."""


class VscodeCommitMismatchError(common.UpdateScriptError):
    """Error when commit doesn't match expected value."""

    def __init__(self, expected: str, actual: str) -> None:
        super().__init__(f"Commit mismatch: expected {expected}, got {actual}")


# ----- Data ------------------------------------------------------------------------


class Metadata(NamedTuple):
    """VSCode Insiders metadata (immutable container)."""

    version: str
    commit: str
    urls: Mapping[str, str]
    hashes: Mapping[str, str]


@dataclass(frozen=True, slots=True)
class FetchResult:
    """Result for a single platform fetch, used to build dicts immutably."""

    system: str
    url: str
    sha256: str


# ----- Pure helpers ----------------------------------------------------------------


def _fetch_json(url: str) -> dict:
    """Fetch and parse JSON from URL."""
    return common.fetch_json(url, timeout=HTTP_TIMEOUT)


def _api_url(plat: str, channel: str = INSIDER_CHANNEL, tag: str = LATEST_TAG) -> str:
    """Compose VSCode update API URL."""
    return f"{BASE_API}/{plat}/{channel}/{tag}"


def _current_commit_from_text(s: str) -> str:
    """Extract current commit from metadata.nix content."""
    if (m := RE_COMMIT.search(s)) is None:
        msg = "Could not find commit in metadata.nix"
        raise VscodeConfigError(msg)
    return m.group(1)


def _latest_commit_and_version() -> tuple[str, str]:
    """Fetch latest commit and version using a reference platform."""
    data = _fetch_json(_api_url(PLATFORMS["x86_64-linux"]))
    # VSCode API returns "version" = commit, productVersion = "1.xx.0-insider"
    commit = data["version"]
    version = str(data.get("productVersion", "")).removesuffix("-insider")
    return commit, version


def _prefetch_sha256_for(commit: str, system: str, plat: str) -> FetchResult:
    """Resolve URL for platform and prefetch sha256 (nix-prefetch-url)."""
    data = _fetch_json(_api_url(plat))
    actual = data["version"]
    if actual != commit:
        raise VscodeCommitMismatchError(commit, actual)
    url = data["url"]

    try:
        out = common.run_nix_prefetch(url)
    except common.SubprocessError as exc:
        msg = f"nix-prefetch-url for {plat}"
        raise common.SubprocessError(msg, exc.error) from exc

    return FetchResult(system=system, url=url, sha256=out)


def _fetch_all(commit: str) -> tuple[Mapping[str, str], Mapping[str, str]]:
    """Fetch URLs and hashes for all platforms in parallel (immutable output)."""
    with ThreadPoolExecutor(max_workers=len(PLATFORMS)) as pool:
        results = list(
            pool.map(
                lambda kv: _prefetch_sha256_for(commit, kv[0], kv[1]),
                PLATFORMS.items(),
            ),
        )

    urls = {r.system: r.url for r in results}
    hashes = {r.system: r.sha256 for r in results}
    return urls, hashes


def _generate_nix(metadata: Metadata) -> str:
    """Render metadata.nix content deterministically."""
    url_lines = [
        f'    {system} = "{url.replace(f"/{metadata.commit}/", "/${{commit}}/")}";'
        for system, url in sorted(metadata.urls.items())
    ]
    sha_lines = [
        f'    {system} = "{sha}";' for system, sha in sorted(metadata.hashes.items())
    ]
    body = "\n".join(
        (
            "# This file is automatically updated by the "
            "update-vscode-insiders workflow",
            "rec {",
            f'  version = "{metadata.version}";',
            f'  commit = "{metadata.commit}";',
            "  url = {",
            "\n".join(url_lines),
            "  };",
            "  sha256 = {",
            "\n".join(sha_lines),
            "  };",
            "}",
            "",
        ),
    )
    return body  # noqa: RET504


def _metadata_path(script_path: Path) -> Path:
    """Compute metadata.nix path relative to this script."""
    return (
        script_path.parent
        / ".."
        / "config"
        / "home-manager"
        / "programs"
        / "vscode"
        / "metadata.nix"
    ).resolve()


# ----- Orchestration (minimal side effects) ----------------------------------------


def update_vscode_insiders(*, verbose: bool = True) -> bool:
    """Return True if updated, False if already up-to-date; print when requested."""
    meta_path = _metadata_path(Path(__file__))
    current_commit = _current_commit_from_text(common.read_text(meta_path))
    latest_commit, version = _latest_commit_and_version()

    if verbose:
        print("Checking for VSCode Insiders updates...")
        print(f"Current commit: {current_commit}")
        print(f"Latest commit:  {latest_commit}")
        print(f"Version:        {version}")

    if current_commit == latest_commit:
        if verbose:
            print("Already up to date")
        return False

    if verbose:
        print("\nFetching new sha256 hashes...")

    urls, hashes = _fetch_all(latest_commit)
    metadata = Metadata(version=version, commit=latest_commit, urls=urls, hashes=hashes)
    content = _generate_nix(metadata)

    if verbose:
        print(f"\nUpdating {meta_path}...")

    common.write_text(meta_path, content)

    if verbose:
        print("\n✅ Successfully updated VSCode Insiders")
        print(f"  Commit:  {current_commit} → {latest_commit}")
        print(f"  Version: {version}")

    return True


def main() -> None:
    """CLI entrypoint with narrow exception surface."""
    try:
        update_vscode_insiders(verbose=True)
        sys.exit(0)
    except (
        VscodeConfigError,
        VscodeCommitMismatchError,
        common.UpdateScriptError,
        FileNotFoundError,
    ) as exc:
        print(f"\n❌ Error: {exc}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
