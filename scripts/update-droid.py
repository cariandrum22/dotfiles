#!/usr/bin/env python3
"""Update Droid CLI tool using functional programming style.

This script automatically updates the Droid CLI package by fetching
prebuilt binaries for all supported platforms and calculating their hashes.

The script can automatically detect the latest version by fetching it from
Factory AI's official install script at https://app.factory.ai/cli.

Usage:
    ./update-droid.py [VERSION]

    If VERSION is not provided, the script will fetch the latest version
    from the Factory AI install script.

Examples:
    ./update-droid.py 0.20.0  # Update to specific version
    ./update-droid.py          # Update to latest version

Files modified:
    config/home-manager/home/packages/droid.nix

Supported platforms:
    - x86_64-linux (Linux 64-bit)
    - aarch64-darwin (macOS Apple Silicon)
"""

import re
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path
from typing import NamedTuple

import common

# ----- Constants ---------------------------------------------------------------

BASE_URL = "https://downloads.factory.ai/factory-cli/releases"
INSTALL_SCRIPT_URL = "https://app.factory.ai/cli"

PLATFORMS: dict[str, str] = {
    "x86_64-linux": "linux/x64",
    "aarch64-darwin": "darwin/arm64",
}

# Precompiled regexes
RE_VERSION = re.compile(r'version\s*=\s*"([^"]+)"')
RE_INSTALL_VERSION = re.compile(r'VER="([^"]+)"')
RE_HASH_BLOCK = re.compile(
    r"(  sources = \{[^}]*)(x86_64-linux[^}]+};\s+aarch64-darwin[^}]+};)([^}]*\})",
    re.DOTALL,
)


# ----- Errors ------------------------------------------------------------------


class DroidConfigError(common.ConfigError):
    """Error reading droid.nix configuration."""


# ----- Data --------------------------------------------------------------------


class PlatformHash(NamedTuple):
    """Hash information for a single platform (immutable)."""

    system: str
    hash: str


@dataclass(frozen=True, slots=True)
class DroidInfo:
    """Droid CLI package information."""

    version: str
    hashes: dict[str, str]


# ----- Pure helpers ------------------------------------------------------------


def _fetch_latest_version() -> str:
    """Fetch latest version from Factory AI install script.

    Returns:
        Latest version string from install script

    Raises:
        common.FetchError: If script cannot be fetched
        DroidConfigError: If version cannot be parsed
    """
    script_content = common.fetch_text(INSTALL_SCRIPT_URL)
    if (m := RE_INSTALL_VERSION.search(script_content)) is None:
        msg = "Could not find version in install script"
        raise DroidConfigError(msg)
    return m.group(1)


def _current_version_from_text(content: str) -> str:
    """Extract current version from droid.nix content."""
    if (m := RE_VERSION.search(content)) is None:
        msg = "Could not find version in droid.nix"
        raise DroidConfigError(msg)
    return m.group(1)


def _download_url(version: str, system: str) -> str:
    """Construct download URL for given version and platform."""
    return f"{BASE_URL}/{version}/{PLATFORMS[system]}/droid"


def _prefetch_hash_for(version: str, system: str) -> PlatformHash:
    """Prefetch and convert hash to SRI format for a platform."""
    url = _download_url(version, system)

    try:
        # Get base32 hash from nix-prefetch-url
        base32_hash = common.run_nix_prefetch(url)

        # Convert to SRI format using nix hash convert
        result = common.run_command([
            "nix",
            "hash",
            "convert",
            "--hash-algo",
            "sha256",
            "--to",
            "sri",
            base32_hash,
        ])
        sri_hash = result.stdout.strip()

        return PlatformHash(system=system, hash=sri_hash)
    except common.SubprocessError as exc:
        msg = f"Failed to prefetch hash for {system}"
        raise common.SubprocessError(msg, exc.error) from exc


def _fetch_all_hashes(version: str) -> dict[str, str]:
    """Fetch hashes for all platforms in parallel."""
    with ThreadPoolExecutor(max_workers=len(PLATFORMS)) as pool:
        results = list(
            pool.map(
                lambda sys: _prefetch_hash_for(version, sys), PLATFORMS.keys(),
            ),
        )

    return {r.system: r.hash for r in results}


def _generate_hash_block(hashes: dict[str, str]) -> str:
    """Generate the sources block for droid.nix."""
    lines = []
    for system in sorted(hashes.keys()):
        platform_path = PLATFORMS[system].replace("/", "/")
        base_url = "https://downloads.factory.ai/factory-cli/releases"
        lines.extend([
            f'    {system} = {{',
            f'      url = "{base_url}/${{version}}/{platform_path}/droid";',
            f'      hash = "{hashes[system]}";',
            '    };',
        ])
    return "\n".join(lines)


def _update_nix_content(content: str, new_version: str, hashes: dict[str, str]) -> str:
    """Update droid.nix content with new version and hashes."""
    # Update version
    content = RE_VERSION.sub(f'version = "{new_version}";', content)

    # Update hash block
    hash_block = _generate_hash_block(hashes)
    if RE_HASH_BLOCK.search(content):
        content = RE_HASH_BLOCK.sub(rf"\1{hash_block}\3", content)
    else:
        msg = "Could not find hash block in droid.nix"
        raise DroidConfigError(msg)

    return content


def _droid_nix_path() -> Path:
    """Compute droid.nix path relative to this script."""
    return common.resolve_script_relative(
        "..",
        "config",
        "home-manager",
        "home",
        "packages",
        "droid.nix",
    )


# ----- Orchestration -----------------------------------------------------------


def update_droid(  # noqa: C901
    new_version: str, *, verbose: bool = True, quiet: bool = False,
) -> bool:
    """Update Droid CLI package to specified version.

    Args:
        new_version: Target version to update to
        verbose: Whether to print progress messages
        quiet: Whether to suppress all non-essential output

    Returns:
        True if updated, False if already at target version

    Raises:
        DroidConfigError: If droid.nix cannot be parsed
        common.SubprocessError: If prefetch commands fail
    """
    nix_path = _droid_nix_path()
    content = common.read_text(nix_path)
    current_version = _current_version_from_text(content)

    if verbose and not quiet:
        print(f"Current version: {current_version}")
        print(f"Target version:  {new_version}")

    if current_version == new_version:
        if not quiet:
            print(f"✓ droid is up to date at version {current_version}")
        return False

    if verbose and not quiet:
        print("\nFetching hashes for all platforms...")

    hashes = _fetch_all_hashes(new_version)

    if verbose and not quiet:
        print("\nCalculated hashes:")
        for system, hash_val in sorted(hashes.items()):
            print(f"  {system}: {hash_val}")

    updated_content = _update_nix_content(content, new_version, hashes)

    if verbose and not quiet:
        print(f"\nUpdating {nix_path}...")

    common.write_text(nix_path, updated_content)

    if not quiet:
        print(f"✓ Updated droid from {current_version} to {new_version}")

    return True


def main() -> None:
    """CLI entrypoint."""
    try:
        if len(sys.argv) > 1:
            version = sys.argv[1]
            # Explicit version provided - use verbose mode
            update_droid(version, verbose=True, quiet=False)
        else:
            # Auto-detect version - use quiet mode for clean output
            version = _fetch_latest_version()
            update_droid(version, verbose=False, quiet=False)

        sys.exit(0)
    except (
        DroidConfigError,
        common.SubprocessError,
        common.FetchError,
        FileNotFoundError,
    ) as exc:
        print(f"\n❌ Error: {exc}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
