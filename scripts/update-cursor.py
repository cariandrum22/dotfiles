#!/usr/bin/env python3
"""Update Cursor AppImage metadata for Nix expressions.

This script automatically updates the Cursor editor AppImage version and sha256 hash
used by Nix to build the package. It fetches the latest release information from
Cursor's API and updates the local Nix expression.

Usage:
    ./update-cursor.py

    The script will:
    1. Check current version in cursor.nix
    2. Fetch latest version from Cursor's API
    3. If newer, download and calculate sha256 hash
    4. Update cursor.nix with new version and hash

Files modified:
    config/home-manager/home/packages/cursor.nix

Platform:
    Linux x86_64 only (AppImage)

Exit codes:
    0: Success (updated or already up-to-date)
    1: Error occurred

Environment:
    SCRIPT_USER_AGENT: Set custom User-Agent header (default: Python-urllib/X.X)
"""

from __future__ import annotations

import re
import sys
from typing import TYPE_CHECKING, NamedTuple

if TYPE_CHECKING:
    from pathlib import Path

import common

# Constants
CURSOR_API_URL = "https://www.cursor.com/api/download?platform=linux-x64&releaseTrack=stable"
RE_VERSION = re.compile(r'version = "([^"]+)"')
RE_DOWNLOAD_URL = re.compile(r'downloadUrl = "([^"]+)"')
RE_HASH = re.compile(r'hash = "([^"]+)"')
RE_VERSION_IN_URL = re.compile(r'[Cc]ursor-(\d+\.\d+\.\d+)')


class CursorConfigError(common.ConfigError):
    """Error reading cursor.nix configuration."""

    def __init__(self, field: str) -> None:
        """Initialize with field name."""
        super().__init__(f"Could not find {field} in cursor.nix")


class CursorVersionError(common.UpdateScriptError):
    """Error extracting version from URL."""

    def __init__(self, url: str) -> None:
        """Initialize with URL."""
        super().__init__(f"Could not extract version from URL: {url}")


class CursorInfo(NamedTuple):
    """Cursor metadata."""

    download_url: str
    download_hash: str
    version: str


# ----- Pure helpers ----------------------------------------------------------------


def _extract_field(content: str, regex: re.Pattern[str], field_name: str) -> str:
    """Extract field from Nix file content using regex."""
    if (match := regex.search(content)) is None:
        raise CursorConfigError(field_name)
    return match.group(1)


def _parse_cursor_info(content: str) -> tuple[str, str, str]:
    """Parse version, URL and hash from cursor.nix content."""
    version = _extract_field(content, RE_VERSION, "version")
    url = _extract_field(content, RE_DOWNLOAD_URL, "downloadUrl")
    hash_value = _extract_field(content, RE_HASH, "hash")
    return version, url, hash_value


def _extract_version_from_url(url: str) -> str:
    """Extract version number from download URL."""
    if (match := RE_VERSION_IN_URL.search(url)) is None:
        raise CursorVersionError(url)
    return match.group(1)


def _generate_nix_content(
    content: str, version: str, download_url: str, download_hash: str,
) -> str:
    """Update Nix file content with new values."""
    # Update version
    content = RE_VERSION.sub(f'version = "{version}"', content, count=1)
    # Update download URL
    content = RE_DOWNLOAD_URL.sub(f'downloadUrl = "{download_url}"', content, count=1)
    # Update hash
    return RE_HASH.sub(f'hash = "{download_hash}"', content, count=1)


# ----- API interaction -------------------------------------------------------------


def fetch_latest_cursor_info() -> CursorInfo:
    """Fetch latest Cursor information from API."""
    print(f"Fetching Cursor API response from {CURSOR_API_URL}...")

    # Fetch download URL from API
    data = common.fetch_json(CURSOR_API_URL)
    download_url = data["downloadUrl"]

    # Extract version from URL
    version = _extract_version_from_url(download_url)

    print(f"  Version: {version}")
    print(f"  Download URL: {download_url}")

    # Get the download hash (SRI format for hash field)
    print("Fetching download hash...")
    download_hash = _prefetch_sri_hash(download_url)
    print(f"  Download hash: {download_hash}")

    return CursorInfo(download_url, download_hash, version)


def _prefetch_sri_hash(url: str) -> str:
    """Prefetch URL and convert to SRI format hash."""
    # Get nix32 hash
    nix32_hash = common.run_nix_prefetch(url)

    # Convert to SRI format
    try:
        result = common.run_command(
            [
                "nix", "hash", "convert",
                "--hash-algo", "sha256", "--to", "sri", nix32_hash,
            ],
            check=True,
        )
        return result.stdout.strip()
    except common.SubprocessError as e:
        msg = "nix hash convert"
        raise common.SubprocessError(msg, e.error) from e


# ----- File operations -------------------------------------------------------------


def _cursor_nix_path() -> Path:
    """Get path to cursor.nix relative to this script."""
    return common.resolve_script_relative(
        "..", "config", "home-manager", "home", "packages", "cursor.nix",
    )


def update_cursor_nix(cursor_file: Path, info: CursorInfo) -> bool:
    """Update cursor.nix with new metadata.

    Returns:
        True if file was updated, False if content unchanged
    """
    content = common.read_text(cursor_file)
    original_content = content

    # Generate updated content
    new_content = _generate_nix_content(
        content, info.version, info.download_url, info.download_hash,
    )

    if new_content == original_content:
        return False

    common.write_text(cursor_file, new_content)
    return True


# ----- Main logic ------------------------------------------------------------------


def update_cursor(*, verbose: bool = True) -> bool:  # noqa: C901 - Clear sequential steps for update process
    """Update Cursor metadata if newer version available.

    Args:
        verbose: Whether to print progress messages

    Returns:
        True if updated, False if already up-to-date

    Raises:
        Various exceptions on errors
    """
    cursor_file = _cursor_nix_path()

    if verbose:
        print("Checking for Cursor updates...")

    # Get current info
    current_content = common.read_text(cursor_file)
    current_version, current_url, current_hash = _parse_cursor_info(current_content)

    if verbose:
        print(f"Current version: {current_version}")
        print(f"Current URL: {current_url}")
        print(f"Current hash: {current_hash}")

    # Get latest info
    if verbose:
        print("\nFetching latest Cursor information...")
    latest_info = fetch_latest_cursor_info()

    # Check if update is needed
    if (
        current_version == latest_info.version
        and current_url == latest_info.download_url
        and current_hash == latest_info.download_hash
    ):
        if verbose:
            print("\nAlready up to date")
        return False

    # Update file
    if verbose:
        print(f"\nUpdating {cursor_file}...")

    if update_cursor_nix(cursor_file, latest_info):
        if verbose:
            print("\n✅ Successfully updated Cursor")
            print(f"  Version: {current_version} → {latest_info.version}")
            print(f"  URL: {current_url} → {latest_info.download_url}")
            print(f"  Hash: {current_hash} → {latest_info.download_hash}")
        return True

    if verbose:
        print("\n❌ Failed to update cursor.nix")
    return False


def main() -> None:
    """Main entry point."""
    try:
        update_cursor(verbose=True)
        sys.exit(0)
    except (
        CursorConfigError,
        CursorVersionError,
        common.UpdateScriptError,
        FileNotFoundError,
    ) as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
