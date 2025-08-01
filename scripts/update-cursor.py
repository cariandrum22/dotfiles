#!/usr/bin/env python3
"""Update Cursor AppImage hashes."""

import json
import re
import subprocess
import sys
from pathlib import Path
from typing import NamedTuple
from urllib.error import URLError
from urllib.request import urlopen

CURSOR_API_URL = "https://www.cursor.com/api/download?platform=linux-x64&releaseTrack=stable"


class CursorInfo(NamedTuple):
    """Cursor metadata."""

    api_response_hash: str
    download_url: str
    download_hash: str
    version: str


def fetch_json(url: str) -> dict:
    """Fetch and parse JSON from URL."""
    try:
        with urlopen(url) as response:
            return json.loads(response.read())
    except (URLError, json.JSONDecodeError) as e:
        raise RuntimeError(f"Failed to fetch {url}: {e}") from e


def get_current_hashes(cursor_file: Path) -> tuple[str, str]:
    """Extract current API response and download hashes from cursor.nix."""
    content = cursor_file.read_text()

    # Extract API response hash
    api_match = re.search(r'sha256 = "([^"]+)";', content)
    if not api_match:
        raise ValueError("Could not find API response sha256 in cursor.nix")

    # Extract download hash
    download_match = re.search(r'hash = "([^"]+)";', content)
    if not download_match:
        raise ValueError("Could not find download hash in cursor.nix")

    return api_match.group(1), download_match.group(1)


def prefetch_url(url: str, name: str | None = None, *, sri_format: bool = False) -> str:
    """Get sha256 hash for a URL using nix-prefetch-url."""
    try:
        cmd = ["nix-prefetch-url", url]
        if name:
            cmd.extend(["--name", name])

        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=True,
        )

        nix32_hash = result.stdout.strip()

        if sri_format:
            # Convert nix32 to SRI format
            convert_cmd = [
                "nix", "hash", "convert",
                "--hash-algo", "sha256", "--to", "sri", nix32_hash
            ]
            convert_result = subprocess.run(
                convert_cmd,
                capture_output=True,
                text=True,
                check=True,
            )
            return convert_result.stdout.strip()
        return nix32_hash
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Failed to prefetch {url}: {e.stderr}") from e


def get_latest_cursor_info() -> CursorInfo:
    """Fetch latest Cursor information."""
    print(f"Fetching Cursor API response from {CURSOR_API_URL}...")

    # First, get the API response hash (nix32 format for sha256 field)
    api_response_hash = prefetch_url(
        CURSOR_API_URL, "cursor-api-response", sri_format=False
    )
    print(f"  API response hash: {api_response_hash}")

    # Fetch the actual API response to get download URL
    data = fetch_json(CURSOR_API_URL)
    download_url = data["downloadUrl"]

    # Extract version from download URL
    # URL format: https://downloads.cursor.com/.../Cursor-1.2.4-x86_64.AppImage
    version_match = re.search(r'[Cc]ursor-(\d+\.\d+\.\d+)', download_url)
    if not version_match:
        raise ValueError(f"Could not extract version from URL: {download_url}")
    version = version_match.group(1)

    print(f"  Version: {version}")
    print(f"  Download URL: {download_url}")

    # Get the download hash (SRI format for hash field)
    print("Fetching download hash...")
    download_hash = prefetch_url(download_url, sri_format=True)
    print(f"  Download hash: {download_hash}")

    return CursorInfo(api_response_hash, download_url, download_hash, version)


def update_cursor_nix(cursor_file: Path, info: CursorInfo) -> bool:
    """Update cursor.nix with new hashes. Returns True if updated."""
    content = cursor_file.read_text()
    original_content = content

    # Update API response hash
    content = re.sub(
        r'(sha256 = ")[^"]+(")',
        f'\\g<1>{info.api_response_hash}\\g<2>',
        content,
        count=1
    )

    # Update download hash
    content = re.sub(
        r'(hash = ")[^"]+(")',
        f'\\g<1>{info.download_hash}\\g<2>',
        content,
        count=1
    )

    # Update version
    content = re.sub(
        r'(version = ")[^"]+(")',
        f'\\g<1>{info.version}\\g<2>',
        content,
        count=1
    )

    if content == original_content:
        return False

    cursor_file.write_text(content)
    return True


def find_cursor_file() -> Path:
    """Find cursor.nix file relative to script location."""
    script_dir = Path(__file__).parent
    return script_dir.parent / "config/home-manager/home/packages/cursor.nix"


def update_cursor() -> bool:
    """Main update logic. Returns True if updated, False if already up to date."""
    cursor_file = find_cursor_file()

    print("Checking for Cursor updates...")

    # Get current hashes
    current_api_hash, current_download_hash = get_current_hashes(cursor_file)
    print(f"Current API hash: {current_api_hash}")
    print(f"Current download hash: {current_download_hash}")

    # Get latest info
    print("\nFetching latest Cursor information...")
    latest_info = get_latest_cursor_info()

    # Check if update is needed
    if (current_api_hash == latest_info.api_response_hash and
        current_download_hash == latest_info.download_hash):
        print("\nAlready up to date")
        return False

    # Update file
    print(f"\nUpdating {cursor_file}...")
    if update_cursor_nix(cursor_file, latest_info):
        print("\n✅ Successfully updated Cursor")
        print(f"  API hash: {current_api_hash} → {latest_info.api_response_hash}")
        print(f"  Download hash: {current_download_hash} → {latest_info.download_hash}")
        print(f"  Version: {latest_info.version}")
        return True
    print("\n❌ Failed to update cursor.nix")
    return False


def main():
    """Main entry point."""
    try:
        updated = update_cursor()
        sys.exit(0 if updated else 0)
    except Exception as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
