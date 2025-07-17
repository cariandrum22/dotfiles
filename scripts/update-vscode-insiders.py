#!/usr/bin/env python3
"""Update VSCode Insiders commit and sha256 hashes."""

import json
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from functools import partial
from pathlib import Path
from typing import NamedTuple
from urllib.error import URLError
from urllib.request import urlopen

PLATFORMS = {
    "x86_64-linux": "linux-x64",
    "aarch64-darwin": "darwin-arm64",
}


class Metadata(NamedTuple):
    """VSCode Insiders metadata."""

    version: str
    commit: str
    urls: dict[str, str]
    hashes: dict[str, str]


def fetch_json(url: str) -> dict:
    """Fetch and parse JSON from URL."""
    try:
        with urlopen(url) as response:
            return json.loads(response.read())
    except (URLError, json.JSONDecodeError) as e:
        raise RuntimeError(f"Failed to fetch {url}: {e}") from e


def get_current_commit(metadata_file: Path) -> str:
    """Extract current commit from metadata.nix."""
    content = metadata_file.read_text()
    match = re.search(r'commit = "([^"]+)"', content)
    if not match:
        raise ValueError("Could not find commit in metadata.nix")
    return match.group(1)


def get_latest_info() -> tuple[str, str]:
    """Fetch latest commit and version from VSCode API."""
    # Use the Linux x64 endpoint as reference
    data = fetch_json(
        "https://update.code.visualstudio.com/api/update/linux-x64/insider/latest"
    )
    commit = data["version"]
    version = data.get("productVersion", "").replace("-insider", "")
    return commit, version




def prefetch_sha256(commit: str, system: str, plat: str) -> tuple[str, str, str]:
    """Get sha256 and URL for a specific commit and platform."""
    # First, get the actual download URL from the API
    api_url = f"https://update.code.visualstudio.com/api/update/{plat}/insider/latest"

    try:
        data = fetch_json(api_url)
        # Ensure we're getting the right commit
        if data["version"] != commit:
            raise RuntimeError(
                f"Commit mismatch: expected {commit}, got {data['version']}"
            )

        download_url = data["url"]

        result = subprocess.run(
            ["nix-prefetch-url", download_url],
            capture_output=True,
            text=True,
            check=True,
        )
        return system, download_url, result.stdout.strip()
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Failed to prefetch {plat}: {e.stderr}") from e


def fetch_all_metadata(commit: str) -> tuple[dict[str, str], dict[str, str]]:
    """Fetch URLs and sha256 hashes for all platforms in parallel."""
    with ThreadPoolExecutor(max_workers=len(PLATFORMS)) as executor:
        fetch_func = partial(prefetch_sha256, commit)
        futures = [
            executor.submit(fetch_func, system, plat)
            for system, plat in PLATFORMS.items()
        ]

        urls = {}
        hashes = {}
        for future in futures:
            try:
                system, url, sha256 = future.result()
                print(f"  {system}: {sha256}")
                urls[system] = url
                hashes[system] = sha256
            except Exception as e:
                print(f"  Failed: {e}")
                raise

        return urls, hashes


def generate_nix_content(metadata: Metadata) -> str:
    """Generate metadata.nix content."""
    # Extract commit hash from URLs to use variable reference
    url_lines = []
    for system, url in sorted(metadata.urls.items()):
        # Replace the commit hash in URL with ${commit}
        url_with_var = url.replace(f"/{metadata.commit}/", "/${commit}/")
        url_lines.append(f'    {system} = "{url_with_var}";')

    sha256_lines = "\n".join(
        f'    {system} = "{sha256}";'
        for system, sha256 in sorted(metadata.hashes.items())
    )

    return f"""\
# This file is automatically updated by the update-vscode-insiders workflow
rec {{
  version = "{metadata.version}";
  commit = "{metadata.commit}";
  url = {{
{chr(10).join(url_lines)}
  }};
  sha256 = {{
{sha256_lines}
  }};
}}
"""


def find_metadata_file() -> Path:
    """Find metadata.nix file relative to script location."""
    script_dir = Path(__file__).parent
    return script_dir.parent / "config/home-manager/programs/vscode/metadata.nix"


def update_vscode_insiders() -> bool:
    """Main update logic. Returns True if updated, False if already up to date."""
    metadata_file = find_metadata_file()

    print("Checking for VSCode Insiders updates...")

    # Check current vs latest
    current_commit = get_current_commit(metadata_file)
    latest_commit, version = get_latest_info()

    print(f"Current commit: {current_commit}")
    print(f"Latest commit: {latest_commit}")
    print(f"Version: {version}")

    if current_commit == latest_commit:
        print("Already up to date")
        return False

    print("\nFetching new sha256 hashes...")
    urls, hashes = fetch_all_metadata(latest_commit)

    # Update file
    metadata = Metadata(version, latest_commit, urls, hashes)
    content = generate_nix_content(metadata)

    print(f"\nUpdating {metadata_file}...")
    metadata_file.write_text(content)

    print("\n✅ Successfully updated VSCode Insiders")
    print(f"  Commit: {current_commit} → {latest_commit}")
    print(f"  Version: {version}")

    return True


def main():
    """Main entry point."""
    try:
        update_vscode_insiders()
        sys.exit(0)
    except Exception as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
