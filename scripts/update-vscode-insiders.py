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


def get_latest_commit() -> str:
    """Fetch latest commit from GitHub API."""
    data = fetch_json("https://api.github.com/repos/microsoft/vscode/commits/main")
    return data["sha"]


def get_version(plat: str) -> str:
    """Fetch version for the given platform."""
    data = fetch_json(
        f"https://update.code.visualstudio.com/api/update/insider/{plat}/stable"
    )
    return data.get("productVersion", "").replace("-insider", "")


def prefetch_sha256(commit: str, system: str, plat: str) -> tuple[str, str]:
    """Get sha256 for a specific commit and platform."""
    url = f"https://update.code.visualstudio.com/commit:{commit}/insider/{plat}/stable"

    try:
        result = subprocess.run(
            ["nix-prefetch-url", url], capture_output=True, text=True, check=True
        )
        return system, result.stdout.strip()
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Failed to prefetch {plat}: {e.stderr}") from e


def fetch_all_hashes(commit: str) -> dict[str, str]:
    """Fetch sha256 hashes for all platforms in parallel."""
    with ThreadPoolExecutor(max_workers=len(PLATFORMS)) as executor:
        fetch_func = partial(prefetch_sha256, commit)
        futures = [
            executor.submit(fetch_func, system, plat)
            for system, plat in PLATFORMS.items()
        ]

        results = []
        for future in futures:
            try:
                system, sha256 = future.result()
                print(f"  {system}: {sha256}")
                results.append((system, sha256))
            except Exception as e:
                print(f"  Failed: {e}")
                raise

        return dict(results)


def generate_nix_content(metadata: Metadata) -> str:
    """Generate metadata.nix content."""
    sha256_lines = "\n".join(
        f'    {system} = "{sha256}";'
        for system, sha256 in sorted(metadata.hashes.items())
    )

    return f"""\
# This file is automatically updated by the update-vscode-insiders workflow
{{
  version = "{metadata.version}";
  commit = "{metadata.commit}";
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
    latest_commit = get_latest_commit()

    print(f"Current commit: {current_commit}")
    print(f"Latest commit: {latest_commit}")

    if current_commit == latest_commit:
        print("Already up to date")
        return False

    # Fetch new metadata
    print("\nFetching version information...")
    version = get_version(PLATFORMS["x86_64-linux"])
    print(f"Version: {version}")

    print("\nFetching new sha256 hashes...")
    hashes = fetch_all_hashes(latest_commit)

    # Update file
    metadata = Metadata(version, latest_commit, hashes)
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
