#!/usr/bin/env python3
"""Update gemini-cli tool using functional programming style.

This script automatically updates the gemini-cli package by fetching
the latest supported release asset from GitHub releases.

Usage:
    ./update-gemini-cli.py

Files modified:
    config/home-manager/home/packages/gemini-cli.nix
"""

import sys
from pathlib import Path

import common
import update_lib as lib

# Configuration
CONFIG = lib.UpdateConfig(
    tool_name="gemini-cli",
    nix_file=Path("config/home-manager/home/packages/gemini-cli.nix"),
    version_pattern=r'(version\s*=\s*")([^"]+)(";)',
    hash_pattern=r'(hash\s*=\s*")([^"]+)(";)',
)

GITHUB_REPO = "google-gemini/gemini-cli"
GITHUB_API = "https://api.github.com"
ASSET_NAME_PATTERN = r'(assetName\s*=\s*")([^"]+)(";)'
PREFERRED_ASSETS = (
    "gemini-cli-bundle.zip",
    "gemini.js",
)


def _fetch_release_asset() -> tuple[lib.Version, str]:
    """Fetch the latest release version and the supported asset name."""
    response = common.fetch_json(
        f"{GITHUB_API}/repos/{GITHUB_REPO}/releases/latest",
    )
    version = lib.Version(response["tag_name"].removeprefix("v"))
    assets = {asset["name"] for asset in response.get("assets", [])}

    for asset_name in PREFERRED_ASSETS:
        if asset_name in assets:
            return version, asset_name

    msg = f"Could not find a supported asset in latest release: {sorted(assets)}"
    raise ValueError(msg)


def main() -> int:
    """Main entry point using functional pipeline."""
    # Custom hash calculation for the published release asset
    def calculate_binary_hash(version: lib.Version, asset_name: str) -> lib.Hash:
        url = (
            f"https://github.com/{GITHUB_REPO}/releases/download/v{version}/"
            f"{asset_name}"
        )
        return lib.calculate_hash(url, unpack=asset_name.endswith(".zip"))

    # Use base GitHub pipeline with modifications
    def pipeline() -> lib.UpdateResult:
        latest_version, latest_asset = _fetch_release_asset()

        # Read current file and extract info
        content = lib.read_file(CONFIG.nix_file)
        current = lib.extract_current_info(CONFIG, content)
        current_asset = lib.extract_with_pattern(content, ASSET_NAME_PATTERN)
        if current_asset is None:
            msg = "Could not extract assetName for gemini-cli"
            raise ValueError(msg)

        # Check for update
        if current.version == latest_version and current_asset == latest_asset:
            return lib.UpdateResult(
                tool_name=CONFIG.tool_name,
                current=current,
                latest=None,
                status=lib.UpdateStatus.UP_TO_DATE,
                message=(
                    "gemini-cli is up to date at "
                    f"version {latest_version} ({latest_asset})"
                ),
            )

        # Calculate hash for release asset
        new_hash = calculate_binary_hash(latest_version, latest_asset)

        # Create new package info
        latest = lib.PackageInfo(
            name=CONFIG.tool_name,
            version=latest_version,
            hash=new_hash,
        )

        # Apply updates and write
        updated_content = lib.apply_updates(CONFIG, content, latest)
        updated_content = lib.replace_with_pattern(
            updated_content,
            ASSET_NAME_PATTERN,
            latest_asset,
        )
        lib.write_file(CONFIG.nix_file, updated_content)

        return lib.UpdateResult(
            tool_name=CONFIG.tool_name,
            current=current,
            latest=latest,
            status=lib.UpdateStatus.UPDATED,
            message=(
                f"Updated gemini-cli from {current.version} ({current_asset}) "
                f"to {latest_version} ({latest_asset})"
            ),
        )

    # Execute pipeline
    result = pipeline()
    lib.print_result(result)

    return 0 if result.status != lib.UpdateStatus.ERROR else 1


if __name__ == "__main__":
    sys.exit(main())
