#!/usr/bin/env python3
"""Update gemini-cli tool using functional programming style.

This script automatically updates the gemini-cli package by fetching
the latest prebuilt binary from GitHub releases.

Usage:
    ./update-gemini-cli.py

Files modified:
    config/home-manager/home/packages/gemini-cli.nix
"""

import sys
from pathlib import Path

import update_lib as lib

# Configuration
CONFIG = lib.UpdateConfig(
    tool_name="gemini-cli",
    nix_file=Path("config/home-manager/home/packages/gemini-cli.nix"),
    version_pattern=r'(version\s*=\s*")([^"]+)(";)',
    hash_pattern=r'(hash\s*=\s*")([^"]+)(";)',
)

GITHUB_REPO = "google-gemini/gemini-cli"


def main() -> int:
    """Main entry point using functional pipeline."""
    # Custom hash calculation for binary file
    def calculate_binary_hash(version: lib.Version) -> lib.Hash:
        url = f"https://github.com/{GITHUB_REPO}/releases/download/v{version}/gemini.js"
        return lib.calculate_hash(url, unpack=False)

    # Use base GitHub pipeline with modifications
    def pipeline() -> lib.UpdateResult:
        # Get latest version
        latest_version = lib.fetch_github_release(GITHUB_REPO, version_prefix="")

        # Read current file and extract info
        content = lib.read_file(CONFIG.nix_file)
        current = lib.extract_current_info(CONFIG, content)

        # Check for update
        if current.version == latest_version:
            return lib.UpdateResult(
                tool_name=CONFIG.tool_name,
                current=current,
                latest=None,
                status=lib.UpdateStatus.UP_TO_DATE,
                message=f"gemini-cli is up to date at version {latest_version}",
            )

        # Calculate hash for binary
        new_hash = calculate_binary_hash(latest_version)

        # Create new package info
        latest = lib.PackageInfo(
            name=CONFIG.tool_name,
            version=latest_version,
            hash=new_hash,
        )

        # Apply updates and write
        updated_content = lib.apply_updates(CONFIG, content, latest)
        lib.write_file(CONFIG.nix_file, updated_content)

        return lib.UpdateResult(
            tool_name=CONFIG.tool_name,
            current=current,
            latest=latest,
            status=lib.UpdateStatus.UPDATED,
            message=f"Updated gemini-cli from {current.version} to {latest_version}",
        )

    # Execute pipeline
    result = pipeline()
    lib.print_result(result)

    return 0 if result.status != lib.UpdateStatus.ERROR else 1


if __name__ == "__main__":
    sys.exit(main())
