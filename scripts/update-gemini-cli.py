#!/usr/bin/env python3
"""Update gemini-cli tool using functional programming style.

This script automatically updates the gemini-cli metadata used by Nix.
It fetches the latest version from GitHub releases and calculates both
the source hash and npm dependencies hash.

Usage:
    ./update-gemini-cli.py

Files modified:
    config/home-manager/home/packages/default.nix
"""

import sys
from pathlib import Path

import update_lib as lib

# Configuration
CONFIG = lib.UpdateConfig(
    tool_name="gemini-cli",
    nix_file=Path("config/home-manager/home/packages/default.nix"),
    version_pattern=(
        r'(unstable\.gemini-cli\.overrideAttrs.*?version\s*=\s*")'
        r'([^"]+)(".*?#\s*gemini-cli version)'
    ),
    hash_pattern=r'(unstable\.gemini-cli\.overrideAttrs.*?hash\s*=\s*")([^"]+)(";)',
)

GITHUB_REPO = "google-gemini/gemini-cli"


def main() -> int:
    """Main entry point using functional pipeline."""
    # Create the update pipeline
    # Note: gemini-cli doesn't use version prefix like "rust-v"
    update_pipeline = lib.create_github_update_pipeline(CONFIG, GITHUB_REPO, prefix="")

    # Execute the pipeline
    result = update_pipeline()

    # Display result
    lib.print_result(result)

    # Return appropriate exit code
    return 0 if not result.error else 1


if __name__ == "__main__":
    sys.exit(main())
