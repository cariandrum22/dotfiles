#!/usr/bin/env python3
"""Update codex-cli tool using functional programming style.

This script automatically updates the codex-cli metadata used by Nix.
It fetches the latest version from GitHub releases and generates the appropriate hash.

Usage:
    ./update-codex-cli.py

Files modified:
    config/home-manager/home/packages/codex.nix
"""

import sys
from pathlib import Path

import update_lib as lib

# Configuration
CONFIG = lib.UpdateConfig(
    tool_name="codex-cli",
    nix_file=Path("config/home-manager/home/packages/codex.nix"),
    version_pattern=r'(version\s*=\s*")([^"]+)(")',
    hash_pattern=r'(hash\s*=\s*")([^"]+)(")',
)

GITHUB_REPO = "openai/codex"
VERSION_PREFIX = "rust-v"  # Codex uses "rust-v" prefix for versions


def main() -> int:
    """Main entry point using functional pipeline."""
    # Create the update pipeline
    update_pipeline = lib.create_github_update_pipeline(
        CONFIG, GITHUB_REPO, VERSION_PREFIX,
    )

    # Execute the pipeline
    result = update_pipeline()

    # Display result
    lib.print_result(result)

    # Return appropriate exit code
    return 0 if not result.error else 1


if __name__ == "__main__":
    sys.exit(main())
