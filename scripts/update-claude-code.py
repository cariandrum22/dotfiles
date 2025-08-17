#!/usr/bin/env python3
"""Update claude-code CLI tool using functional programming style.

This script automatically updates the claude-code metadata used by Nix.
It fetches the latest version from NPM and generates the appropriate hash.

Usage:
    ./update-claude-code.py

Files modified:
    config/home-manager/home/packages/default.nix
"""

import sys
from pathlib import Path

import update_lib as lib

# Configuration
CONFIG = lib.UpdateConfig(
    tool_name="claude-code",
    nix_file=Path("config/home-manager/home/packages/default.nix"),
    version_pattern=r'(unstable\.claude-code\.overrideAttrs\s*\(\s*finalAttrs:\s*oldAttrs:\s*rec\s*\{\s*version\s*=\s*")([^"]+)(")',
    hash_pattern=r'(unstable\.claude-code\.overrideAttrs.*?hash\s*=\s*")([^"]+)(")',
)

PACKAGE_NAME = "@anthropic-ai/claude-code"


def main() -> int:
    """Main entry point using functional pipeline."""
    # Create the update pipeline
    update_pipeline = lib.create_npm_update_pipeline(CONFIG, PACKAGE_NAME)

    # Execute the pipeline
    result = update_pipeline()

    # Display result
    lib.print_result(result)

    # Return appropriate exit code
    return 0 if not result.error else 1


if __name__ == "__main__":
    sys.exit(main())
