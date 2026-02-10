#!/usr/bin/env python3
"""Update codex-cli tool using functional programming style.

This script automatically updates the codex-cli metadata used by Nix.
It fetches the latest version from GitHub releases and generates the appropriate hashes.
This includes both the source hash and the cargoHash for Rust dependencies.

The script also tracks changes to cargoPatches files. If patch files change,
the cargoHash will be recalculated even if the version remains the same.

Usage:
    ./update-codex-cli.py

Files modified:
    config/home-manager/home/packages/codex.nix
"""

import sys
from pathlib import Path

import update_lib as lib

# Path to the nix file
NIX_FILE = Path("config/home-manager/home/packages/codex.nix")

# Configuration
# Use specific patterns to match only the main package, not ramaBoringssl
CONFIG = lib.UpdateConfig(
    tool_name="codex-cli",
    nix_file=NIX_FILE,
    # Match version after pname = "codex-cli" (with newline between)
    version_pattern=r'(pname\s*=\s*"codex-cli";\n\s*version\s*=\s*")([^"]+)(")',
    # Match hash inside fetchFromGitHub block (with newlines)
    hash_pattern=r'(repo\s*=\s*"codex";\n\s*rev[^;]+;\n\s*hash\s*=\s*")([^"]+)(")',
    cargo_hash_pattern=r'(cargoHash\s*=\s*")([^"]+)(")',
    cargo_patches=(NIX_FILE.parent / "remove-cargo-bin.patch",),
)

GITHUB_REPO = "openai/codex"
VERSION_PREFIX = "rust-v"  # Codex uses "rust-v" prefix for versions


def main() -> int:
    """Main entry point using functional pipeline."""
    # Create the Rust-specific update pipeline
    update_pipeline = lib.create_rust_github_update_pipeline(
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
