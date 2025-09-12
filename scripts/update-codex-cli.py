#!/usr/bin/env python3
"""Update codex-cli tool using functional programming style.

This script automatically updates the codex-cli metadata used by Nix.
It fetches the latest version from GitHub releases and generates the appropriate hash.
Also updates the cargoHash for Rust dependencies.

Usage:
    ./update-codex-cli.py

Files modified:
    config/home-manager/home/packages/codex.nix
"""

import re
import subprocess  # noqa: S404
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
CARGO_HASH_PATTERN = r'(cargoHash\s*=\s*")([^"]+)(")'


def calculate_cargo_hash(owner: str, repo: str, rev: str) -> str | None:
    """Calculate the cargoHash for a Rust project."""
    try:
        # First, get the source hash
        url = f"https://github.com/{owner}/{repo}/archive/{rev}.tar.gz"
        cmd = ["nix-prefetch-url", "--unpack", url]
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        source_hash = result.stdout.strip()

        if not source_hash:
            return None

        # Convert to SRI hash format
        cmd = ["nix", "hash", "to-sri", "--type", "sha256", source_hash]
        result = subprocess.run(cmd, capture_output=True, text=True, check=True)
        sri_hash = result.stdout.strip()

        # Create a temporary Nix expression to build the package with a fake hash
        # This will fail but give us the correct hash in the error message
        fenix_url = "https://github.com/nix-community/fenix/archive/main.tar.gz"
        nixpkgs_url = "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz"
        nix_expr = f"""
        let
          pkgs = import <nixpkgs> {{}};
          unstable = import (fetchTarball "{nixpkgs_url}") {{}};
          fenix = import (fetchTarball "{fenix_url}") {{
            inherit (pkgs) system;
          }};
          rustToolchain = fenix.latest;
          customRustPlatform = unstable.makeRustPlatform {{
            inherit (rustToolchain) cargo rustc;
          }};
        in
        customRustPlatform.buildRustPackage rec {{
          pname = "codex-cli";
          version = "{rev}";
          src = pkgs.fetchFromGitHub {{
            owner = "{owner}";
            repo = "{repo}";
            rev = "{rev}";
            hash = "{sri_hash}";
          }};
          sourceRoot = "source/codex-rs";
          cargoHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
          nativeBuildInputs = with unstable; [ pkg-config ]
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ unstable.autoPatchelfHook ];
          buildInputs = with unstable; [ openssl ]
            ++ pkgs.lib.optionals pkgs.stdenv.isLinux [ unstable.stdenv.cc.cc.lib ];
        }}
        """

        # Run nix-build to trigger the hash mismatch error
        cmd = ["nix-build", "--expr", nix_expr, "--no-out-link", "--impure"]
        result = subprocess.run(cmd, check=False, capture_output=True, text=True)

        if result.returncode != 0:
            # Extract the correct hash from the error message
            error_output = result.stderr
            # Look for patterns like "got: sha256-..." or "specified: sha256-... but got: sha256-..."  # noqa: E501
            match = re.search(r"got:\s+(sha256-[A-Za-z0-9+/=]+)", error_output)
            if match:
                return match.group(1)
        else:
            return None

    except Exception as e:  # noqa: BLE001
        print(f"Error calculating cargo hash: {e}")
        return None


def update_cargo_hash(file_path: Path, new_hash: str) -> bool:
    """Update the cargoHash in the Nix file."""
    try:
        content = file_path.read_text(encoding="utf-8")
        new_content = re.sub(
            CARGO_HASH_PATTERN,
            f'\\g<1>{new_hash}\\g<3>',
            content,
        )

        if new_content != content:
            file_path.write_text(new_content, encoding="utf-8")
            return True
        return False  # noqa: TRY300
    except Exception as e:  # noqa: BLE001
        print(f"Error updating cargo hash: {e}")
        return False


def main() -> int:
    """Main entry point using functional pipeline."""
    # Create the update pipeline
    update_pipeline = lib.create_github_update_pipeline(
        CONFIG, GITHUB_REPO, VERSION_PREFIX,
    )

    # Execute the pipeline
    result = update_pipeline()

    # If the version was updated, also update cargoHash
    if result.status == lib.UpdateStatus.UPDATED:
        # The version from result.latest.version already includes the prefix
        full_version = str(result.latest.version)
        print(f"  → Calculating cargoHash for version {full_version}...")

        cargo_hash = calculate_cargo_hash("openai", "codex", full_version)

        if cargo_hash:
            print(f"  → Updating cargoHash to {cargo_hash}")
            if update_cargo_hash(CONFIG.nix_file, cargo_hash):
                print("  ✓ CargoHash updated successfully")
            else:
                print("  ✗ Failed to update cargoHash")
                result.error = "Failed to update cargoHash"
        else:
            print("  ✗ Failed to calculate cargoHash")
            print("  → Please update cargoHash manually after the first build failure")

    # Display result
    lib.print_result(result)

    # Return appropriate exit code
    return 0 if not result.error else 1


if __name__ == "__main__":
    sys.exit(main())
