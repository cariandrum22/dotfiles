#!/usr/bin/env python3
"""Common library for update scripts - Functional style.

This module provides shared functionality for package update scripts using
functional programming principles:
- Pure functions with no side effects
- Function composition
- Immutable data structures
- Higher-order functions
"""

from __future__ import annotations

import contextlib
import re
import subprocess  # noqa: S404
import tempfile
from dataclasses import dataclass
from enum import Enum
from functools import partial, reduce
from typing import TYPE_CHECKING, NamedTuple

if TYPE_CHECKING:
    from collections.abc import Callable
    from pathlib import Path

from collections.abc import Callable  # noqa: TC003
from pathlib import Path

import common

# ----- Constants -------------------------------------------------------------------

NPM_REGISTRY_API = "https://registry.npmjs.org"
GITHUB_API = "https://api.github.com"

# ----- Types -----------------------------------------------------------------------


class UpdateStatus(Enum):
    """Status of an update operation."""
    UP_TO_DATE = "up_to_date"
    UPDATED = "updated"
    ERROR = "error"


class Version(NamedTuple):
    """Version information."""
    value: str

    def __str__(self) -> str:
        return self.value

    def __eq__(self, other: object) -> bool:
        if isinstance(other, str):
            return self.value == other
        if isinstance(other, Version):
            return self.value == other.value
        return False

    def __hash__(self) -> int:
        """Implement hash for NamedTuple subclass."""
        return hash(self.value)


class Hash(NamedTuple):
    """Hash value."""
    value: str

    def __str__(self) -> str:
        return self.value


@dataclass(frozen=True)
class PackageInfo:
    """Immutable package information."""
    name: str
    version: Version
    hash: Hash
    npm_deps_hash: Hash | None = None
    cargo_hash: Hash | None = None


@dataclass(frozen=True)
class UpdateConfig:
    """Immutable update configuration."""
    tool_name: str
    nix_file: Path
    version_pattern: str
    hash_pattern: str
    npm_hash_pattern: str | None = None
    cargo_hash_pattern: str | None = None


@dataclass(frozen=True)
class UpdateResult:
    """Immutable result of an update operation."""
    tool_name: str
    current: PackageInfo
    latest: PackageInfo | None
    status: UpdateStatus
    message: str

    @property
    def updated(self) -> bool:
        return self.status == UpdateStatus.UPDATED

    @property
    def error(self) -> bool:
        return self.status == UpdateStatus.ERROR


# ----- Pure Functions for Version Fetching ----------------------------------------

def fetch_npm_version(package: str) -> Version:
    """Fetch the latest version from NPM registry (pure function wrapper)."""
    url = f"{NPM_REGISTRY_API}/{package}"
    response = common.fetch_json(url)
    return Version(response["dist-tags"]["latest"])


def fetch_github_release(repo: str, version_prefix: str = "") -> Version:
    """Fetch the latest release version from GitHub (pure function wrapper)."""
    url = f"{GITHUB_API}/repos/{repo}/releases/latest"

    response = common.fetch_json(url)
    version = response["tag_name"]

    # If a prefix is expected and the version doesn't have it, add it
    if version_prefix and not version.startswith(version_prefix):
        # Remove 'v' prefix if present before adding our prefix
        version = version.removeprefix("v")
        version = f"{version_prefix}{version}"
    elif not version_prefix and version.startswith("v"):
        # No prefix expected but version has 'v', remove it
        version = version[1:]

    return Version(version)


def get_npm_download_url(package: str, version: Version) -> str:
    """Get NPM package download URL (pure function)."""
    package_name = package.rsplit('/', maxsplit=1)[-1]
    return f"{NPM_REGISTRY_API}/{package}/-/{package_name}-{version}.tgz"


def get_github_download_url(repo: str, version: Version) -> str:
    """Get GitHub release download URL (pure function)."""
    version_str = str(version)

    # Handle rust-v prefix specially
    if version_str.startswith("rust-v"):
        # Use as-is, it's already in the correct format
        tag_name = version_str
    elif version_str.startswith("v"):
        # Already has v prefix
        tag_name = version_str
    else:
        # Add v prefix
        tag_name = f"v{version_str}"

    return f"https://github.com/{repo}/archive/refs/tags/{tag_name}.tar.gz"


# ----- Pure Functions for Hash Calculation ----------------------------------------

def calculate_hash(url: str, *, unpack: bool = True) -> Hash:
    """Calculate nix hash for a URL (pure function wrapper)."""
    cmd = ["nix-prefetch-url"]
    if unpack:
        cmd.append("--unpack")
    cmd.append(url)

    result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    hash_value = result.stdout.strip()

    # Convert to SRI format
    sri_cmd = ["nix", "hash", "to-sri", "--type", "sha256", hash_value]
    sri_result = subprocess.run(sri_cmd, capture_output=True, text=True, check=True)
    return Hash(sri_result.stdout.strip())


def calculate_cargo_hash(nix_file: Path) -> Hash | None:
    """Calculate cargoHash by building with dummy hash and extract correct one."""
    # Create a temporary file with dummy cargoHash
    with tempfile.NamedTemporaryFile(
        mode='w', suffix='.nix', delete=False, encoding='utf-8',
    ) as tmp:
        content = read_file(nix_file)
        # Replace cargoHash with a dummy value
        dummy_content = re.sub(
            r'(cargoHash\s*=\s*")([^"]+)(")',
            r'\1sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=\3',
            content,
            flags=re.MULTILINE | re.DOTALL,
        )
        tmp.write(dummy_content)
        tmp_path = tmp.name

    try:
        # Try to build and capture the error
        nixpkgs_url = "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz"
        cmd = [
            "nix-build", "-E",
            f'with import <nixpkgs> {{}}; callPackage {tmp_path} '
            f'{{ unstable = import (fetchTarball "{nixpkgs_url}") {{}}; }}',
        ]
        result = subprocess.run(
            cmd,
            check=False, capture_output=True,
            text=True,
            timeout=60,  # 60 seconds timeout
        )

        # Look for the correct hash in the error output
        error_text = result.stderr
        # Pattern to match: got:    sha256-...
        match = re.search(r'got:\s+(sha256-[A-Za-z0-9+/=]+)', error_text)
        if match:
            return Hash(match.group(1))

        # Also check for the SRI format in error
        match = re.search(
            r'specified:\s+(sha256-[A-Za-z0-9+/=]+)\s+got:\s+(sha256-[A-Za-z0-9+/=]+)',
            error_text,
        )
        if match:
            return Hash(match.group(2))

    except subprocess.TimeoutExpired:
        # Build took too long, likely a real error
        pass
    except subprocess.CalledProcessError:
        # Expected to fail, check for hash in error
        pass
    finally:
        # Clean up temp file
        with contextlib.suppress(OSError):
            Path(tmp_path).unlink()

    return None


# ----- Pure Functions for File Operations -----------------------------------------

def read_file(path: Path) -> str:
    """Read file contents (pure function wrapper)."""
    return path.read_text(encoding="utf-8")


def write_file(path: Path, content: str) -> None:
    """Write file contents (side effect isolated here)."""
    path.write_text(content, encoding="utf-8")


# ----- Pure Functions for Pattern Matching ----------------------------------------

def extract_with_pattern(content: str, pattern: str, group: int = 2) -> str | None:
    """Extract value using regex pattern (pure function)."""
    match = re.search(pattern, content, re.MULTILINE | re.DOTALL)
    return match.group(group) if match else None


def replace_with_pattern(content: str, pattern: str, replacement: str) -> str:
    """Replace using regex pattern (pure function)."""
    def replacer(match: re.Match[str]) -> str:
        groups = match.groups()
        expected_groups = 3
        if len(groups) == expected_groups:
            return groups[0] + replacement + groups[2]
        return replacement

    return re.sub(pattern, replacer, content, flags=re.MULTILINE | re.DOTALL)


# ----- Function Composition Helpers -----------------------------------------------

def compose(*functions: Callable) -> Callable:
    """Compose functions from right to left."""
    return reduce(lambda f, g: lambda x: f(g(x)), functions, lambda x: x)


def pipe(*functions: Callable) -> Callable:
    """Pipe functions from left to right."""
    return reduce(lambda f, g: lambda x: g(f(x)), functions, lambda x: x)


# ----- Higher-Order Functions for Updates -----------------------------------------

def create_npm_updater(package: str) -> Callable[[Version], PackageInfo]:
    """Create an NPM package updater function."""
    def updater(version: Version) -> PackageInfo:
        url = get_npm_download_url(package, version)
        hash_val = calculate_hash(url)
        return PackageInfo(name=package, version=version, hash=hash_val)
    return updater


def create_github_updater(repo: str) -> Callable[[Version], PackageInfo]:
    """Create a GitHub release updater function."""
    def updater(version: Version) -> PackageInfo:
        url = get_github_download_url(repo, version)
        hash_val = calculate_hash(url)
        return PackageInfo(name=repo, version=version, hash=hash_val)
    return updater


def create_rust_github_updater(
    repo: str,
    config: UpdateConfig,
) -> Callable[[Version], PackageInfo]:
    """Create a GitHub release updater for Rust packages with cargoHash."""
    def updater(version: Version) -> PackageInfo:
        url = get_github_download_url(repo, version)
        hash_val = calculate_hash(url)

        # First update the file with new version and source hash
        content = read_file(config.nix_file)
        temp_info = PackageInfo(name=repo, version=version, hash=hash_val)
        updated_content = apply_updates(config, content, temp_info)
        write_file(config.nix_file, updated_content)

        # Now calculate the cargoHash with the updated file
        cargo_hash_val = calculate_cargo_hash(config.nix_file)

        return PackageInfo(
            name=repo,
            version=version,
            hash=hash_val,
            cargo_hash=cargo_hash_val,
        )
    return updater


# ----- Main Functional Update Pipeline --------------------------------------------

def extract_current_info(config: UpdateConfig, content: str) -> PackageInfo:
    """Extract current package info from Nix file content."""
    version_str = extract_with_pattern(content, config.version_pattern)
    hash_str = extract_with_pattern(content, config.hash_pattern)

    if not version_str or not hash_str:
        msg = f"Could not extract version or hash for {config.tool_name}"
        raise ValueError(msg)

    npm_hash = None
    if config.npm_hash_pattern:
        npm_hash_str = extract_with_pattern(content, config.npm_hash_pattern)
        npm_hash = Hash(npm_hash_str) if npm_hash_str else None

    cargo_hash = None
    if config.cargo_hash_pattern:
        cargo_hash_str = extract_with_pattern(content, config.cargo_hash_pattern)
        cargo_hash = Hash(cargo_hash_str) if cargo_hash_str else None

    return PackageInfo(
        name=config.tool_name,
        version=Version(version_str),
        hash=Hash(hash_str),
        npm_deps_hash=npm_hash,
        cargo_hash=cargo_hash,
    )


def apply_updates(config: UpdateConfig, content: str, new_info: PackageInfo) -> str:
    """Apply updates to Nix file content (pure function)."""
    # Apply version update
    updated = replace_with_pattern(
        content, config.version_pattern, str(new_info.version),
    )

    # Apply hash update
    updated = replace_with_pattern(
        updated, config.hash_pattern, str(new_info.hash),
    )

    # Apply npm deps hash update if present
    if config.npm_hash_pattern and new_info.npm_deps_hash:
        updated = replace_with_pattern(
            updated, config.npm_hash_pattern, str(new_info.npm_deps_hash),
        )

    # Apply cargo hash update if present
    if config.cargo_hash_pattern and new_info.cargo_hash:
        updated = replace_with_pattern(
            updated, config.cargo_hash_pattern, str(new_info.cargo_hash),
        )

    return updated


def check_for_update(
    current: PackageInfo,
    latest_version: Version,
    updater: Callable[[Version], PackageInfo],
) -> tuple[UpdateStatus, PackageInfo | None]:
    """Check if update is needed and calculate new info if so."""
    if current.version == latest_version:
        return UpdateStatus.UP_TO_DATE, None

    new_info = updater(latest_version)
    return UpdateStatus.UPDATED, new_info


def update_package(
    config: UpdateConfig,
    version_fetcher: Callable[[], Version],
    package_updater: Callable[[Version], PackageInfo],
) -> UpdateResult:
    """Main functional update pipeline."""
    try:
        # Read current state
        content = read_file(config.nix_file)
        current_info = extract_current_info(config, content)

        # Fetch latest version
        latest_version = version_fetcher()

        # Check for updates
        status, new_info = check_for_update(
            current_info, latest_version, package_updater,
        )

        # Apply updates if needed
        if status == UpdateStatus.UPDATED and new_info:
            updated_content = apply_updates(config, content, new_info)
            write_file(config.nix_file, updated_content)

            return UpdateResult(
                tool_name=config.tool_name,
                current=current_info,
                latest=new_info,
                status=UpdateStatus.UPDATED,
                message=(
                    f"Updated {config.tool_name} from {current_info.version} "
                    f"to {new_info.version}"
                ),
            )

        return UpdateResult(
            tool_name=config.tool_name,
            current=current_info,
            latest=current_info,
            status=UpdateStatus.UP_TO_DATE,
            message=(
                f"{config.tool_name} is up to date at version "
                f"{current_info.version}"
            ),
        )

    except (OSError, subprocess.CalledProcessError) as e:
        # File or subprocess errors
        return UpdateResult(
            tool_name=config.tool_name,
            current=PackageInfo(
                name=config.tool_name,
                version=Version("unknown"),
                hash=Hash("unknown"),
            ),
            latest=None,
            status=UpdateStatus.ERROR,
            message=f"System error updating {config.tool_name}: {e}",
        )
    except (ValueError, KeyError, TypeError) as e:
        # Data parsing or validation errors
        return UpdateResult(
            tool_name=config.tool_name,
            current=PackageInfo(
                name=config.tool_name,
                version=Version("unknown"),
                hash=Hash("unknown"),
            ),
            latest=None,
            status=UpdateStatus.ERROR,
            message=f"Data error updating {config.tool_name}: {e}",
        )
    except Exception as e:  # noqa: BLE001 - Catch-all for unexpected errors
        # Unexpected errors
        return UpdateResult(
            tool_name=config.tool_name,
            current=PackageInfo(
                name=config.tool_name,
                version=Version("unknown"),
                hash=Hash("unknown"),
            ),
            latest=None,
            status=UpdateStatus.ERROR,
            message=f"Unexpected error updating {config.tool_name}: {e}",
        )


# ----- Convenience Functions -------------------------------------------------------

def create_npm_update_pipeline(
    config: UpdateConfig,
    package: str,
) -> Callable[[], UpdateResult]:
    """Create a complete NPM update pipeline."""
    version_fetcher = partial(fetch_npm_version, package)
    package_updater = create_npm_updater(package)
    return partial(update_package, config, version_fetcher, package_updater)


def create_github_update_pipeline(
    config: UpdateConfig,
    repo: str,
    prefix: str = "",
) -> Callable[[], UpdateResult]:
    """Create a complete GitHub update pipeline."""
    version_fetcher = partial(fetch_github_release, repo, prefix)
    package_updater = create_github_updater(repo)
    return partial(update_package, config, version_fetcher, package_updater)


def create_rust_github_update_pipeline(
    config: UpdateConfig,
    repo: str,
    prefix: str = "",
) -> Callable[[], UpdateResult]:
    """Create a complete GitHub update pipeline for Rust packages."""
    version_fetcher = partial(fetch_github_release, repo, prefix)
    package_updater = create_rust_github_updater(repo, config)
    return partial(update_package, config, version_fetcher, package_updater)


# ----- Output Functions ------------------------------------------------------------

def format_result(result: UpdateResult) -> str:
    """Format update result for display."""
    status_symbol = {
        UpdateStatus.UP_TO_DATE: "✓",
        UpdateStatus.UPDATED: "✓",
        UpdateStatus.ERROR: "✗",
    }

    lines = [
        f"\n{status_symbol[result.status]} {result.message}",
    ]

    if result.status == UpdateStatus.UPDATED and result.latest:
        lines.extend([
            f"  Version: {result.current.version} → {result.latest.version}",
            f"  Hash: {result.current.hash} → {result.latest.hash}",
        ])
        if result.current.npm_deps_hash and result.latest.npm_deps_hash:
            lines.append(
                f"  NPM Deps: {result.current.npm_deps_hash} → "
                f"{result.latest.npm_deps_hash}",
            )
        if result.current.cargo_hash and result.latest.cargo_hash:
            lines.append(
                f"  Cargo Hash: {result.current.cargo_hash} → "
                f"{result.latest.cargo_hash}",
            )

    return "\n".join(lines)


def print_result(result: UpdateResult) -> None:
    """Print formatted update result."""
    print(format_result(result))
