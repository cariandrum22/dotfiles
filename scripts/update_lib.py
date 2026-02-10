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
import hashlib
import os
import re
import shutil
import subprocess  # noqa: S404
import sys
import tarfile
import tempfile
import time
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
DEFAULT_RETRIES = 3

# Pattern for stored patch hash comment in nix files
PATCH_HASH_PATTERN = r'#\s*cargoPatches\s+hash:\s*([a-f0-9]+)'
PATCH_HASH_COMMENT_TEMPLATE = "# cargoPatches hash: {}"

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
    patch_hash: str | None = None  # Hash of cargo patches for change detection


@dataclass(frozen=True)
class UpdateConfig:
    """Immutable update configuration."""
    tool_name: str
    nix_file: Path
    version_pattern: str
    hash_pattern: str
    npm_hash_pattern: str | None = None
    cargo_hash_pattern: str | None = None
    # Patch files to track for cargoHash recalculation
    cargo_patches: tuple[Path, ...] = ()


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


def calculate_patch_hash(patch_files: tuple[Path, ...]) -> str | None:
    """Calculate combined hash of patch files for change detection.

    Returns a short hash string if patches exist, None otherwise.
    """
    if not patch_files:
        return None

    hasher = hashlib.sha256()
    for patch_file in sorted(patch_files):  # Sort for deterministic ordering
        if patch_file.exists():
            hasher.update(patch_file.read_bytes())
        else:
            # Include filename even if missing to detect removal
            hasher.update(f"MISSING:{patch_file}".encode())

    return hasher.hexdigest()[:16]


def calculate_cargo_hash(nix_file: Path) -> Hash | None:  # noqa: C901, PLR0912, PLR0915
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
            f'with import (fetchTarball "{nixpkgs_url}") {{}}; callPackage {tmp_path} '
            f'{{ unstable = import (fetchTarball "{nixpkgs_url}") {{}}; }}',
        ]

        # Print debug info in CI environment
        if os.getenv('CI'):
            print("[CI Debug] Calculating cargoHash with nix-build...", file=sys.stderr)
            print(f"[CI Debug] Command: {' '.join(cmd)}", file=sys.stderr)

        result = subprocess.run(
            cmd,
            check=False, capture_output=True,
            text=True,
            timeout=300,  # 5 minutes timeout for CI environments
        )

        # Look for the correct hash in the error output
        error_text = result.stderr

        # Debug output in CI
        if os.getenv('CI') and not error_text:
            print("[CI Debug] No stderr output from nix-build", file=sys.stderr)
            if result.stdout:
                print(f"[CI Debug] stdout: {result.stdout[:500]}", file=sys.stderr)

        # Pattern to match: got:    sha256-...
        match = re.search(r'got:\s+(sha256-[A-Za-z0-9+/=]+)', error_text)
        if match:
            hash_val = Hash(match.group(1))
            if os.getenv('CI'):
                print(f"[CI Debug] Found cargoHash: {hash_val}", file=sys.stderr)
            return hash_val

        # Also check for the SRI format in error
        match = re.search(
            r'specified:\s+(sha256-[A-Za-z0-9+/=]+)\s+got:\s+(sha256-[A-Za-z0-9+/=]+)',
            error_text,
        )
        if match:
            hash_val = Hash(match.group(2))
            if os.getenv('CI'):
                print(
                    f"[CI Debug] Found cargoHash (SRI format): {hash_val}",
                    file=sys.stderr,
                )
            return hash_val

        # If no match found, log the error in CI
        if os.getenv('CI'):
            print("[CI Debug] Could not extract cargoHash from output", file=sys.stderr)
            print(
                f"[CI Debug] stderr output (first 1000 chars): {error_text[:1000]}",
                file=sys.stderr,
            )

    except subprocess.TimeoutExpired:
        # Build took too long
        if os.getenv('CI'):
            print("[CI Debug] nix-build timed out after 5 minutes", file=sys.stderr)
    except subprocess.CalledProcessError as e:
        # Expected to fail, but log in CI
        if os.getenv('CI'):
            print(f"[CI Debug] nix-build failed with error: {e}", file=sys.stderr)
    except Exception as e:  # noqa: BLE001
        # Unexpected error - catch-all needed for CI debugging
        if os.getenv('CI'):
            print(f"[CI Debug] Unexpected error: {e}", file=sys.stderr)
    finally:
        # Clean up temp file
        with contextlib.suppress(OSError):
            Path(tmp_path).unlink()

    return None


def calculate_npm_deps_hash(  # noqa: C901, PLR0912, PLR0915
    package_url: str,
    *,
    retries: int = DEFAULT_RETRIES,
) -> Hash | None:
    """Calculate npmDepsHash using prefetch-npm-deps.

    Args:
        package_url: URL to the NPM package tarball
        retries: Number of retry attempts for network operations

    Returns:
        Hash of npm dependencies or None if calculation fails
    """
    temp_dir = None
    try:
        # Create temporary directory for extraction
        temp_dir = tempfile.mkdtemp(prefix='npm-prefetch-')
        tarball_path = Path(temp_dir) / 'package.tgz'

        # Download the tarball with retries
        for attempt in range(retries):
            try:
                response = common.fetch_with_retry(package_url, retries=1)
                tarball_path.write_bytes(response.read())
                break
            except Exception as e:  # noqa: BLE001
                if attempt == retries - 1:
                    print(f"Failed to download {package_url}: {e}", file=sys.stderr)
                    return None
                time.sleep(2 ** attempt)

        # Extract tarball
        with tarfile.open(tarball_path, 'r:gz') as tar:
            tar.extractall(path=temp_dir)  # noqa: S202

        package_dir = Path(temp_dir) / 'package'
        lock_file = package_dir / 'package-lock.json'

        # Generate package-lock.json if it doesn't exist
        if not lock_file.exists():
            result = subprocess.run(
                ['npm', 'install', '--package-lock-only', '--ignore-scripts'],
                cwd=package_dir,
                capture_output=True,
                text=True,
                timeout=60,
                check=False,
            )
            if result.returncode != 0:
                print(
                    f"Failed to generate package-lock.json: {result.stderr}",
                    file=sys.stderr,
                )
                return None

        # Calculate hash using prefetch-npm-deps
        result = subprocess.run(
            ['prefetch-npm-deps', str(lock_file)],
            capture_output=True,
            text=True,
            timeout=120,
            check=True,
        )

        hash_val = result.stdout.strip()
        if hash_val.startswith('sha256-'):
            return Hash(hash_val)

        print(
            f"Unexpected hash format from prefetch-npm-deps: {hash_val}",
            file=sys.stderr,
        )
        return None  # noqa: TRY300

    except subprocess.TimeoutExpired:
        print("prefetch-npm-deps timed out", file=sys.stderr)
        return None
    except subprocess.CalledProcessError as e:
        print(f"prefetch-npm-deps failed: {e.stderr}", file=sys.stderr)
        return None
    except Exception as e:  # noqa: BLE001
        print(f"Error calculating npm deps hash: {e}", file=sys.stderr)
        return None
    finally:
        # Clean up temp directory
        if temp_dir:
            with contextlib.suppress(OSError):
                shutil.rmtree(temp_dir)


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


def create_npm_updater_with_deps_hash(
    package: str,
    nix_file: Path,  # noqa: ARG001
) -> Callable[[Version], PackageInfo]:
    """Create an NPM package updater function that also calculates npmDepsHash."""
    def updater(version: Version) -> PackageInfo:
        url = get_npm_download_url(package, version)
        hash_val = calculate_hash(url)

        # Calculate npmDepsHash using prefetch-npm-deps
        npm_deps_hash_val = calculate_npm_deps_hash(url)

        return PackageInfo(
            name=package,
            version=version,
            hash=hash_val,
            npm_deps_hash=npm_deps_hash_val,
        )
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

        # Calculate current patch hash
        patch_hash = calculate_patch_hash(config.cargo_patches)

        # First update the file with new version and source hash
        content = read_file(config.nix_file)
        temp_info = PackageInfo(
            name=repo, version=version, hash=hash_val, patch_hash=patch_hash,
        )
        updated_content = apply_updates(config, content, temp_info)
        write_file(config.nix_file, updated_content)

        # Now calculate the cargoHash with the updated file
        cargo_hash_val = calculate_cargo_hash(config.nix_file)

        return PackageInfo(
            name=repo,
            version=version,
            hash=hash_val,
            cargo_hash=cargo_hash_val,
            patch_hash=patch_hash,
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

    # Calculate patch hash for change detection
    patch_hash = calculate_patch_hash(config.cargo_patches)

    return PackageInfo(
        name=config.tool_name,
        version=Version(version_str),
        hash=Hash(hash_str),
        npm_deps_hash=npm_hash,
        cargo_hash=cargo_hash,
        patch_hash=patch_hash,
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

    # Update or add patch hash comment if patches are configured
    if config.cargo_patches and new_info.patch_hash:
        new_comment = PATCH_HASH_COMMENT_TEMPLATE.format(new_info.patch_hash)

        if re.search(PATCH_HASH_PATTERN, updated):
            # Replace existing comment
            updated = re.sub(
                r'#\s*cargoPatches\s+hash:\s*[a-f0-9]+',
                new_comment,
                updated,
            )
        else:
            # Add comment after cargoHash line
            # Match the full line: cargoHash = "...";
            updated = re.sub(
                r'(cargoHash\s*=\s*"[^"]+";)',
                rf'\1\n    {new_comment}',
                updated,
            )

    return updated


def check_for_update(
    current: PackageInfo,
    latest_version: Version,
    updater: Callable[[Version], PackageInfo],
) -> tuple[UpdateStatus, PackageInfo | None]:
    """Check if update is needed and calculate new info if so."""
    # Check if npmDepsHash looks invalid (dummy hash)
    needs_npm_deps_update = (
        current.npm_deps_hash and
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in str(current.npm_deps_hash)
    )

    # If version is different or npmDepsHash is invalid, update
    if current.version != latest_version or needs_npm_deps_update:
        new_info = updater(latest_version)
        return UpdateStatus.UPDATED, new_info

    return UpdateStatus.UP_TO_DATE, None


def check_for_rust_update(
    current: PackageInfo,
    latest_version: Version,
    updater: Callable[[Version], PackageInfo],
    *,
    stored_patch_hash: str | None = None,
) -> tuple[UpdateStatus, PackageInfo | None]:
    """Check if Rust package update is needed, including cargoHash validation.

    Args:
        current: Current package info including calculated patch hash
        latest_version: Latest available version
        updater: Function to fetch new package info
        stored_patch_hash: Previously stored patch hash (from nix file comment)
    """
    # Check if cargoHash looks invalid (dummy hash)
    needs_cargo_update = (
        current.cargo_hash and
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in str(current.cargo_hash)
    )

    # Check if patch files have changed since last cargoHash calculation
    patches_changed = (
        current.patch_hash is not None and
        current.patch_hash != stored_patch_hash
    )

    if patches_changed:
        old_hash = stored_patch_hash or "none"
        print(
            f"Patch files changed: {old_hash} -> {current.patch_hash}",
            file=sys.stderr,
        )

    # If version is different, cargoHash is invalid, or patches changed, update
    if current.version != latest_version or needs_cargo_update or patches_changed:
        new_info = updater(latest_version)
        return UpdateStatus.UPDATED, new_info

    return UpdateStatus.UP_TO_DATE, None


def update_rust_package(
    config: UpdateConfig,
    version_fetcher: Callable[[], Version],
    package_updater: Callable[[Version], PackageInfo],
) -> UpdateResult:
    """Main functional update pipeline for Rust packages."""
    try:
        # Read current state
        content = read_file(config.nix_file)
        current_info = extract_current_info(config, content)

        # Extract stored patch hash from nix file comment
        stored_patch_hash = extract_with_pattern(content, PATCH_HASH_PATTERN, group=1)

        # Fetch latest version
        latest_version = version_fetcher()

        # Check for updates (including cargoHash and patch validation)
        status, new_info = check_for_rust_update(
            current_info,
            latest_version,
            package_updater,
            stored_patch_hash=stored_patch_hash,
        )

        # Apply updates if needed
        if status == UpdateStatus.UPDATED and new_info:
            updated_content = apply_updates(config, content, new_info)
            write_file(config.nix_file, updated_content)

            # Build update message
            if current_info.version != new_info.version:
                message = (
                    f"Updated {config.tool_name} from {current_info.version} "
                    f"to {new_info.version}"
                )
            elif current_info.patch_hash != stored_patch_hash:
                message = (
                    f"Updated {config.tool_name} cargoHash due to patch changes"
                )
            else:
                message = f"Updated {config.tool_name} cargoHash"

            return UpdateResult(
                tool_name=config.tool_name,
                current=current_info,
                latest=new_info,
                status=UpdateStatus.UPDATED,
                message=message,
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

    # Use updater with npmDepsHash if pattern is configured
    if config.npm_hash_pattern:
        package_updater = create_npm_updater_with_deps_hash(package, config.nix_file)
    else:
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
    return partial(update_rust_package, config, version_fetcher, package_updater)


# ----- Output Functions ------------------------------------------------------------

def format_result(result: UpdateResult) -> str:
    """Format update result for display."""
    status_symbol = {
        UpdateStatus.UP_TO_DATE: "✓",
        UpdateStatus.UPDATED: "✓",
        UpdateStatus.ERROR: "✗",
    }

    lines = [
        f"{status_symbol[result.status]} {result.message}",
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
        if result.latest.patch_hash:
            lines.append(f"  Patch Hash: {result.latest.patch_hash}")

    return "\n".join(lines)


def print_result(result: UpdateResult) -> None:
    """Print formatted update result."""
    print(format_result(result))
