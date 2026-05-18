#!/usr/bin/env python3
"""Update claude-code CLI package metadata.

Claude Code now publishes a thin npm wrapper plus platform-specific native
packages. This updater tracks the wrapper version, but records hashes for the
native packages that the Nix derivation actually installs.
"""

from __future__ import annotations

import re
import sys
from concurrent.futures import ThreadPoolExecutor
from typing import TYPE_CHECKING, NamedTuple

if TYPE_CHECKING:
    from pathlib import Path

import common
import update_lib as lib

PACKAGE_NAME = "@anthropic-ai/claude-code"
PLATFORM_PACKAGES: dict[str, str] = {
    "aarch64-darwin": "@anthropic-ai/claude-code-darwin-arm64",
    "x86_64-linux": "@anthropic-ai/claude-code-linux-x64",
}

RE_VERSION = re.compile(r'version\s*=\s*"([^"]+)";')
RE_HASH_BLOCK = re.compile(
    r"(  sources = \{\n)((?:    \w+(?:-\w+)* = \{.*?\};\n)+)(  \};)",
    re.DOTALL | re.MULTILINE,
)


class ClaudeCodeConfigError(common.ConfigError):
    """Error reading claude-code.nix configuration."""


class PlatformHash(NamedTuple):
    """Hash information for a single platform."""

    system: str
    hash: str


def _claude_code_nix_path() -> Path:
    return common.resolve_script_relative(
        "..",
        "config",
        "home-manager",
        "home",
        "packages",
        "claude-code.nix",
    )


def _fetch_registry() -> dict[str, object]:
    return common.fetch_json(f"https://registry.npmjs.org/{PACKAGE_NAME}")


def _fetch_registry_versions(package_name: str) -> set[str]:
    response = common.fetch_json(f"https://registry.npmjs.org/{package_name}")
    versions = response.get("versions")
    if not isinstance(versions, dict) or not versions:
        msg = f"Could not determine published versions for {package_name}"
        raise ClaudeCodeConfigError(msg)
    return set(versions.keys())


def _version_key(version: str) -> tuple[int, ...]:
    main, _, suffix = version.partition("-")
    parts = tuple(int(part) for part in main.split("."))
    if suffix:
        return (*parts, -1)
    return (*parts, 0)


def _supported_versions_desc() -> tuple[str, list[str]]:
    registry = _fetch_registry()
    latest = registry.get("dist-tags", {}).get("latest")
    versions = registry.get("versions")

    if not isinstance(latest, str) or not latest:
        msg = "Could not determine latest claude-code version"
        raise ClaudeCodeConfigError(msg)

    if not isinstance(versions, dict) or not versions:
        msg = "Could not determine published claude-code versions"
        raise ClaudeCodeConfigError(msg)

    supported = set(versions.keys())
    with ThreadPoolExecutor(max_workers=len(PLATFORM_PACKAGES)) as pool:
        for package_versions in pool.map(
            _fetch_registry_versions,
            PLATFORM_PACKAGES.values(),
        ):
            supported &= package_versions

    if not supported:
        msg = (
            "Could not find any claude-code version published for all "
            "supported platforms"
        )
        raise ClaudeCodeConfigError(msg)

    return latest, sorted(supported, key=_version_key, reverse=True)


def _current_version_from_text(content: str) -> str:
    if (match := RE_VERSION.search(content)) is None:
        msg = "Could not find version in claude-code.nix"
        raise ClaudeCodeConfigError(msg)
    return match.group(1)


def _current_hashes_from_text(content: str) -> dict[str, str]:
    if (match := RE_HASH_BLOCK.search(content)) is None:
        msg = "Could not find sources block in claude-code.nix"
        raise ClaudeCodeConfigError(msg)

    body = match.group(2)
    hashes: dict[str, str] = {}
    current_system: str | None = None

    for line in body.splitlines():
        if (
            system_match := re.match(
                r"^\s*([A-Za-z0-9_-]+)\s*=\s*\{$",
                line,
            )
        ) is not None:
            current_system = system_match.group(1)
            continue

        if current_system is None:
            continue

        if (hash_match := re.match(r'^\s*hash\s*=\s*"([^"]+)";$', line)) is not None:
            hashes[current_system] = hash_match.group(1)
            continue

        if re.match(r"^\s*\};$", line) is not None:
            current_system = None

    return hashes


def _tarball_basename(package_name: str) -> str:
    return package_name.rsplit("/", maxsplit=1)[-1]


def _download_url(version: str, system: str) -> str:
    package_name = PLATFORM_PACKAGES[system]
    tarball_name = _tarball_basename(package_name)
    return (
        f"https://registry.npmjs.org/{package_name}/-/"
        f"{tarball_name}-{version}.tgz"
    )


def _prefetch_hash_for(version: str, system: str) -> PlatformHash:
    url = _download_url(version, system)
    try:
        return PlatformHash(
            system=system,
            hash=str(lib.calculate_hash(url)),
        )
    except common.SubprocessError as exc:
        msg = (
            f"Failed to prefetch {system} package for claude-code "
            f"{version}: {exc}"
        )
        raise ClaudeCodeConfigError(msg) from exc


def _fetch_all_hashes(version: str) -> dict[str, str]:
    with ThreadPoolExecutor(max_workers=len(PLATFORM_PACKAGES)) as pool:
        results = list(
            pool.map(
                lambda system: _prefetch_hash_for(version, system),
                PLATFORM_PACKAGES.keys(),
            ),
        )
    return {result.system: result.hash for result in results}


def _generate_hash_block(hashes: dict[str, str]) -> str:
    lines: list[str] = []
    for system in sorted(hashes.keys()):
        package_name = PLATFORM_PACKAGES[system]
        tarball_name = _tarball_basename(package_name)
        lines.extend([
            f"    {system} = {{",
            f'      url = "https://registry.npmjs.org/{package_name}/-/{tarball_name}-${{version}}.tgz";',
            f'      hash = "{hashes[system]}";',
            "    };",
        ])
    return "\n".join(lines)


def _update_nix_content(content: str, new_version: str, hashes: dict[str, str]) -> str:
    content = RE_VERSION.sub(f'version = "{new_version}";', content)

    hash_block = _generate_hash_block(hashes)
    if RE_HASH_BLOCK.search(content):
        return RE_HASH_BLOCK.sub(rf"\1{hash_block}\n\3", content)

    msg = "Could not find hash block in claude-code.nix"
    raise ClaudeCodeConfigError(msg)


def _select_target_release(*, verbose: bool) -> tuple[str, str, dict[str, str]]:
    wrapper_latest, candidate_versions = _supported_versions_desc()
    prefetch_errors: list[str] = []

    for version in candidate_versions:
        if verbose:
            print(f"\nFetching hashes for supported platforms ({version})...")

        try:
            hashes = _fetch_all_hashes(version)
        except ClaudeCodeConfigError as exc:
            prefetch_errors.append(str(exc))
            continue

        return wrapper_latest, version, hashes

    details = "\n".join(f"  - {error}" for error in prefetch_errors)
    msg = "Could not find a usable claude-code release"
    if details:
        msg = f"{msg}\n{details}"
    raise ClaudeCodeConfigError(msg)


def _hashes_match(
    current_hashes: dict[str, str],
    target_hashes: dict[str, str],
) -> bool:
    return all(
        current_hashes.get(system) == target_hashes[system]
        for system in PLATFORM_PACKAGES
    )


def _print_release_selection(
    current_version: str,
    wrapper_latest: str,
    target_version: str,
) -> None:
    print(f"Current version: {current_version}")
    print(f"Wrapper latest:  {wrapper_latest}")
    print(f"Target version:  {target_version}")
    if wrapper_latest != target_version:
        print("Selected the newest version published for every supported platform.")


def update_claude_code(*, verbose: bool = True) -> bool:
    nix_path = _claude_code_nix_path()
    content = common.read_text(nix_path)
    current_version = _current_version_from_text(content)
    current_hashes = _current_hashes_from_text(content)
    wrapper_latest, target_version, hashes = _select_target_release(verbose=verbose)

    if verbose:
        _print_release_selection(current_version, wrapper_latest, target_version)

    if current_version == target_version and _hashes_match(current_hashes, hashes):
        print(f"✓ claude-code is up to date at version {current_version}")
        return False

    if verbose:
        print("\nCalculated hashes:")
        for system, hash_value in sorted(hashes.items()):
            print(f"  {system}: {hash_value}")

    updated_content = _update_nix_content(content, target_version, hashes)
    common.write_text(nix_path, updated_content)

    if current_version == target_version:
        print(f"✓ Refreshed claude-code hashes for version {target_version}")
        return True

    print(f"✓ Updated claude-code from {current_version} to {target_version}")
    return True


def main() -> None:
    try:
        update_claude_code(verbose=False)
        sys.exit(0)
    except (
        ClaudeCodeConfigError,
        common.FetchError,
        common.SubprocessError,
        FileNotFoundError,
    ) as exc:
        print(f"\n❌ Error: {exc}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
