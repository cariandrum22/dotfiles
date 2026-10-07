#!/usr/bin/env python3
"""Update the antigravity-cli package metadata.

Antigravity CLI publishes per-platform release tarballs on GitHub. This
updater tracks the latest release that provides every supported platform's
asset and records flat (unpacked=false) hashes, matching the `fetchurl`
sources in the Nix derivation.

Files modified:
    config/home-manager/home/packages/antigravity-cli.nix
"""

from __future__ import annotations

import re
import sys
from concurrent.futures import ThreadPoolExecutor
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from pathlib import Path

import common
import update_lib as lib

GITHUB_REPO = "google-antigravity/antigravity-cli"
PLATFORM_ASSETS: dict[str, str] = {
    "aarch64-darwin": "agy_cli_mac_arm64.tar.gz",
    "x86_64-linux": "agy_cli_linux_x64_musl.tar.gz",
}

RE_VERSION = re.compile(r'(  version = ")([^"]+)(";)')


class AntigravityConfigError(common.ConfigError):
    """Error reading or updating antigravity-cli.nix."""


def _nix_path() -> Path:
    return common.resolve_script_relative(
        "..",
        "config",
        "home-manager",
        "home",
        "packages",
        "antigravity-cli.nix",
    )


def _latest_release() -> str:
    response = common.fetch_json(
        f"https://api.github.com/repos/{GITHUB_REPO}/releases/latest",
    )
    version = str(response.get("tag_name", "")).removeprefix("v")
    assets = {asset["name"] for asset in response.get("assets", [])}
    missing = sorted(set(PLATFORM_ASSETS.values()) - assets)
    if not version or missing:
        msg = f"Latest {GITHUB_REPO} release {version!r} lacks assets: {missing}"
        raise AntigravityConfigError(msg)
    return version


def _download_url(version: str, system: str) -> str:
    return (
        f"https://github.com/{GITHUB_REPO}/releases/download/{version}/"
        f"{PLATFORM_ASSETS[system]}"
    )


def _hash_for(version: str, system: str) -> tuple[str, str]:
    url = _download_url(version, system)
    return system, str(lib.calculate_hash(url, unpack=False))


def _fetch_all_hashes(version: str) -> dict[str, str]:
    with ThreadPoolExecutor(max_workers=len(PLATFORM_ASSETS)) as pool:
        return dict(
            pool.map(lambda system: _hash_for(version, system), PLATFORM_ASSETS),
        )


def _current_version(content: str) -> str:
    if (match := RE_VERSION.search(content)) is None:
        msg = "Could not find version in antigravity-cli.nix"
        raise AntigravityConfigError(msg)
    return match.group(2)


def _system_hash_pattern(system: str) -> re.Pattern[str]:
    return re.compile(
        rf'(    {re.escape(system)} = \{{\n      url = "[^"]+";\n      hash = ")'
        rf'([^"]+)(";)',
    )


def _current_hashes(content: str) -> dict[str, str]:
    hashes = {
        system: match.group(2)
        for system in PLATFORM_ASSETS
        if (match := _system_hash_pattern(system).search(content)) is not None
    }
    if set(hashes) != set(PLATFORM_ASSETS):
        msg = "Could not find a hash for every platform in antigravity-cli.nix"
        raise AntigravityConfigError(msg)
    return hashes


def _update_content(content: str, version: str, hashes: dict[str, str]) -> str:
    updated = RE_VERSION.sub(rf"\g<1>{version}\g<3>", content)
    for system, hash_value in hashes.items():
        updated = _system_hash_pattern(system).sub(
            rf"\g<1>{hash_value}\g<3>",
            updated,
        )
    return updated


def update_antigravity_cli() -> bool:
    nix_path = _nix_path()
    content = common.read_text(nix_path)
    current_version = _current_version(content)
    current_hashes = _current_hashes(content)

    target_version = _latest_release()
    hashes = _fetch_all_hashes(target_version)

    if current_version == target_version and current_hashes == hashes:
        print(f"✓ antigravity-cli is up to date at version {current_version}")
        return False

    common.write_text(nix_path, _update_content(content, target_version, hashes))
    if current_version == target_version:
        print(f"✓ Refreshed antigravity-cli hashes for version {target_version}")
    else:
        print(f"✓ Updated antigravity-cli from {current_version} to {target_version}")
    return True


def main() -> None:
    try:
        update_antigravity_cli()
        sys.exit(0)
    except (
        AntigravityConfigError,
        common.FetchError,
        common.SubprocessError,
        FileNotFoundError,
    ) as exc:
        print(f"\n❌ Error: {exc}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
