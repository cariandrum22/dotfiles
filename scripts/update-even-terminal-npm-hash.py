#!/usr/bin/env python3
"""Synchronize Even Terminal's Nix npmDepsHash with its lockfile."""

from __future__ import annotations

import re
import subprocess  # noqa: S404 - Runs the repository's pinned Nix utility.
import sys
from pathlib import Path

REPOSITORY_ROOT = Path(__file__).resolve().parent.parent
PACKAGE_DIRECTORY = (
    REPOSITORY_ROOT / "config/home-manager/home/packages/even-terminal"
)
LOCK_FILE = PACKAGE_DIRECTORY / "package-lock.json"
NIX_FILE = PACKAGE_DIRECTORY / "default.nix"
NPM_DEPS_HASH_PATTERN = re.compile(r'(npmDepsHash\s*=\s*")[^"]+(";)')
PREFETCH_TIMEOUT_SECONDS = 300


def _calculate_npm_deps_hash(lock_file: Path) -> str:
    result = subprocess.run(
        ["prefetch-npm-deps", str(lock_file)],
        check=True,
        capture_output=True,
        text=True,
        timeout=PREFETCH_TIMEOUT_SECONDS,
    )
    npm_deps_hash = result.stdout.strip()
    if not npm_deps_hash.startswith("sha256-"):
        msg = f"Unexpected prefetch-npm-deps output: {npm_deps_hash}"
        raise ValueError(msg)
    return npm_deps_hash


def _replace_npm_deps_hash(content: str, npm_deps_hash: str) -> str:
    updated, replacements = NPM_DEPS_HASH_PATTERN.subn(
        rf"\g<1>{npm_deps_hash}\g<2>",
        content,
        count=1,
    )
    if replacements != 1:
        msg = "Could not find exactly one npmDepsHash assignment"
        raise ValueError(msg)
    return updated


def _synchronize_npm_deps_hash() -> tuple[str, bool]:
    npm_deps_hash = _calculate_npm_deps_hash(LOCK_FILE)
    current = NIX_FILE.read_text(encoding="utf-8")
    updated = _replace_npm_deps_hash(current, npm_deps_hash)
    if updated == current:
        return npm_deps_hash, False

    NIX_FILE.write_text(updated, encoding="utf-8")
    return npm_deps_hash, True


def main() -> int:
    try:
        npm_deps_hash, changed = _synchronize_npm_deps_hash()
    except (OSError, subprocess.SubprocessError, ValueError) as error:
        print(f"Failed to update Even Terminal npmDepsHash: {error}", file=sys.stderr)
        return 1

    if changed:
        print(f"Updated Even Terminal npmDepsHash: {npm_deps_hash}")
    else:
        print(f"Even Terminal npmDepsHash is current: {npm_deps_hash}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
