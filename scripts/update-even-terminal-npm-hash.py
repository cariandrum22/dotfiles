#!/usr/bin/env python3
"""Synchronize Even Terminal's Nix npmDepsHash with its lockfile."""

from __future__ import annotations

import argparse
import re
import subprocess  # noqa: S404 - Runs the repository's pinned Nix utility.
import sys
from pathlib import Path

DEFAULT_REPOSITORY_ROOT = Path(__file__).resolve().parent.parent
PACKAGE_DIRECTORY = Path("config/home-manager/home/packages/even-terminal")
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
    if len(NPM_DEPS_HASH_PATTERN.findall(content)) != 1:
        msg = "Could not find exactly one npmDepsHash assignment"
        raise ValueError(msg)

    updated, replacements = NPM_DEPS_HASH_PATTERN.subn(
        rf"\g<1>{npm_deps_hash}\g<2>",
        content,
        count=1,
    )
    if replacements != 1:
        msg = "Failed to replace npmDepsHash assignment"
        raise ValueError(msg)
    return updated


def _synchronize_npm_deps_hash(repository_root: Path) -> tuple[str, bool]:
    package_directory = repository_root / PACKAGE_DIRECTORY
    lock_file = package_directory / "package-lock.json"
    nix_file = package_directory / "default.nix"
    npm_deps_hash = _calculate_npm_deps_hash(lock_file)
    current = nix_file.read_text(encoding="utf-8")
    updated = _replace_npm_deps_hash(current, npm_deps_hash)
    if updated == current:
        return npm_deps_hash, False

    nix_file.write_text(updated, encoding="utf-8")
    return npm_deps_hash, True


def _parse_args(argv: list[str] | None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Synchronize Even Terminal's Nix npmDepsHash with its lockfile.",
    )
    parser.add_argument(
        "--repository-root",
        type=Path,
        default=DEFAULT_REPOSITORY_ROOT,
        help="repository checkout to update (defaults to the script's repository)",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = _parse_args(argv)
    try:
        npm_deps_hash, changed = _synchronize_npm_deps_hash(
            args.repository_root.resolve(),
        )
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
