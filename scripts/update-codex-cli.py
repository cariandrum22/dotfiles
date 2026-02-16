#!/usr/bin/env python3
"""Update codex-cli tool using functional programming style.

This script updates codex-cli metadata in Nix and maintains per-platform
cargoHashes. It fetches the latest version from GitHub releases and
generates the appropriate hashes.

Usage:
    ./update-codex-cli.py

Environment:
    CODEX_SYSTEM: Override detected Nix system (e.g., x86_64-linux).
    CODEX_FORCE_CARGO_HASH: If set to 1/true, always recalc cargoHash.

Files modified:
    config/home-manager/home/packages/codex.nix
"""

from __future__ import annotations

import os
import platform
import re
import sys
import tempfile
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
)

GITHUB_REPO = "openai/codex"
VERSION_PREFIX = "rust-v"  # Codex uses "rust-v" prefix for versions

_SYSTEM_MAP = {
    ("linux", "x86_64"): "x86_64-linux",
    ("linux", "amd64"): "x86_64-linux",
    ("linux", "aarch64"): "aarch64-linux",
    ("linux", "arm64"): "aarch64-linux",
    ("darwin", "x86_64"): "x86_64-darwin",
    ("darwin", "amd64"): "x86_64-darwin",
    ("darwin", "arm64"): "aarch64-darwin",
    ("darwin", "aarch64"): "aarch64-darwin",
}


def _detect_system() -> str:
    override = os.getenv("CODEX_SYSTEM")
    if override:
        return override
    system = platform.system().lower()
    machine = platform.machine().lower()
    key = (system, machine)
    if key not in _SYSTEM_MAP:
        msg = f"Unsupported system/arch: {system}/{machine}"
        raise ValueError(msg)
    return _SYSTEM_MAP[key]


def _extract_cargo_hash(content: str, system: str) -> str | None:
    match = re.search(
        r'cargoHashes\s*=\s*{\n(?P<body>.*?)};',
        content,
        re.DOTALL,
    )
    if not match:
        return None
    body = match.group("body")
    line_match = re.search(
        rf'^\s*{re.escape(system)}\s*=\s*"([^"]+)";',
        body,
        re.MULTILINE,
    )
    return line_match.group(1) if line_match else None


def _update_cargo_hashes(content: str, system: str, new_hash: str) -> str:
    match = re.search(
        r'cargoHashes\s*=\s*{\n(?P<body>.*?)};',
        content,
        re.DOTALL,
    )
    if not match:
        msg = "Could not find cargoHashes block in codex.nix"
        raise ValueError(msg)

    body = match.group("body")
    lines = body.splitlines()

    entry_re = re.compile(rf'^(\s*){re.escape(system)}\s*=')
    for idx, line in enumerate(lines):
        if entry_re.match(line):
            lines[idx] = re.sub(
                r'=\s*"[^"]+";',
                f'= "{new_hash}";',
                line,
            )
            break
    else:
        # Insert a new entry before closing brace
        indent_match = re.search(
            r'^\s*cargoHashes\s*=\s*{', content, re.MULTILINE,
        )
        base_indent = (
            indent_match.group(0).split("cargoHashes")[0] if indent_match else ""
        )
        entry_indent = f"{base_indent}  "
        lines.append(f'{entry_indent}{system} = "{new_hash}";')

    new_body = "\n".join(lines)
    return content[:match.start("body")] + new_body + content[match.end("body"):]


def _calculate_cargo_hash(content: str) -> lib.Hash:
    temp_content = re.sub(
        r'cargoHash\s*=\s*cargoHashes\.[^;]+;',
        'cargoHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";',
        content,
        flags=re.MULTILINE,
    )
    with tempfile.NamedTemporaryFile(
        mode="w",
        suffix=".nix",
        delete=False,
        encoding="utf-8",
        dir=NIX_FILE.parent,
    ) as tmp:
        tmp.write(temp_content)
        tmp_path = Path(tmp.name)
    try:
        hash_val = lib.calculate_cargo_hash(tmp_path)
        if hash_val is None:
            msg = "Failed to compute cargoHash"
            raise RuntimeError(msg)
        return hash_val
    finally:
        tmp_path.unlink(missing_ok=True)


def _needs_cargo_update(
    content: str,
    system: str,
    *,
    force: bool,
    version_updated: bool,
) -> bool:
    existing = _extract_cargo_hash(content, system)
    return (
        force
        or version_updated
        or existing is None
        or "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in existing
    )


def main() -> int:
    """Main entry point."""
    system = _detect_system()
    force_cargo = (
        os.getenv("CODEX_FORCE_CARGO_HASH", "").lower() in {"1", "true", "yes"}
    )

    content = lib.read_file(NIX_FILE)
    current = lib.extract_current_info(CONFIG, content)

    latest_version = lib.fetch_github_release(GITHUB_REPO, VERSION_PREFIX)

    updated_content = content
    latest_hash = current.hash

    if current.version != latest_version:
        latest_hash = lib.calculate_hash(
            lib.get_github_download_url(GITHUB_REPO, latest_version),
        )
        updated_content = lib.apply_updates(
            CONFIG,
            updated_content,
            lib.PackageInfo(
                name=CONFIG.tool_name,
                version=latest_version,
                hash=latest_hash,
            ),
        )

    if _needs_cargo_update(
        updated_content,
        system,
        force=force_cargo,
        version_updated=current.version != latest_version,
    ):
        new_cargo_hash = _calculate_cargo_hash(updated_content)
        updated_content = _update_cargo_hashes(
            updated_content, system, str(new_cargo_hash),
        )

    if updated_content != content:
        lib.write_file(NIX_FILE, updated_content)

    # Build result for consistent output
    status = (
        lib.UpdateStatus.UPDATED
        if updated_content != content
        else lib.UpdateStatus.UP_TO_DATE
    )
    message = (
        f"Updated {CONFIG.tool_name} to {latest_version}"
        if status == lib.UpdateStatus.UPDATED
        else f"{CONFIG.tool_name} is up to date at version {current.version}"
    )
    result = lib.UpdateResult(
        tool_name=CONFIG.tool_name,
        current=current,
        latest=lib.PackageInfo(
            name=CONFIG.tool_name,
            version=latest_version,
            hash=latest_hash,
        ),
        status=status,
        message=message,
    )
    lib.print_result(result)
    return 0 if status != lib.UpdateStatus.ERROR else 1


if __name__ == "__main__":
    sys.exit(main())
