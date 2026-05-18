#!/usr/bin/env python3
"""Update codex-cli tool using functional programming style.

This script updates codex-cli metadata in Nix and maintains per-platform
cargoHashes, rusty_v8 archive hashes, and LiveKit WebRTC archive hashes. It
fetches the latest version from GitHub releases and generates the appropriate
hashes.

Usage:
    ./update-codex-cli.py

Environment:
    CODEX_SYSTEM: Override detected Nix system (e.g., x86_64-linux).
    CODEX_FORCE_CARGO_HASH: If set to 1/true, always recalc cargoHash.

Files modified:
    config/home-manager/home/packages/codex.nix
    config/home-manager/home/packages/rusty-v8-prebuilt-out-dir.patch
    config/home-manager/home/packages/stub-runfiles.patch
"""

from __future__ import annotations

import base64
import json
import os
import platform
import re
import shutil
import subprocess  # noqa: S404
import sys
import tarfile
import tempfile
from pathlib import Path
from typing import TYPE_CHECKING
from urllib.request import urlopen

import common
import update_lib as lib

if TYPE_CHECKING:
    from collections.abc import Callable

# Path to the nix file
NIX_FILE = Path("config/home-manager/home/packages/codex.nix")
RUNFILES_PATCH_FILE = NIX_FILE.parent / "stub-runfiles.patch"
RUSTY_V8_PATCH_FILE = NIX_FILE.parent / "rusty-v8-prebuilt-out-dir.patch"

# Configuration
# Use specific patterns to match only the main package, not ramaBoringssl
CONFIG = lib.UpdateConfig(
    tool_name="codex-cli",
    nix_file=NIX_FILE,
    # Match version after pname = "codex-cli" (with newline between)
    version_pattern=r'(pname\s*=\s*"codex-cli";\n\s*version\s*=\s*")([^"]+)(")',
    # Match hash inside fetchFromGitHub block (with newlines)
    hash_pattern=r'(repo\s*=\s*"codex";\n\s*rev[^;]+;\n\s*hash\s*=\s*")([^"]+)(")',
    cargo_patches=(RUNFILES_PATCH_FILE,),
)

GITHUB_REPO = "openai/codex"
VERSION_PREFIX = "rust-v"  # Codex uses "rust-v" prefix for versions
_RUSTY_V8_GITHUB_REPO = "denoland/rusty_v8"
_LIVEKIT_RUST_SDKS_GITHUB_REPO = "livekit/rust-sdks"

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

_RUSTY_V8_TARGETS = {
    "x86_64-linux": "x86_64-unknown-linux-gnu",
    "aarch64-linux": "aarch64-unknown-linux-gnu",
    "x86_64-darwin": "x86_64-apple-darwin",
    "aarch64-darwin": "aarch64-apple-darwin",
}
_SUPPORTED_SYSTEMS = tuple(_RUSTY_V8_TARGETS)

_RUSTY_V8_VERSION_PATTERN = re.compile(
    r'(^\s*rustyV8Version\s*=\s*")([^"]+)(";\s*$)',
    re.MULTILINE,
)
_LIVEKIT_WEBRTC_TAG_PATTERN = re.compile(
    r'(^\s*livekitWebRtcTag\s*=\s*")([^"]+)(";\s*$)',
    re.MULTILINE,
)
_LIVEKIT_WEBRTC_SOURCE_PATTERN = re.compile(
    r'git\+https://github\.com/(?P<repo>[^?\n"]*?/rust-sdks)\.git\?rev='
    r'(?P<rev>[0-9a-f]{40})#',
)
_LIVEKIT_WEBRTC_TAG_SOURCE_PATTERN = re.compile(
    r'^\s*pub const WEBRTC_TAG: &str = "([^"]+)";\s*$',
    re.MULTILINE,
)
_GITHUB_RELEASE_ASSET_DIGEST_PATTERN = re.compile(
    r"^sha256:(?P<hex>[0-9a-f]{64})$",
)

_LIVEKIT_WEBRTC_TRIPLES = {
    "x86_64-linux": "linux-x64-release",
    "aarch64-linux": "linux-arm64-release",
    "x86_64-darwin": "mac-x64-release",
    "aarch64-darwin": "mac-arm64-release",
}

_RUNFILES_DEP_PATTERN = re.compile(
    r'^(?P<indent>\s*)runfiles\s*=\s*\{[^}]*github\.com/dzbarsky/rules_rust[^}]*\}\s*$',
    re.MULTILINE,
)
_RUNFILES_LOCK_SOURCE_PATTERN = re.compile(
    r'(^\[\[package\]\]\nname = "runfiles"\nversion = "[^"]+"\n)'
    r'source = "git\+https://github\.com/dzbarsky/rules_rust[^"\n]*"\n',
    re.MULTILINE,
)
_RUNFILES_STUB_CARGO_TOML = """[package]
name = "runfiles"
version = "0.1.0"
edition = "2024"
license = "Apache-2.0"

[lib]
path = "src/lib.rs"
"""
_RUNFILES_STUB_LIB_RS = """use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Default)]
pub struct Runfiles;

impl Runfiles {
    pub fn create() -> Result<Self, std::io::Error> {
        Ok(Self)
    }

    pub fn rlocation<P: AsRef<Path>>(&self, path: P) -> Option<PathBuf> {
        Some(path.as_ref().to_path_buf())
    }
}

#[macro_export]
macro_rules! rlocation {
    ($runfiles:expr, $path:expr) => {{
        let _ = &$runfiles;
        Some(std::path::PathBuf::from($path))
    }};
}
"""


def _with_original_newline(content: str, updated: str) -> str:
    if content.endswith("\n") or not updated:
        return updated
    return updated.removesuffix("\n")


def _rewrite_cargo_toml(content: str) -> str:
    updated = _RUNFILES_DEP_PATTERN.sub(
        r'\g<indent>runfiles = { path = "utils/runfiles-stub" }',
        content,
    )
    return _with_original_newline(content, updated)


def _rewrite_cargo_lock(content: str) -> str:
    updated = _RUNFILES_LOCK_SOURCE_PATTERN.sub(r'\1', content)
    return _with_original_newline(content, updated)


def _rewrite_if_changed(path: Path, transform: Callable[[str], str]) -> None:
    content = path.read_text(encoding="utf-8")
    updated = transform(content)
    if updated != content:
        path.write_text(updated, encoding="utf-8")


def _write_runfiles_stub(tree: Path) -> None:
    stub_dir = tree / "utils" / "runfiles-stub"
    src_dir = stub_dir / "src"
    src_dir.mkdir(parents=True, exist_ok=True)
    (stub_dir / "Cargo.toml").write_text(
        _RUNFILES_STUB_CARGO_TOML,
        encoding="utf-8",
    )
    (src_dir / "lib.rs").write_text(_RUNFILES_STUB_LIB_RS, encoding="utf-8")


def _download_text(url: str) -> str:
    request = common.build_request(url)
    with urlopen(request, timeout=120) as response:  # noqa: S310
        return response.read().decode("utf-8")


def _prepare_patch_tree(tree: Path) -> None:
    for cargo_toml in tree.rglob("Cargo.toml"):
        _rewrite_if_changed(cargo_toml, _rewrite_cargo_toml)

    cargo_lock = tree / "Cargo.lock"
    if cargo_lock.exists():
        _rewrite_if_changed(cargo_lock, _rewrite_cargo_lock)

    _write_runfiles_stub(tree)


def _download_release_tarball(version: lib.Version, destination: Path) -> Path:
    tarball_url = lib.get_github_download_url(GITHUB_REPO, version)
    tarball_path = destination / "codex.tar.gz"

    def _fetch_tarball() -> bytes:
        request = common.build_request(tarball_url)
        with urlopen(request, timeout=120) as response:  # noqa: S310
            return response.read()

    tarball_path.write_bytes(
        common.retry_with_backoff(_fetch_tarball, retries=lib.DEFAULT_RETRIES),
    )
    return tarball_path


def _extract_codex_source_tree(tarball_path: Path, destination: Path) -> Path:
    extract_dir = destination / "extract"
    extract_dir.mkdir()

    with tarfile.open(tarball_path, "r:gz") as archive:
        archive.extractall(path=extract_dir, filter="data")

    extracted_roots = [path for path in extract_dir.iterdir() if path.is_dir()]
    if len(extracted_roots) != 1:
        msg = f"Unexpected archive layout for {tarball_path}"
        raise RuntimeError(msg)

    source_dir = extracted_roots[0] / "codex-rs"
    if not source_dir.is_dir():
        msg = f"Could not find codex-rs in extracted archive for {tarball_path}"
        raise RuntimeError(msg)

    return source_dir


def _regenerate_stub_runfiles_patch(version: lib.Version) -> bool:
    with tempfile.TemporaryDirectory(prefix="codex-patch-") as temp_dir:
        temp_root = Path(temp_dir)
        tarball_path = _download_release_tarball(version, temp_root)
        source_dir = _extract_codex_source_tree(tarball_path, temp_root)

        orig_dir = temp_root / "orig"
        mod_dir = temp_root / "mod"
        shutil.copytree(source_dir, orig_dir)
        shutil.copytree(source_dir, mod_dir)
        _prepare_patch_tree(mod_dir)

        result = subprocess.run(
            ["diff", "-urN", "orig", "mod"],
            cwd=temp_root,
            check=False,
            capture_output=True,
            text=True,
        )
        if result.returncode not in {0, 1}:
            msg = result.stderr.strip() or "Failed to generate stub-runfiles.patch"
            raise RuntimeError(msg)

        patch_content = result.stdout
        patch_content = re.sub(
            r'^(--- [^\t\n]+)\t.*$',
            r'\1',
            patch_content,
            flags=re.MULTILINE,
        )
        patch_content = re.sub(
            r'^(\+\+\+ [^\t\n]+)\t.*$',
            r'\1',
            patch_content,
            flags=re.MULTILINE,
        )

    current_patch = (
        RUNFILES_PATCH_FILE.read_text(encoding="utf-8")
        if RUNFILES_PATCH_FILE.exists()
        else None
    )
    if current_patch == patch_content:
        return False

    RUNFILES_PATCH_FILE.write_text(patch_content, encoding="utf-8")
    return True


def _regenerate_rusty_v8_patch(version: lib.Version) -> bool:
    patch_url = (
        "https://raw.githubusercontent.com/openai/codex/"
        f"{version}/patches/rusty_v8_prebuilt_out_dir.patch"
    )
    patch_content = _download_text(patch_url)

    current_patch = (
        RUSTY_V8_PATCH_FILE.read_text(encoding="utf-8")
        if RUSTY_V8_PATCH_FILE.exists()
        else None
    )
    if current_patch == patch_content:
        return False

    RUSTY_V8_PATCH_FILE.write_text(patch_content, encoding="utf-8")
    return True


def _extract_upstream_rusty_v8_version(source_dir: Path) -> str:
    cargo_toml = source_dir / "Cargo.toml"
    match = re.search(
        r'^\s*v8\s*=\s*"=([^"]+)"\s*$',
        cargo_toml.read_text(encoding="utf-8"),
        re.MULTILINE,
    )
    if not match:
        msg = f"Could not find v8 version in {cargo_toml}"
        raise RuntimeError(msg)
    return match.group(1)


def _extract_rusty_v8_version(content: str) -> str | None:
    match = _RUSTY_V8_VERSION_PATTERN.search(content)
    return match.group(2) if match else None


def _extract_livekit_webrtc_tag(content: str) -> str | None:
    match = _LIVEKIT_WEBRTC_TAG_PATTERN.search(content)
    return match.group(2) if match else None


def _sha256_hex_digest_to_sri(hex_digest: str) -> str:
    return "sha256-" + base64.b64encode(bytes.fromhex(hex_digest)).decode("ascii")


def _fetch_github_release_assets(repo: str, tag: str) -> list[dict[str, object]]:
    url = f"{lib.GITHUB_API}/repos/{repo}/releases/tags/{tag}"
    response = common.retry_with_backoff(
        lambda: common.fetch_json(url),
        retries=lib.DEFAULT_RETRIES,
    )
    assets = response.get("assets")
    if not isinstance(assets, list):
        msg = f"GitHub release metadata for {repo}@{tag} did not contain assets"
        raise TypeError(msg)
    return [asset for asset in assets if isinstance(asset, dict)]


def _release_asset_digest_hashes(
    repo: str,
    tag: str,
    asset_names: dict[str, str],
) -> dict[str, str]:
    assets_by_name = {
        asset_name: asset
        for asset in _fetch_github_release_assets(repo, tag)
        if isinstance(asset_name := asset.get("name"), str)
    }

    hashes: dict[str, str] = {}
    for system, asset_name in asset_names.items():
        asset = assets_by_name.get(asset_name)
        if not isinstance(asset, dict):
            continue
        digest = asset.get("digest")
        if not isinstance(digest, str):
            continue
        match = _GITHUB_RELEASE_ASSET_DIGEST_PATTERN.match(digest)
        if not match:
            continue
        hashes[system] = _sha256_hex_digest_to_sri(match.group("hex"))
    return hashes


def _update_rusty_v8_version(content: str, version: str) -> str:
    updated, replacements = _RUSTY_V8_VERSION_PATTERN.subn(
        rf'\1{version}\3',
        content,
        count=1,
    )
    if replacements != 1:
        msg = "Could not find rustyV8Version assignment in codex.nix"
        raise ValueError(msg)
    return updated


def _update_livekit_webrtc_tag(content: str, tag: str) -> str:
    updated, replacements = _LIVEKIT_WEBRTC_TAG_PATTERN.subn(
        rf'\1{tag}\3',
        content,
        count=1,
    )
    if replacements != 1:
        msg = "Could not find livekitWebRtcTag assignment in codex.nix"
        raise ValueError(msg)
    return updated


def _extract_rusty_v8_archive_hashes(content: str) -> dict[str, str]:
    match = re.search(
        r'rustyV8ArchiveHashes\s*=\s*{\n(?P<body>.*?)};',
        content,
        re.DOTALL,
    )
    if not match:
        return {}

    body = match.group("body")
    return {
        entry.group(1): entry.group(2)
        for entry in re.finditer(
            r'^\s*([A-Za-z0-9_-]+)\s*=\s*"([^"]+)";',
            body,
            re.MULTILINE,
        )
    }


def _update_rusty_v8_archive_hashes(
    content: str, hashes: dict[str, str],
) -> str:
    match = re.search(
        r'rustyV8ArchiveHashes\s*=\s*{\n(?P<body>.*?)};',
        content,
        re.DOTALL,
    )
    if not match:
        msg = "Could not find rustyV8ArchiveHashes block in codex.nix"
        raise ValueError(msg)

    body = match.group("body")
    indent_match = re.search(
        r'^(\s*)[A-Za-z0-9_-]+\s*=\s*"[^"]+";',
        body,
        re.MULTILINE,
    )
    entry_indent = indent_match.group(1) if indent_match else "    "
    updated_body = "\n".join(
        f'{entry_indent}{system} = "{hashes[system]}";'
        for system in _RUSTY_V8_TARGETS
    )
    return content[:match.start("body")] + updated_body + content[match.end("body"):]


def _extract_livekit_webrtc_zip_hashes(content: str) -> dict[str, str]:
    match = re.search(
        r'livekitWebRtcZipHashes\s*=\s*{\n(?P<body>.*?)};',
        content,
        re.DOTALL,
    )
    if not match:
        return {}

    body = match.group("body")
    return {
        entry.group(1): entry.group(2)
        for entry in re.finditer(
            r'^\s*([A-Za-z0-9_-]+)\s*=\s*"([^"]+)";',
            body,
            re.MULTILINE,
        )
    }


def _update_livekit_webrtc_zip_hashes(
    content: str, hashes: dict[str, str],
) -> str:
    match = re.search(
        r'livekitWebRtcZipHashes\s*=\s*{\n(?P<body>.*?)};',
        content,
        re.DOTALL,
    )
    if not match:
        msg = "Could not find livekitWebRtcZipHashes block in codex.nix"
        raise ValueError(msg)

    body = match.group("body")
    indent_match = re.search(
        r'^(\s*)[A-Za-z0-9_-]+\s*=\s*"[^"]+";',
        body,
        re.MULTILINE,
    )
    entry_indent = indent_match.group(1) if indent_match else "    "
    updated_body = "\n".join(
        f'{entry_indent}{system} = "{hashes[system]}";'
        for system in _LIVEKIT_WEBRTC_TRIPLES
    )
    return content[:match.start("body")] + updated_body + content[match.end("body"):]


def _rusty_v8_archive_url(version: str, system: str) -> str:
    target = _RUSTY_V8_TARGETS[system]
    return (
        "https://github.com/denoland/rusty_v8/releases/download/"
        f"v{version}/librusty_v8_release_{target}.a.gz"
    )


def _rusty_v8_archive_name(system: str) -> str:
    return f"librusty_v8_release_{_RUSTY_V8_TARGETS[system]}.a.gz"


def _prefetch_file_hash(url: str) -> str:
    result = common.run_command(
        ["nix", "store", "prefetch-file", "--json", url],
        timeout=300,
    )
    try:
        return json.loads(result.stdout)["hash"]
    except (KeyError, json.JSONDecodeError) as exc:
        msg = f"Failed to parse nix prefetch output for {url}"
        raise RuntimeError(msg) from exc


def _calculate_rusty_v8_archive_hashes(version: str) -> dict[str, str]:
    asset_hashes = _release_asset_digest_hashes(
        _RUSTY_V8_GITHUB_REPO,
        f"v{version}",
        {
            system: _rusty_v8_archive_name(system)
            for system in _RUSTY_V8_TARGETS
        },
    )
    return {
        system: asset_hashes.get(system)
        or _prefetch_file_hash(_rusty_v8_archive_url(version, system))
        for system in _RUSTY_V8_TARGETS
    }


def _extract_rust_sdks_source(source_dir: Path) -> tuple[str, str]:
    cargo_lock = source_dir / "Cargo.lock"
    match = _LIVEKIT_WEBRTC_SOURCE_PATTERN.search(
        cargo_lock.read_text(encoding="utf-8"),
    )
    if not match:
        msg = f"Could not find rust-sdks source in {cargo_lock}"
        raise RuntimeError(msg)
    return match.group("repo"), match.group("rev")


def _extract_upstream_livekit_webrtc_tag(repo: str, rev: str) -> str:
    source = _download_text(
        f"https://raw.githubusercontent.com/{repo}/{rev}/webrtc-sys/build/src/lib.rs",
    )
    match = _LIVEKIT_WEBRTC_TAG_SOURCE_PATTERN.search(source)
    if not match:
        msg = f"Could not find WEBRTC_TAG in rust-sdks {repo}@{rev}"
        raise RuntimeError(msg)
    return match.group(1)


def _livekit_webrtc_archive_url(tag: str, system: str) -> str:
    triple = _LIVEKIT_WEBRTC_TRIPLES[system]
    return (
        "https://github.com/livekit/rust-sdks/releases/download/"
        f"{tag}/webrtc-{triple}.zip"
    )


def _livekit_webrtc_archive_name(system: str) -> str:
    return f"webrtc-{_LIVEKIT_WEBRTC_TRIPLES[system]}.zip"


def _calculate_livekit_webrtc_zip_hashes(tag: str) -> dict[str, str]:
    asset_hashes = _release_asset_digest_hashes(
        _LIVEKIT_RUST_SDKS_GITHUB_REPO,
        tag,
        {
            system: _livekit_webrtc_archive_name(system)
            for system in _LIVEKIT_WEBRTC_TRIPLES
        },
    )
    return {
        system: asset_hashes.get(system)
        or _prefetch_file_hash(_livekit_webrtc_archive_url(tag, system))
        for system in _LIVEKIT_WEBRTC_TRIPLES
    }


def _needs_rusty_v8_update(content: str, version: str) -> bool:
    if _extract_rusty_v8_version(content) != version:
        return True
    hashes = _extract_rusty_v8_archive_hashes(content)
    return any(system not in hashes for system in _RUSTY_V8_TARGETS)


def _needs_livekit_webrtc_update(content: str, tag: str) -> bool:
    if _extract_livekit_webrtc_tag(content) != tag:
        return True
    hashes = _extract_livekit_webrtc_zip_hashes(content)
    return any(system not in hashes for system in _LIVEKIT_WEBRTC_TRIPLES)


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


def _extract_cargo_hashes(content: str) -> dict[str, str]:
    match = re.search(
        r'cargoHashes\s*=\s*{\n(?P<body>.*?)};',
        content,
        re.DOTALL,
    )
    if not match:
        return {}

    body = match.group("body")
    return {
        entry.group(1): entry.group(2)
        for entry in re.finditer(
            r'^\s*([A-Za-z0-9_-]+)\s*=\s*"([^"]+)";',
            body,
            re.MULTILINE,
        )
    }


def _extract_cargo_hash(content: str, system: str) -> str | None:
    return _extract_cargo_hashes(content).get(system)


def _update_cargo_hashes(
    content: str,
    new_hash: str,
    *,
    systems: tuple[str, ...] = _SUPPORTED_SYSTEMS,
) -> str:
    match = re.search(
        r'cargoHashes\s*=\s*{\n(?P<body>.*?)};',
        content,
        re.DOTALL,
    )
    if not match:
        msg = "Could not find cargoHashes block in codex.nix"
        raise ValueError(msg)

    body = match.group("body")
    indent_match = re.search(
        r'^(\s*)[A-Za-z0-9_-]+\s*=\s*"[^"]+";',
        body,
        re.MULTILINE,
    )
    if indent_match:
        entry_indent = indent_match.group(1)
    else:
        block_indent_match = re.search(
            r'^(\s*)cargoHashes\s*=\s*{',
            content,
            re.MULTILINE,
        )
        block_indent = block_indent_match.group(1) if block_indent_match else ""
        entry_indent = f"{block_indent}  "

    hashes = _extract_cargo_hashes(content)
    for system in systems:
        hashes[system] = new_hash

    ordered_systems = [
        supported_system
        for supported_system in _SUPPORTED_SYSTEMS
        if supported_system in hashes
    ]
    remaining_systems = [
        supported_system
        for supported_system in hashes
        if supported_system not in _SUPPORTED_SYSTEMS
    ]

    updated_body = "\n".join(
        f'{entry_indent}{supported_system} = "{hashes[supported_system]}";'
        for supported_system in [*ordered_systems, *remaining_systems]
    ) + "\n"

    return content[:match.start("body")] + updated_body + content[match.end("body"):]


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
    hashes = _extract_cargo_hashes(content)
    existing = hashes.get(system)
    has_placeholder = (
        existing is not None
        and "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in existing
    )
    return (
        force
        or version_updated
        or existing is None
        or has_placeholder
    )


def _refresh_release_metadata(
    content: str,
    current_hash: lib.Hash,
    latest_version: lib.Version,
    *,
    version_updated: bool,
) -> tuple[str, lib.Hash, str, str]:
    updated_content = content
    latest_hash = current_hash
    rusty_v8_version = _extract_rusty_v8_version(updated_content)
    livekit_webrtc_tag = _extract_livekit_webrtc_tag(updated_content)

    if version_updated:
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
        _regenerate_stub_runfiles_patch(latest_version)
        _regenerate_rusty_v8_patch(latest_version)
        with tempfile.TemporaryDirectory(prefix="codex-rusty-v8-") as temp_dir:
            temp_root = Path(temp_dir)
            tarball_path = _download_release_tarball(latest_version, temp_root)
            source_dir = _extract_codex_source_tree(tarball_path, temp_root)
            rusty_v8_version = _extract_upstream_rusty_v8_version(source_dir)
            rust_sdks_repo, rust_sdks_rev = _extract_rust_sdks_source(source_dir)
            livekit_webrtc_tag = _extract_upstream_livekit_webrtc_tag(
                rust_sdks_repo,
                rust_sdks_rev,
            )
    elif not RUSTY_V8_PATCH_FILE.exists():
        _regenerate_rusty_v8_patch(latest_version)

    if rusty_v8_version is None or livekit_webrtc_tag is None:
        with tempfile.TemporaryDirectory(prefix="codex-upstream-metadata-") as temp_dir:
            temp_root = Path(temp_dir)
            tarball_path = _download_release_tarball(latest_version, temp_root)
            source_dir = _extract_codex_source_tree(tarball_path, temp_root)
            rusty_v8_version = rusty_v8_version or _extract_upstream_rusty_v8_version(
                source_dir,
            )
            rust_sdks_repo, rust_sdks_rev = _extract_rust_sdks_source(source_dir)
            livekit_webrtc_tag = (
                livekit_webrtc_tag
                or _extract_upstream_livekit_webrtc_tag(
                    rust_sdks_repo,
                    rust_sdks_rev,
                )
            )

    if rusty_v8_version is None or livekit_webrtc_tag is None:
        msg = "Could not determine upstream Codex archive metadata"
        raise RuntimeError(msg)

    return updated_content, latest_hash, rusty_v8_version, livekit_webrtc_tag


def _refresh_rusty_v8_metadata(content: str, rusty_v8_version: str) -> str:
    if not _needs_rusty_v8_update(content, rusty_v8_version):
        return content

    updated_content = _update_rusty_v8_version(content, rusty_v8_version)
    return _update_rusty_v8_archive_hashes(
        updated_content,
        _calculate_rusty_v8_archive_hashes(rusty_v8_version),
    )


def _refresh_livekit_webrtc_metadata(content: str, tag: str) -> str:
    if not _needs_livekit_webrtc_update(content, tag):
        return content

    updated_content = _update_livekit_webrtc_tag(content, tag)
    return _update_livekit_webrtc_zip_hashes(
        updated_content,
        _calculate_livekit_webrtc_zip_hashes(tag),
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

    version_updated = current.version != latest_version
    updated_content, latest_hash, rusty_v8_version, livekit_webrtc_tag = (
        _refresh_release_metadata(
            content,
            current.hash,
            latest_version,
            version_updated=version_updated,
        )
    )
    updated_content = _refresh_rusty_v8_metadata(
        updated_content,
        rusty_v8_version,
    )
    updated_content = _refresh_livekit_webrtc_metadata(
        updated_content,
        livekit_webrtc_tag,
    )

    if _needs_cargo_update(
        updated_content,
        system,
        force=force_cargo,
        version_updated=version_updated,
    ):
        new_cargo_hash = _calculate_cargo_hash(updated_content)
        updated_content = _update_cargo_hashes(
            updated_content,
            str(new_cargo_hash),
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
