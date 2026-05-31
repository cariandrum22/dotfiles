"""Regression tests for update-codex-cli helpers."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from types import ModuleType


def _load_update_codex_cli() -> ModuleType:
    module_path = Path(__file__).with_name("update-codex-cli.py")
    scripts_dir = str(module_path.parent)
    if scripts_dir not in sys.path:
        sys.path.insert(0, scripts_dir)

    spec = importlib.util.spec_from_file_location("update_codex_cli", module_path)
    if spec is None or spec.loader is None:
        msg = f"Could not load {module_path}"
        raise RuntimeError(msg)

    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _require_contains(needle: str, haystack: str, message: str) -> None:
    if needle not in haystack:
        raise AssertionError(message)


def _require_not_contains(needle: str, haystack: str, message: str) -> None:
    if needle in haystack:
        raise AssertionError(message)


def test_numeric_replacement_does_not_form_octal_backreference() -> None:
    update_codex_cli = _load_update_codex_cli()
    content = '\n  rustyV8Version = "146.4.0";\n'

    updated = update_codex_cli._update_rusty_v8_version(content, "147.4.0")

    _require_contains(
        '  rustyV8Version = "147.4.0";',
        updated,
        "rustyV8Version was not updated correctly",
    )
    _require_not_contains(
        "L7.4.0",
        updated,
        "numeric replacement formed an octal escape",
    )


def test_livekit_tag_replacement_preserves_prefix_and_suffix() -> None:
    update_codex_cli = _load_update_codex_cli()
    content = '\n  livekitWebRtcTag = "webrtc-24f6822-2";\n'

    updated = update_codex_cli._update_livekit_webrtc_tag(
        content,
        "webrtc-1234567-3",
    )

    _require_contains(
        '  livekitWebRtcTag = "webrtc-1234567-3";',
        updated,
        "livekitWebRtcTag was not updated correctly",
    )


def test_cargo_hash_replacement_preserves_closing_brace_indent() -> None:
    update_codex_cli = _load_update_codex_cli()
    content = (
        "let\n"
        "  cargoHashes = {\n"
        '    x86_64-linux = "sha256-old";\n'
        '    aarch64-linux = "sha256-old";\n'
        '    x86_64-darwin = "sha256-old";\n'
        '    aarch64-darwin = "sha256-old";\n'
        "  };\n"
        "in\n"
        "{}\n"
    )

    updated = update_codex_cli._update_cargo_hashes(
        content,
        "sha256-new",
    )

    _require_contains(
        "\n  };\n",
        updated,
        "cargoHashes closing brace indentation was not preserved",
    )
    _require_not_contains(
        "\n};\n",
        updated,
        "cargoHashes closing brace was moved to column zero",
    )


def main() -> None:
    test_numeric_replacement_does_not_form_octal_backreference()
    test_livekit_tag_replacement_preserves_prefix_and_suffix()
    test_cargo_hash_replacement_preserves_closing_brace_indent()


if __name__ == "__main__":
    main()
