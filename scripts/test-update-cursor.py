"""Regression tests for update-cursor helpers."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from types import ModuleType


def _require_equal(actual: object, expected: object, message: str) -> None:
    if actual != expected:
        raise AssertionError(message)


def _require_endswith(value: str, suffix: str, message: str) -> None:
    if not value.endswith(suffix):
        raise AssertionError(message)


def _load_update_cursor() -> ModuleType:
    module_path = Path(__file__).with_name("update-cursor.py")
    scripts_dir = str(module_path.parent)
    if scripts_dir not in sys.path:
        sys.path.insert(0, scripts_dir)

    spec = importlib.util.spec_from_file_location("update_cursor", module_path)
    if spec is None or spec.loader is None:
        msg = f"Could not load {module_path}"
        raise RuntimeError(msg)

    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_zsync_url_is_normalized_to_appimage() -> None:
    update_cursor = _load_update_cursor()
    url = (
        "https://downloads.cursor.com/production/"
        "042b3c1a4c53f2c3808067f519fbfc67b72cad8b/linux/x64/"
        "Cursor-3.9.16-x86_64.AppImage.zsync"
    )

    normalized = update_cursor._normalize_appimage_url(url)

    _require_endswith(
        normalized,
        "Cursor-3.9.16-x86_64.AppImage",
        "zsync URL was not normalized to the AppImage URL",
    )


def test_update_api_payload_uses_product_version_and_appimage_url() -> None:
    update_cursor = _load_update_cursor()
    payload = {
        "version": "3.9.16",
        "productVersion": "3.9.16",
        "url": (
            "https://downloads.cursor.com/production/"
            "042b3c1a4c53f2c3808067f519fbfc67b72cad8b/linux/x64/"
            "Cursor-3.9.16-x86_64.AppImage.zsync"
        ),
    }

    metadata = update_cursor._metadata_from_update_payload(payload)

    _require_equal(
        metadata.version,
        "3.9.16",
        "productVersion was not used as the Cursor version",
    )
    _require_equal(
        metadata.commit_sha,
        "042b3c1a4c53f2c3808067f519fbfc67b72cad8b",
        "commit SHA was not extracted from the immutable download URL",
    )
    _require_endswith(
        metadata.download_url,
        "Cursor-3.9.16-x86_64.AppImage",
        "metadata URL was not normalized to the AppImage URL",
    )


def test_download_page_extracts_latest_linux_x64_url() -> None:
    update_cursor = _load_update_cursor()
    content = (
        '<a href="https://api2.cursor.sh/updates/download/golden/linux-arm64/'
        'cursor/3.9">Linux AppImage (ARM64)</a>'
        '<a href="https://api2.cursor.sh/updates/download/golden/linux-x64/'
        'cursor/3.9">Linux AppImage (x64)</a>'
    )

    url = update_cursor._extract_download_page_url(content)

    _require_equal(
        url,
        "https://api2.cursor.sh/updates/download/golden/linux-x64/cursor/3.9",
        "Linux x64 AppImage URL was not extracted from the download page",
    )


def main() -> None:
    test_zsync_url_is_normalized_to_appimage()
    test_update_api_payload_uses_product_version_and_appimage_url()
    test_download_page_extracts_latest_linux_x64_url()


if __name__ == "__main__":
    main()
