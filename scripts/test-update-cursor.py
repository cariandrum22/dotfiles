"""Regression tests for update-cursor helpers."""

from __future__ import annotations

import importlib.util
import sys
from datetime import UTC, datetime, timedelta
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


def _require(*, condition: bool, message: str) -> None:
    if not condition:
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
        "commit SHA was not extracted from the versioned download URL",
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


def test_current_artifact_is_rehashed_when_no_update_is_available() -> None:
    update_cursor = _load_update_cursor()
    current_url = (
        "https://downloads.cursor.com/production/"
        "042b3c1a4c53f2c3808067f519fbfc67b72cad8b/linux/x64/"
        "Cursor-3.9.16-x86_64.AppImage"
    )
    new_hash = "sha256-republished-artifact"
    prefetched_urls: list[str] = []

    def no_update(_current_version: str) -> None:
        return None

    def prefetch(url: str) -> str:
        prefetched_urls.append(url)
        return new_hash

    update_cursor._fetch_update_api_metadata = no_update
    update_cursor.common.run_nix_prefetch_sri = prefetch

    info = update_cursor.fetch_latest_cursor_info(
        "3.9.16",
        current_url,
        verbose=False,
    )

    _require_equal(
        info.download_hash,
        new_hash,
        "current artifact hash was reused instead of recalculated",
    )
    _require_equal(
        prefetched_urls,
        [current_url],
        "current artifact URL was not prefetched exactly once",
    )


def test_recent_new_artifact_is_deferred_before_prefetch() -> None:
    update_cursor = _load_update_cursor()
    current_url = (
        "https://downloads.cursor.com/production/"
        "1111111111111111111111111111111111111111/linux/x64/"
        "Cursor-3.19.19-x86_64.AppImage"
    )
    new_url = (
        "https://downloads.cursor.com/production/"
        "2222222222222222222222222222222222222222/linux/x64/"
        "Cursor-3.20.10-x86_64.AppImage"
    )
    now = datetime(2026, 9, 11, 3, tzinfo=UTC)
    prefetched_urls: list[str] = []

    def latest(_current_version: str) -> object:
        return update_cursor.CursorMetadata(
            new_url,
            "3.20.10",
            "2222222222222222222222222222222222222222",
        )

    def prefetch(url: str) -> str:
        prefetched_urls.append(url)
        return "sha256-should-not-be-used"

    update_cursor._fetch_update_api_metadata = latest
    update_cursor._fetch_artifact_last_modified = lambda _url: now - timedelta(hours=1)
    update_cursor._utc_now = lambda: now
    update_cursor.common.run_nix_prefetch_sri = prefetch

    deferred = False
    try:
        update_cursor.fetch_latest_cursor_info(
            "3.19.19",
            current_url,
            verbose=False,
        )
    except update_cursor.CursorArtifactTooRecentError:
        deferred = True

    _require(condition=deferred, message="recent Cursor artifact was accepted")
    _require_equal(
        prefetched_urls,
        [],
        "recent Cursor artifact was prefetched before stabilization",
    )


def test_stable_new_artifact_is_prefetched() -> None:
    update_cursor = _load_update_cursor()
    current_url = (
        "https://downloads.cursor.com/production/"
        "1111111111111111111111111111111111111111/linux/x64/"
        "Cursor-3.19.19-x86_64.AppImage"
    )
    new_url = (
        "https://downloads.cursor.com/production/"
        "2222222222222222222222222222222222222222/linux/x64/"
        "Cursor-3.20.10-x86_64.AppImage"
    )
    now = datetime(2026, 9, 12, 3, tzinfo=UTC)

    update_cursor._fetch_update_api_metadata = lambda _version: (
        update_cursor.CursorMetadata(
            new_url,
            "3.20.10",
            "2222222222222222222222222222222222222222",
        )
    )
    update_cursor._fetch_artifact_last_modified = lambda _url: now - timedelta(hours=25)
    update_cursor._utc_now = lambda: now
    update_cursor.common.run_nix_prefetch_sri = lambda _url: "sha256-stable"

    info = update_cursor.fetch_latest_cursor_info(
        "3.19.19",
        current_url,
        verbose=False,
    )

    _require_equal(info.download_hash, "sha256-stable", "stable hash was not used")


def main() -> None:
    test_zsync_url_is_normalized_to_appimage()
    test_update_api_payload_uses_product_version_and_appimage_url()
    test_download_page_extracts_latest_linux_x64_url()
    test_current_artifact_is_rehashed_when_no_update_is_available()
    test_recent_new_artifact_is_deferred_before_prefetch()
    test_stable_new_artifact_is_prefetched()


if __name__ == "__main__":
    main()
