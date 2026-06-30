#!/usr/bin/env python3
"""Update Cursor AppImage metadata for Nix expressions.

This script automatically updates the Cursor editor AppImage version and sha256 hash
used by Nix to build the package. It fetches the latest release information from
Cursor's API and updates the local Nix expression.

Usage:
    ./update-cursor.py

    The script will:
    1. Check current version in cursor.nix
    2. Fetch latest version from Cursor's API
    3. If newer, download and calculate sha256 hash
    4. Update cursor.nix with new version and hash

Files modified:
    config/home-manager/home/packages/cursor.nix

Platform:
    Linux x86_64 only (AppImage)

Exit codes:
    0: Success (updated or already up-to-date)
    1: Error occurred

Environment:
    SCRIPT_USER_AGENT: Set custom User-Agent header (default: Python-urllib/X.X)
"""

from __future__ import annotations

import json
import re
import sys
from html import unescape
from typing import TYPE_CHECKING, NamedTuple, override
from urllib.error import HTTPError, URLError
from urllib.parse import urljoin
from urllib.request import HTTPRedirectHandler, build_opener, urlopen

if TYPE_CHECKING:
    from collections.abc import Callable
    from pathlib import Path

import common

# Constants
CURSOR_UPDATE_API_URL_TEMPLATE = (
    "https://api2.cursor.sh/updates/api/update/linux-x64/cursor/{version}/stable"
)
CURSOR_DOWNLOAD_PAGE_URL = "https://cursor.com/download"
RE_VERSION = re.compile(r'version = "([^"]+)"')
RE_DOWNLOAD_URL = re.compile(r'downloadUrl = "([^"]+)"')
RE_HASH = re.compile(r'hash = "([^"]+)"')
RE_VERSION_IN_URL = re.compile(r'[Cc]ursor-(\d+\.\d+\.\d+)')
RE_COMMIT_IN_URL = re.compile(r"/production/([0-9a-f]{40})/")
RE_DOWNLOAD_PAGE_URL = re.compile(
    r"https:(?:\\?/){2}api2\.cursor\.sh(?:\\?/)updates(?:\\?/)download"
    r"(?:\\?/)golden(?:\\?/)linux-x64(?:\\?/)cursor(?:\\?/)[^\\\"<]+",
)
RETRY_ATTEMPTS = 3
HTTP_NO_CONTENT = 204
HTTP_REDIRECT_MIN = 300
HTTP_REDIRECT_MAX = 400


class CursorConfigError(common.ConfigError):
    """Error reading cursor.nix configuration."""

    def __init__(self, field: str) -> None:
        """Initialize with field name."""
        super().__init__(f"Could not find {field} in cursor.nix")


class CursorVersionError(common.UpdateScriptError):
    """Error extracting version from URL."""

    def __init__(self, url: str) -> None:
        """Initialize with URL."""
        super().__init__(f"Could not extract version from URL: {url}")


class CursorApiError(common.UpdateScriptError):
    """Error validating Cursor API metadata."""

    def __init__(self, message: str) -> None:
        """Initialize with message."""
        super().__init__(message)


class CursorInfo(NamedTuple):
    """Cursor metadata."""

    download_url: str
    download_hash: str
    version: str
    commit_sha: str


class CursorMetadata(NamedTuple):
    """Cursor metadata before Nix hash calculation."""

    download_url: str
    version: str
    commit_sha: str


class _NoRedirectHandler(HTTPRedirectHandler):
    """Return redirect responses without following large AppImage downloads."""

    @override
    def redirect_request(
        self,
        req: object,
        fp: object,
        code: int,
        msg: str,
        headers: object,
        newurl: str,
    ) -> None:
        """Disable urllib's automatic redirect handling."""
        del req, fp, code, msg, headers, newurl


# ----- Pure helpers ----------------------------------------------------------------


def _extract_field(content: str, regex: re.Pattern[str], field_name: str) -> str:
    """Extract field from Nix file content using regex."""
    if (match := regex.search(content)) is None:
        raise CursorConfigError(field_name)
    return match.group(1)


def _parse_cursor_info(content: str) -> tuple[str, str, str]:
    """Parse version, URL and hash from cursor.nix content."""
    version = _extract_field(content, RE_VERSION, "version")
    url = _extract_field(content, RE_DOWNLOAD_URL, "downloadUrl")
    hash_value = _extract_field(content, RE_HASH, "hash")
    return version, url, hash_value


def _extract_version_from_url(url: str) -> str:
    """Extract version number from download URL."""
    if (match := RE_VERSION_IN_URL.search(url)) is None:
        raise CursorVersionError(url)
    return match.group(1)


def _extract_commit_from_url(url: str) -> str:
    """Extract commit SHA from download URL."""
    if (match := RE_COMMIT_IN_URL.search(url)) is None:
        msg = f"Could not extract commit SHA from URL: {url}"
        raise CursorApiError(msg)
    return match.group(1)


def _normalize_appimage_url(url: str) -> str:
    """Normalize Cursor download URLs to the AppImage artifact."""
    return url.removesuffix(".zsync")


def _cursor_update_api_url(current_version: str) -> str:
    """Build Cursor update API URL for the currently packaged version."""
    return CURSOR_UPDATE_API_URL_TEMPLATE.format(version=current_version)


def _extract_download_page_url(content: str) -> str:
    """Extract the latest Linux x64 AppImage redirect URL from Cursor's page."""
    if (match := RE_DOWNLOAD_PAGE_URL.search(content)) is None:
        msg = "Could not find Linux x64 AppImage download URL on Cursor download page"
        raise CursorApiError(msg)
    return unescape(match.group(0).replace("\\/", "/"))


def _extract_api_string(
    payload: dict[str, object], field_name: str, *, required: bool = True,
) -> str | None:
    """Extract a string field from the Cursor API payload."""
    value = payload.get(field_name)
    if value is None:
        if required:
            msg = f"Cursor API response is missing '{field_name}'"
            raise CursorApiError(msg)
        return None
    if not isinstance(value, str) or not value:
        msg = f"Cursor API field '{field_name}' must be a non-empty string"
        raise CursorApiError(msg)
    return value


def _validate_download_url(download_url: str, version: str, commit_sha: str) -> None:
    """Validate that the download URL matches the reported Cursor metadata."""
    url_version = _extract_version_from_url(download_url)
    if url_version != version:
        msg = f"Cursor API version mismatch: response={version}, url={url_version}"
        raise CursorApiError(msg)

    url_commit = _extract_commit_from_url(download_url)
    if url_commit != commit_sha:
        msg = f"Cursor API commit mismatch: response={commit_sha}, url={url_commit}"
        raise CursorApiError(msg)


def _metadata_from_update_payload(payload: dict[str, object]) -> CursorMetadata:
    """Build Cursor metadata from the update API payload."""
    raw_download_url = _extract_api_string(payload, "url")
    if raw_download_url is None:
        msg = "Cursor update API response is missing 'url'"
        raise CursorApiError(msg)

    download_url = _normalize_appimage_url(raw_download_url)
    version = _extract_api_string(payload, "productVersion", required=False)
    if version is None:
        version = _extract_api_string(payload, "version", required=False)
    if version is None:
        version = _extract_version_from_url(download_url)

    commit_sha = _extract_commit_from_url(download_url)
    _validate_download_url(download_url, version, commit_sha)
    return CursorMetadata(download_url, version, commit_sha)


def _generate_nix_content(
    content: str, version: str, download_url: str, download_hash: str,
) -> str:
    """Update Nix file content with new values."""
    # Update version
    content = RE_VERSION.sub(f'version = "{version}"', content, count=1)
    # Update download URL
    content = RE_DOWNLOAD_URL.sub(f'downloadUrl = "{download_url}"', content, count=1)
    # Update hash
    return RE_HASH.sub(f'hash = "{download_hash}"', content, count=1)


# ----- API interaction -------------------------------------------------------------


def _fetch_optional_json(url: str) -> dict[str, object] | None:
    """Fetch JSON, returning None when Cursor reports no update with an empty body."""
    req = common.build_request(url, headers=common.JSON_HEADERS)
    try:
        with urlopen(req, timeout=common.HTTP_TIMEOUT) as resp:  # noqa: S310
            body = resp.read().decode("utf-8").strip()
            if resp.status == HTTP_NO_CONTENT or not body:
                return None
            data = json.loads(body)
    except (HTTPError, URLError, json.JSONDecodeError) as exc:
        raise common.FetchError(url, exc) from exc

    if not isinstance(data, dict):
        msg = f"Cursor update API response must be a JSON object: {url}"
        raise CursorApiError(msg)
    return data


def _resolve_cursor_download_url(url: str) -> str:
    """Resolve Cursor's lightweight download URL to the immutable AppImage URL."""
    normalized_url = _normalize_appimage_url(url)
    if "://api2.cursor.sh/updates/download/" not in normalized_url:
        return normalized_url

    opener = build_opener(_NoRedirectHandler)
    req = common.build_request(normalized_url)
    try:
        with opener.open(req, timeout=common.HTTP_TIMEOUT) as resp:
            return _normalize_appimage_url(resp.geturl())
    except HTTPError as exc:
        if HTTP_REDIRECT_MIN <= exc.code < HTTP_REDIRECT_MAX:
            location = exc.headers.get("Location")
            if location:
                return _normalize_appimage_url(urljoin(normalized_url, location))
        raise common.FetchError(normalized_url, exc) from exc
    except URLError as exc:
        raise common.FetchError(normalized_url, exc) from exc


def _fetch_update_api_metadata(current_version: str) -> CursorMetadata | None:
    """Fetch Cursor metadata from the current update API."""
    payload = _fetch_optional_json(_cursor_update_api_url(current_version))
    if payload is None:
        return None
    return _metadata_from_update_payload(payload)


def _fetch_download_page_metadata() -> CursorMetadata:
    """Fetch Cursor metadata from the public download page as a fallback."""
    page_content = common.fetch_text(CURSOR_DOWNLOAD_PAGE_URL)
    redirect_url = _extract_download_page_url(page_content)
    download_url = _resolve_cursor_download_url(redirect_url)
    version = _extract_version_from_url(download_url)
    commit_sha = _extract_commit_from_url(download_url)
    _validate_download_url(download_url, version, commit_sha)
    return CursorMetadata(download_url, version, commit_sha)


def _retry_logger(
    action: str, *, verbose: bool,
) -> Callable[[int, Exception], None] | None:
    """Build a retry callback for common.retry_with_backoff."""
    if not verbose:
        return None

    def log_retry(retry_number: int, exc: Exception) -> None:
        next_attempt = retry_number + 1
        print(
            f"  Retrying {action} ({next_attempt}/{RETRY_ATTEMPTS}) after error: {exc}",
        )

    return log_retry


def fetch_latest_cursor_info(
    current_version: str,
    current_url: str,
    current_hash: str,
    *,
    verbose: bool = True,
) -> CursorInfo:
    """Fetch latest Cursor information from API."""
    update_api_url = _cursor_update_api_url(current_version)
    if verbose:
        print(f"Fetching Cursor update API response from {update_api_url}...")

    try:
        metadata = common.retry_with_backoff(
            lambda: _fetch_update_api_metadata(current_version),
            retries=RETRY_ATTEMPTS,
            exceptions=(common.FetchError,),
            on_retry=_retry_logger("Cursor update API metadata", verbose=verbose),
        )
    except common.FetchError as exc:
        if verbose:
            print(f"  Cursor update API failed: {exc}")
            print(f"  Falling back to {CURSOR_DOWNLOAD_PAGE_URL}...")
        metadata = common.retry_with_backoff(
            _fetch_download_page_metadata,
            retries=RETRY_ATTEMPTS,
            exceptions=(common.FetchError,),
            on_retry=_retry_logger("Cursor download page metadata", verbose=verbose),
        )

    if metadata is None:
        commit_sha = _extract_commit_from_url(current_url)
        if verbose:
            print("  Cursor update API reports no newer version")
        return CursorInfo(current_url, current_hash, current_version, commit_sha)

    if verbose:
        print(f"  Version: {metadata.version}")
        print(f"  Commit: {metadata.commit_sha}")
        print(f"  Download URL: {metadata.download_url}")
        print("Fetching download hash...")

    download_hash = common.retry_with_backoff(
        lambda: common.run_nix_prefetch_sri(metadata.download_url),
        retries=RETRY_ATTEMPTS,
        exceptions=(common.SubprocessError, common.PrefetchError),
        on_retry=_retry_logger("Cursor download hash", verbose=verbose),
    )

    if verbose:
        print(f"  Download hash: {download_hash}")

    return CursorInfo(
        metadata.download_url,
        download_hash,
        metadata.version,
        metadata.commit_sha,
    )


# ----- File operations -------------------------------------------------------------


def _cursor_nix_path() -> Path:
    """Get path to cursor.nix relative to this script."""
    return common.resolve_script_relative(
        "..", "config", "home-manager", "home", "packages", "cursor.nix",
    )


def update_cursor_nix(cursor_file: Path, info: CursorInfo) -> bool:
    """Update cursor.nix with new metadata.

    Returns:
        True if file was updated, False if content unchanged
    """
    content = common.read_text(cursor_file)
    original_content = content

    # Generate updated content
    new_content = _generate_nix_content(
        content, info.version, info.download_url, info.download_hash,
    )

    if new_content == original_content:
        return False

    common.write_text(cursor_file, new_content)
    return True


# ----- Main logic ------------------------------------------------------------------


def update_cursor(*, verbose: bool = True) -> bool:  # noqa: C901 - Clear sequential steps for update process
    """Update Cursor metadata if newer version available.

    Args:
        verbose: Whether to print progress messages

    Returns:
        True if updated, False if already up-to-date

    Raises:
        Various exceptions on errors
    """
    cursor_file = _cursor_nix_path()

    if verbose:
        print("Checking for Cursor updates...")

    # Get current info
    current_content = common.read_text(cursor_file)
    current_version, current_url, current_hash = _parse_cursor_info(current_content)

    if verbose:
        print(f"Current version: {current_version}")
        print(f"Current URL: {current_url}")
        print(f"Current hash: {current_hash}")

    # Get latest info
    if verbose:
        print("\nFetching latest Cursor information...")
    latest_info = fetch_latest_cursor_info(
        current_version,
        current_url,
        current_hash,
        verbose=verbose,
    )

    # Check if update is needed
    if (
        current_version == latest_info.version
        and current_url == latest_info.download_url
        and current_hash == latest_info.download_hash
    ):
        if verbose:
            print("\nAlready up to date")
        return False

    # Update file
    if verbose:
        print(f"\nUpdating {cursor_file}...")

    if update_cursor_nix(cursor_file, latest_info):
        if verbose:
            print("\n✅ Successfully updated Cursor")
            print(f"  Version: {current_version} → {latest_info.version}")
            print(f"  Commit: {latest_info.commit_sha}")
            print(f"  URL: {current_url} → {latest_info.download_url}")
            print(f"  Hash: {current_hash} → {latest_info.download_hash}")
        return True

    if verbose:
        print("\n❌ Failed to update cursor.nix")
    return False


def main() -> None:
    """Main entry point."""
    try:
        update_cursor(verbose=True)
        sys.exit(0)
    except (
        CursorConfigError,
        CursorVersionError,
        CursorApiError,
        common.UpdateScriptError,
        FileNotFoundError,
    ) as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
