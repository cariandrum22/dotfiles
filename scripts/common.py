#!/usr/bin/env python3
"""Common utilities for dotfiles update scripts.

This module provides shared functionality for various update scripts including:
- HTTP client utilities with consistent headers and error handling
- File I/O helpers with UTF-8 encoding
- Subprocess execution wrappers
- Retry logic with exponential backoff
- Common error types
"""

from __future__ import annotations

import inspect
import json
import os
import subprocess  # noqa: S404 - Required for subprocess commands
import time
from pathlib import Path
from typing import TYPE_CHECKING, Any, TypeVar
from urllib.error import HTTPError, URLError
from urllib.request import Request, urlopen

if TYPE_CHECKING:
    from collections.abc import Callable, Mapping

# Type variables for generic functions
T = TypeVar("T")

# ----- Constants -------------------------------------------------------------------

# User-Agent configuration
# By default, we don't set any User-Agent and let urllib use its default
# (Python-urllib/X.X). Can be overridden with SCRIPT_USER_AGENT environment variable
DEFAULT_USER_AGENT = os.environ.get("SCRIPT_USER_AGENT", None)

# Standard headers for JSON APIs
_base_json_headers = {
    "Accept": "application/json",
    "Content-Type": "application/json",
}
if DEFAULT_USER_AGENT:
    _base_json_headers["User-Agent"] = DEFAULT_USER_AGENT
JSON_HEADERS: Mapping[str, str] = _base_json_headers

# Default timeout for HTTP requests (seconds)
HTTP_TIMEOUT = 30

# Default timeout for subprocess commands (seconds)
SUBPROCESS_TIMEOUT = 60


# ----- Base Errors -----------------------------------------------------------------


class UpdateScriptError(Exception):
    """Base error for all update script operations."""


class FetchError(UpdateScriptError):
    """Error fetching data from remote sources."""

    def __init__(self, url: str, error: Exception) -> None:
        """Initialize with URL and underlying error."""
        super().__init__(f"Failed to fetch {url}: {error}")
        self.url = url
        self.error = error


class ConfigError(UpdateScriptError):
    """Error reading or parsing configuration files."""


class SubprocessError(UpdateScriptError):
    """Error executing subprocess commands."""

    def __init__(self, cmd: str, error: subprocess.CalledProcessError) -> None:
        """Initialize with command and subprocess error."""
        super().__init__(f"Command '{cmd}' failed: {error.stderr or error}")
        self.cmd = cmd
        self.error = error


# ----- HTTP Utilities --------------------------------------------------------------


def build_request(
    url: str,
    *,
    headers: Mapping[str, str] | None = None,
    user_agent: str | None = DEFAULT_USER_AGENT,
) -> Request:
    """Build urllib Request with consistent headers.

    Args:
        url: Target URL
        headers: Optional headers to merge with defaults
        user_agent: User-Agent string (None uses urllib default)

    Returns:
        Configured Request object
    """
    default_headers = {}
    if user_agent:
        default_headers["User-Agent"] = user_agent
    if headers:
        default_headers.update(headers)
    return Request(url, headers=default_headers or None)  # noqa: S310 - Trusted HTTPS URLs only


def fetch_text(url: str, *, timeout: int = HTTP_TIMEOUT) -> str:
    """Fetch text content from URL.

    Args:
        url: URL to fetch
        timeout: Request timeout in seconds

    Returns:
        Decoded text content

    Raises:
        FetchError: On HTTP errors or network issues
    """
    req = build_request(url)
    try:
        with urlopen(req, timeout=timeout) as resp:  # noqa: S310 - Trusted HTTPS only
            return resp.read().decode("utf-8")
    except (HTTPError, URLError) as exc:
        raise FetchError(url, exc) from exc


def fetch_json(url: str, *, timeout: int = HTTP_TIMEOUT) -> dict[str, Any]:
    """Fetch and parse JSON from URL.

    Args:
        url: URL to fetch
        timeout: Request timeout in seconds

    Returns:
        Parsed JSON data

    Raises:
        FetchError: On HTTP errors, network issues, or invalid JSON
    """
    req = build_request(url, headers=JSON_HEADERS)
    try:
        with urlopen(req, timeout=timeout) as resp:  # noqa: S310 - Trusted HTTPS only
            return json.loads(resp.read().decode("utf-8"))
    except (HTTPError, URLError) as exc:
        raise FetchError(url, exc) from exc
    except json.JSONDecodeError as exc:
        raise FetchError(url, exc) from exc


# ----- File I/O Utilities ----------------------------------------------------------


def read_text(path: Path) -> str:
    """Read text file with UTF-8 encoding.

    Args:
        path: File path

    Returns:
        File contents

    Raises:
        FileNotFoundError: If file doesn't exist
        OSError: On other I/O errors
    """
    return path.read_text(encoding="utf-8")


def write_text(path: Path, content: str) -> None:
    """Write text file with UTF-8 encoding.

    Args:
        path: File path
        content: Text content to write

    Raises:
        OSError: On I/O errors
    """
    path.write_text(content, encoding="utf-8")


def read_lines(path: Path, *, strip: bool = True) -> list[str]:
    """Read file as list of lines.

    Args:
        path: File path
        strip: Whether to strip whitespace from lines

    Returns:
        List of lines
    """
    lines = read_text(path).splitlines()
    return [ln.strip() for ln in lines if ln.strip()] if strip else lines


# ----- Subprocess Utilities --------------------------------------------------------


def run_command(
    cmd: list[str],
    *,
    timeout: int = SUBPROCESS_TIMEOUT,
    check: bool = True,
    capture_output: bool = True,
) -> subprocess.CompletedProcess[str]:
    """Run subprocess command with consistent settings.

    Args:
        cmd: Command and arguments
        timeout: Command timeout in seconds
        check: Whether to raise on non-zero exit
        capture_output: Whether to capture stdout/stderr

    Returns:
        CompletedProcess result

    Raises:
        SubprocessError: On command failure (if check=True)
        subprocess.TimeoutExpired: On timeout
    """
    try:
        return subprocess.run(
            cmd,
            check=check,
            capture_output=capture_output,
            text=True,
            timeout=timeout,
        )
    except subprocess.CalledProcessError as exc:
        raise SubprocessError(" ".join(cmd), exc) from exc


def run_nix_prefetch(url: str, *, timeout: int = SUBPROCESS_TIMEOUT) -> str:
    """Run nix-prefetch-url and return sha256 hash.

    Args:
        url: URL to prefetch
        timeout: Command timeout in seconds

    Returns:
        SHA256 hash string

    Raises:
        SubprocessError: On command failure
        subprocess.TimeoutExpired: On timeout
    """
    result = run_command(["nix-prefetch-url", url], timeout=timeout)
    return result.stdout.strip()


# ----- Retry Logic -----------------------------------------------------------------


def exponential_backoff(
    base: float = 1.0, factor: float = 2.0, max_delay: float = 60.0,
) -> Callable[[int], float]:
    """Create exponential backoff delay function.

    Args:
        base: Base delay in seconds
        factor: Multiplication factor for each retry
        max_delay: Maximum delay cap

    Returns:
        Function that computes delay for given attempt number
    """
    def compute_delay(attempt: int) -> float:
        return min(base * (factor ** attempt), max_delay)
    return compute_delay


def retry_with_backoff(  # noqa: UP047 - Generic type parameters not needed for simple utility function
    func: Callable[[], T],
    *,
    retries: int = 3,
    delay_func: Callable[[int], float] | None = None,
    exceptions: tuple[type[Exception], ...] = (Exception,),
    on_retry: Callable[[int, Exception], None] | None = None,
) -> T:
    """Execute function with retry logic and exponential backoff.

    Args:
        func: Function to execute
        retries: Maximum retry attempts
        delay_func: Function to compute delay for each attempt
        exceptions: Exception types to catch and retry
        on_retry: Optional callback for retry notifications

    Returns:
        Function result

    Raises:
        Last exception if all retries fail
    """
    if delay_func is None:
        delay_func = exponential_backoff()

    last_exc: Exception | None = None

    for attempt in range(retries):
        try:
            return func()
        except exceptions as exc:
            last_exc = exc
            if attempt < retries - 1:
                if on_retry:
                    on_retry(attempt + 1, exc)
                delay = delay_func(attempt)
                time.sleep(delay)
            else:
                break

    if last_exc:
        raise last_exc
    msg = "Retry logic error: no exception captured"
    raise RuntimeError(msg)


# ----- Path Utilities --------------------------------------------------------------


def resolve_script_relative(*parts: str) -> Path:
    """Resolve path relative to the calling script.

    Args:
        parts: Path components relative to script directory

    Returns:
        Resolved absolute path
    """
    # Get the calling script's path
    frame = inspect.currentframe()
    if frame and frame.f_back:
        caller_file = frame.f_back.f_globals.get("__file__")
        if caller_file:
            script_dir = Path(caller_file).parent
            return script_dir.joinpath(*parts).resolve()

    # Fallback to current directory
    return Path(*parts).resolve()
