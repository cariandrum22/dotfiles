#!/usr/bin/env python3
"""Update VSCode extensions from marketplace.

This script queries the Visual Studio Code marketplace for extensions
and generates a Nix expression with their metadata and sha256 hashes.

Usage:
    # Update from extensions file:
    ./update-vscode-extensions.py

    # Update from currently installed extensions:
    ./update-vscode-extensions.py --from-installed

    # Update from specific file:
    ./update-vscode-extensions.py --file /path/to/extensions
"""

import argparse
import http.client
import json
import platform
import subprocess
import sys
from pathlib import Path
from typing import Any, TypedDict

MARKETPLACE_DOMAIN = "marketplace.visualstudio.com"
ASSETS_DOMAIN = "{publisher}.gallery.vsassets.io"
EXTENSION_QUERY_ENDPOINT = "/_apis/public/gallery/extensionquery"
DOWNLOAD_ENDPOINT = (
    "/_apis/public/gallery/publisher/{publisher}"
    "/extension/{extension}/{version}"
    "/assetbyname/Microsoft.VisualStudio.Services.VSIXPackage{queryString}"
)


class Extension(TypedDict):
    """Extension dictionary structure."""

    publisher: str
    name: str


class QueryResult(TypedDict):
    """Query result structure."""

    status: int
    reason: str
    body: dict[str, Any]


def get_installed_extensions() -> list[str]:
    """Get list of installed extensions from VSCode.

    Returns:
        List of extension identifiers in publisher.name format

    Raises:
        RuntimeError: If VSCode command fails
    """
    try:
        # Try code-insiders first, then regular code
        for cmd in ["code-insiders", "code"]:
            try:
                result = subprocess.run(
                    [cmd, "--list-extensions"],
                    capture_output=True,
                    text=True,
                    check=True,
                )
                extensions = [
                    line.strip() for line in result.stdout.splitlines() if line.strip()
                ]
                print(f"Found {len(extensions)} installed extensions via {cmd}")
                return extensions
            except (subprocess.CalledProcessError, FileNotFoundError):
                continue

        msg = "Could not find VSCode command (tried code-insiders and code)"
        raise RuntimeError(msg)
    except Exception as e:
        raise RuntimeError(f"Failed to get installed extensions: {e}") from e


def read_extensions_file(file_path: Path) -> list[str]:
    """Read extension list from file.

    Args:
        file_path: Path to the extensions file

    Returns:
        List of extension identifiers in publisher.name format
    """
    with file_path.open() as f:
        return [line.strip() for line in f if line.strip()]


def to_dict(item: str) -> Extension:
    """Convert extension string to dictionary.

    Args:
        item: Extension identifier in publisher.name format

    Returns:
        Dictionary with publisher and name keys
    """
    publisher, name = item.split(".")
    return {"publisher": publisher, "name": name}


def parse_extension_list(extensions: list[str]) -> list[Extension]:
    """Parse list of extension strings into dictionaries.

    Args:
        extensions: List of extension identifiers

    Returns:
        List of extension dictionaries
    """
    return [to_dict(ext) for ext in extensions]


def query(extension: Extension) -> QueryResult:
    """Query marketplace for extension information.

    Args:
        extension: Extension dictionary with publisher and name

    Returns:
        Query result with status, reason, and body
    """
    connection = http.client.HTTPSConnection(MARKETPLACE_DOMAIN)
    headers = {
        "Content-Type": "application/json",
        "Accept": "application/json;api-version=3.0-preview.1",
    }
    data = {
        "filters": [
            {
                "criteria": [
                    {"filterType": 8, "value": "Microsoft.VisualStudio.Code"},
                    {
                        "filterType": 7,
                        "value": f"{extension['publisher']}.{extension['name']}",
                    },
                ],
                "pageNumber": 1,
                "pageSize": 100,
                "sortBy": 0,
                "sortOrder": 0,
            }
        ],
        "assetTypes": [],
        "flags": 0x200,  # Include latest version only
    }
    connection.request("POST", EXTENSION_QUERY_ENDPOINT, json.dumps(data), headers)
    response = connection.getresponse()
    result = {
        "status": response.status,
        "reason": response.reason,
        "body": json.loads(response.read()),
    }
    connection.close()
    return result


def results_to_nix_attr(
    publisher: str,
    name: str,
    version: str,
    sha256: str,
    arch: str | None = None,
) -> str:
    """Convert extension metadata to Nix attribute.

    Args:
        publisher: Extension publisher
        name: Extension name
        version: Extension version
        sha256: Package sha256 hash
        arch: Optional architecture string

    Returns:
        Formatted Nix attribute string
    """
    arch_line = f'arch = "{arch}";' if arch is not None else ""
    return f"""
  {{
    name = "{name}";
    publisher = "{publisher}";
    version = "{version}";
    sha256 = "{sha256}";
    {arch_line}
  }}"""


def convert_system_arch_representation() -> str:
    """Convert system architecture to VSCode format.

    Returns:
        Architecture string in VSCode format (e.g., linux-x64)

    Raises:
        ValueError: If architecture is unsupported
    """
    system = platform.system().lower()
    machine = platform.machine()

    arch_map = {
        "x86_64": "x64",
        "i686": "x86",
        "aarch64": "arm64",
        "arm64": "arm64",
        "armv7l": "armhf",
    }

    arch_suffix = arch_map.get(machine)
    if not arch_suffix:
        msg = f"Unsupported architecture: {machine}"
        raise ValueError(msg)

    return f"{system}-{arch_suffix}"


def extract_version_and_platform(
    versions: list[dict[str, Any]],
) -> tuple[str, str | None]:
    """Extract version and platform from version list.

    Args:
        versions: List of version dictionaries from marketplace

    Returns:
        Tuple of (version, platform) where platform may be None

    Raises:
        ValueError: If no suitable version is found
    """
    target_platform = convert_system_arch_representation()

    # First try to find platform-specific version
    for version in versions:
        if version.get("targetPlatform") == target_platform:
            return version["version"], version["targetPlatform"]

    # Fall back to universal version
    for version in versions:
        if "targetPlatform" not in version:
            return version["version"], None

    msg = "No suitable version found"
    raise ValueError(msg)


def extract_extension_info(result: QueryResult) -> str:
    """Extract extension info and fetch sha256.

    Args:
        result: Query result from marketplace

    Returns:
        Nix attribute string for the extension
    """
    extension = result["body"]["results"][0]["extensions"][0]
    publisher = extension["publisher"]["publisherName"]
    name = extension["extensionName"]
    version, arch = extract_version_and_platform(extension["versions"])
    query_string = f"?targetPlatform={arch}" if arch else ""

    sha256 = (
        subprocess.run(
            [
                "nix-prefetch-url",
                "https://"
                + ASSETS_DOMAIN.format(publisher=publisher)
                + DOWNLOAD_ENDPOINT.format(
                    publisher=publisher,
                    extension=name,
                    version=version,
                    queryString=query_string,
                ),
            ],
            check=True,
            capture_output=True,
            encoding="utf-8",
        )
    ).stdout.strip()
    return results_to_nix_attr(publisher, name, version, sha256, arch)


def parse_args() -> argparse.Namespace:
    """Parse command line arguments.

    Returns:
        Parsed arguments
    """
    parser = argparse.ArgumentParser(
        description="Update VSCode extensions Nix expressions"
    )
    parser.add_argument(
        "--from-installed",
        action="store_true",
        help="Get extensions from installed VSCode instead of file",
    )
    parser.add_argument(
        "--file",
        type=Path,
        help=(
            "Path to extensions file "
            "(default: ../config/home-manager/programs/vscode/extensions)"
        ),
    )
    parser.add_argument(
        "--output",
        type=Path,
        help=(
            "Output path for extensions.nix "
            "(default: ../config/home-manager/programs/vscode/extensions.nix)"
        ),
    )
    return parser.parse_args()


def process_extensions(extensions: list[Extension]) -> list[QueryResult]:
    """Query marketplace for all extensions.

    Args:
        extensions: List of extensions to query

    Returns:
        List of successful query results
    """
    results = []
    for ext in extensions:
        print(f"Querying {ext['publisher']}.{ext['name']}...")
        try:
            result = query(ext)
            results.append(result)
        except Exception as e:
            print(f"  Failed: {e}")
            continue
    return results


def main() -> int:
    """Update VSCode extensions.

    Returns:
        Exit code (0 for success)
    """
    args = parse_args()

    # Determine paths
    script_dir = Path(__file__).parent
    extensions_file = (
        args.file
        if args.file
        else script_dir.parent / "config/home-manager/programs/vscode/extensions"
    )
    output_file = (
        args.output
        if args.output
        else script_dir.parent / "config/home-manager/programs/vscode/extensions.nix"
    )

    print("Updating VSCode extensions...")

    # Get extensions list
    if args.from_installed:
        extensions_list = get_installed_extensions()
    else:
        if not extensions_file.exists():
            print(f"Error: Extensions file not found: {extensions_file}")
            return 1
        extensions_list = read_extensions_file(extensions_file)

    # Query marketplace and generate output
    extensions = parse_extension_list(extensions_list)
    results = process_extensions(extensions)

    # Generate Nix expression
    extensions_nix = "".join([extract_extension_info(result) for result in results])
    with output_file.open("w") as file:
        file.write("[")
        file.write(extensions_nix)
        file.write("\n]\n")

    print(f"\nSuccessfully updated {output_file}")
    print(f"Total extensions: {len(results)}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
