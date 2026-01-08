# Scripts

This directory contains utility scripts for maintaining the dotfiles repository.

All scripts follow a functional programming style with shared utilities in `common.py` for
consistent error handling, HTTP operations, and file I/O.

## Common Module (common.py)

Shared utilities for all update scripts:

- **HTTP operations**: Consistent headers, User-Agent handling, JSON fetching
- **File I/O**: UTF-8 text reading/writing with proper error handling
- **Subprocess execution**: Wrappers for nix-prefetch-url and other commands
- **Retry logic**: Exponential backoff for network operations
- **Error types**: Custom exception hierarchy for better error reporting
- **Path utilities**: Resolve paths relative to script location

### Environment Variables

- `SCRIPT_USER_AGENT`: Set custom User-Agent header (default: Python-urllib/X.X)

## update-cursor.py

Updates Cursor editor AppImage metadata for Nix.

```bash
# Check for updates and update cursor.nix if needed
./scripts/update-cursor.py
```

- Fetches the latest version from Cursor's API
- Downloads and calculates SHA256 hash for the AppImage
- Updates `config/home-manager/home/packages/cursor.nix`
- Linux x86_64 only (AppImage format)

## update-vscode-insiders.py

Updates VSCode Insiders version metadata and SHA256 hashes.

```bash
# Check for updates and update metadata.nix if needed
./scripts/update-vscode-insiders.py
```

- Fetches the latest commit from Microsoft's VSCode repository
- Downloads and calculates SHA256 hashes for each platform
- Updates `config/home-manager/programs/vscode/metadata.nix`
- Run automatically daily via GitHub Actions

## update-vscode-extensions.py

Updates VSCode extensions Nix expressions from the marketplace.

```bash
# Update from extensions file (default)
./scripts/update-vscode-extensions.py

# Update from currently installed VSCode extensions
./scripts/update-vscode-extensions.py --from-installed

# Specify custom paths
./scripts/update-vscode-extensions.py \
  --file /path/to/extensions \
  --output /path/to/extensions.nix

# Adjust parallelism and retry behavior
./scripts/update-vscode-extensions.py \
  --workers 5 \        # Use 5 parallel workers (default: 3)
  --retries 5          # Retry failed requests 5 times (default: 3)
```

- Reads extension list from file or installed VSCode
- Queries VS Code marketplace for latest versions in parallel
- Generates `extensions.nix` with metadata and SHA256 hashes
- **Automatically formats output with nixfmt** (requires Nix)
- Automatic retry with exponential backoff for failed requests
- Configurable parallelism to avoid overwhelming the marketplace API
- Run automatically daily via GitHub Actions

**Note:** This script requires Nix to be installed for formatting the generated file.

### Workflow

1. Add/remove extensions in `config/home-manager/programs/vscode/extensions`
2. Run `./scripts/update-vscode-extensions.py`
3. Commit the updated `extensions.nix`

Or use the `--from-installed` flag to sync with your current VSCode installation.

## Code Quality

All Python scripts are linted with `ruff` using strict rules. Some exceptions are documented inline
with `noqa` comments:

- `UP047`: Generic type parameters not needed for simple utility functions
- `C901`: Complex functions kept for clarity in sequential update processes
- `TRY300`: Early return patterns preferred over else blocks for readability
- `BLE001`: Catch-all exceptions needed for API error resilience

To check code quality:

```bash
# Check all scripts
ruff check scripts/*.py

# Auto-fix issues where possible
ruff check scripts/*.py --fix
```

## Error Handling

All scripts provide detailed error messages and proper exit codes:

- Exit code 0: Success (updated or already up-to-date)
- Exit code 1: Error occurred

Scripts use a consistent exception hierarchy defined in `common.py` for better error reporting and
debugging.
