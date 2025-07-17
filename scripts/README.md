# Scripts

This directory contains utility scripts for maintaining the dotfiles repository.

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
```

- Reads extension list from file or installed VSCode
- Queries VS Code marketplace for latest versions
- Generates `extensions.nix` with metadata and SHA256 hashes
- Run automatically monthly via GitHub Actions

### Workflow

1. Add/remove extensions in `config/home-manager/programs/vscode/extensions`
2. Run `./scripts/update-vscode-extensions.py`
3. Commit the updated `extensions.nix`

Or use the `--from-installed` flag to sync with your current VSCode installation.
