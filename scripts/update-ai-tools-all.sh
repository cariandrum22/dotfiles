#!/usr/bin/env bash
# Update all AI tools

set -euo pipefail

echo "Updating AI tools..."
echo

python3 scripts/update-claude-code.py
echo
python3 scripts/update-codex-cli.py
echo
python3 scripts/update-droid.py
echo
python3 scripts/update-gemini-cli.py

echo
echo "All AI tools updated!"
