---
name: setup-secrets-scan
description: Set up secret detection hooks (gitleaks, ripsecrets, detect-aws-credentials, detect-private-keys) in a Nix flake project. Use when adding secret leak prevention.
disable-model-invocation: true
argument-hint: (no arguments required)
---

# Setup Secrets Scanning

Add secret detection hooks to the project's `flake.nix`.

## Prerequisites

- The project must have `git-hooks.nix` integrated in `flake.nix`. If not, tell the user to run `/setup-git-hooks` first.

## Steps

### 1. Add secret scanning hooks to `flake.nix`

Add the hooks from [hooks.nix.template](hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate hooks that already exist — if a hook already exists, skip it.

Hooks added:

- **gitleaks** — Scans staged changes for secrets (`protect --staged --redact`)
- **gitleaks-full-scan** — Scans the entire working tree for secrets (`detect --source . --no-git --redact`)
- **ripsecrets** — Fast secret scanner
- **detect-aws-credentials** — Detects AWS credentials in files
- **detect-private-keys** — Detects private keys in files

### 2. Check for `.gitleaks.toml`

If `.gitleaks.toml` exists in the project root, append `--config .gitleaks.toml` to both gitleaks and gitleaks-full-scan entry commands. If it does not exist, use the default configuration.

## Important Notes

- Do NOT remove or modify any existing hooks.
- The `--redact` flag ensures secrets are not printed in hook output.
- gitleaks runs in two modes: `protect` (staged changes only, fast) and `detect` (full tree scan, thorough).
