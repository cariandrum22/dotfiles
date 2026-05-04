---
name: setup-iac-security
description: Set up IaC security scanning hooks (tfsec, trivy) in a Nix flake project. Use when adding infrastructure-as-code security checks.
disable-model-invocation: true
argument-hint: (no arguments required)
---

# Setup IaC Security Scanning

Add IaC security scanning hooks to the project's `flake.nix`.

## Prerequisites

- The project must have `git-hooks.nix` integrated in `flake.nix`. If not, tell the user to run `/setup-git-hooks` first.

## Steps

### 1. Add security scanning hooks to `flake.nix`

Add the hooks from [hooks.nix.template](hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate hooks that already exist — if a hook already exists, skip it.

Hooks added:

- **tfsec** — Static analysis for Terraform/OpenTofu to detect security issues and misconfigurations. Scans all directories, excludes `.direnv` and `.terraform`.
- **trivy** — Vulnerability scanner for IaC configurations. Checks for CRITICAL, HIGH, MEDIUM, and LOW severity issues. Excludes `.direnv` and `.terraform`.

Both hooks:
- Run against the entire project (not per-file) via `pass_filenames = false`
- Exclude `.direnv` and `.terraform` directories

## Important Notes

- Do NOT remove or modify any existing hooks.
- These hooks scan IaC configurations (Terraform, CloudFormation, Kubernetes manifests, Dockerfiles, etc.), not just OpenTofu files.
- Both tools exit with non-zero status on findings, blocking the commit.
