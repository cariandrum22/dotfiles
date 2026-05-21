# Setup IaC Security Scanning

Add IaC security scanning hooks to the project's `flake.nix`.

## Existing Configuration Policy

Before changing an existing repository file, inspect the current content and ask the user to confirm the proposed change. This applies even when the change is additive, such as merging config keys, appending CI steps, adding package scripts, normalizing workflow names, or updating tool versions.

Do not ask when creating a missing file from this skill's template or when the user explicitly requested applying all changes without confirmation. Preserve project-specific settings and avoid replacing entire files unless the user approves that replacement.

When an existing file is involved, present a concise change plan before editing:

- `file`: target path
- `current_state`: what exists and whether this skill owns it
- `operation`: `skip`, `merge`, `update`, `replace`, or `create-adjacent`
- `proposed_delta`: exact setting, block, command, or path change to add or modify
- `risk`: compatibility, policy, or behavior risk
- `question`: the approval needed from the user

Default to `skip` or `merge`. Use `replace` only when the user explicitly
approves replacing that file.

If the user explicitly requested applying all changes without confirmation, do
not wait for approval after presenting the plan. Still follow the plan, preserve
project-specific settings, and do not use `replace` unless the user explicitly
allowed replacement.

Otherwise, if multiple existing files are affected, batch them in one plan and
wait for approval before editing any of them.

## Prerequisites

- The project must have `git-hooks.nix` integrated in `flake.nix`. If not, tell the user to run `/setup-git-hooks` first.

## Steps

### 1. Add security scanning hooks to `flake.nix`

Add the hooks from [hooks.nix.template](assets/hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate hooks that already exist — if a hook already exists, skip it.

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
