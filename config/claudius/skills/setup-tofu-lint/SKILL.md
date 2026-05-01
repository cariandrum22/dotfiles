---
name: setup-tofu-lint
description: Set up OpenTofu/Terraform lint and format hooks (tofu fmt, hclfmt, tflint, tofu validate) in a Nix flake project. Use when adding OpenTofu code quality checks.
disable-model-invocation: true
argument-hint: (no arguments required)
---

# Setup OpenTofu Lint & Format

Add OpenTofu/Terraform code quality hooks to the project's `flake.nix`.

## Prerequisites

- The project must have `git-hooks.nix` integrated in `flake.nix`. If not, tell the user to run `/setup-git-hooks` first.

## Steps

### 1. Add OpenTofu hooks to `flake.nix`

Add the hooks from [hooks.nix.template](hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate hooks that already exist — if a hook already exists, skip it.

Hooks added:

- **tofu-format** — Runs `tofu fmt` on `.tf` and `.tfvars` files
- **hclfmt** — Formats `.hcl` files (e.g. `.terraform.lock.hcl`, Packer configs)
- **tflint** — Terraform/OpenTofu linter for best practices and errors
- **tofu-validate-no-backend** — Runs `tofu init -backend=false` then `tofu validate` per directory (skips `.terraform/` dirs). Runs serially to avoid init conflicts.
- **terraform-validate** — Explicitly disabled (replaced by tofu-validate-no-backend)

### 2. Ensure `pkgs.opentofu` is available

The hooks reference `pkgs.opentofu` for tofu-format and tofu-validate-no-backend. Most packages are pulled in via `pre-commit-check.enabledPackages`, but verify that `opentofu` is accessible. If the project already has `opentofu` in its packages, no action needed.

## Important Notes

- Do NOT remove or modify any existing hooks.
- The `tofu-validate-no-backend` hook uses `require_serial = true` because `tofu init` can conflict when run in parallel across directories.
- The hook sets `AWS_EC2_METADATA_DISABLED=true` and uses a temporary `TF_CLI_CONFIG_FILE` to avoid interference with real backends during validation.
