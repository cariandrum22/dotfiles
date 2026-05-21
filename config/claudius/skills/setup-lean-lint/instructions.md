# Setup Lean Lint

Add Lean quality checks for a Lake project. This skill standardizes on
`lake build` plus `lake lint` when the project exposes a Lake lint driver. Keep
the formatting surface project-specific and check-only.

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

- The project must have `lean-toolchain`.
- The project must have `lakefile.lean` or `lakefile.toml`.
- `lake` must be available in the environment.
- For Nix Flake projects: ensure the Lean toolchain is available from the dev
  shell or via an existing `elan`-managed workflow.
- For non-Nix projects: tell the user to install the Lean toolchain with their
  existing workflow. Do NOT run installation commands automatically.

## Steps

### 1. Determine hook integration method

Check whether `flake.nix` exists in the project root and already contains a
`git-hooks` or `pre-commit-hooks` input.

- If yes: follow Path A (Nix `git-hooks.nix`).
- If no: follow Path B (pre-commit local hooks).

Do NOT apply both paths.

---

### Path A: Nix git-hooks.nix

#### A-1. Add hooks to `flake.nix`

Add the hook from [hooks.nix.template](assets/hooks.nix.template) into the
`hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate
if it already exists.

Hook added:

- `lean-lint` - runs the shared Lean quality script

---

### Path B: Pre-commit local hooks

#### B-1. Add hooks to `.pre-commit-config.yaml`

If `.pre-commit-config.yaml` does not exist, create it.

Add the hook from
[pre-commit-hooks.yaml.template](assets/pre-commit-hooks.yaml.template) to the
`repos` list. Do NOT duplicate if it already exists.

Hook added:

- `lean-lint` - runs the shared Lean quality script

### 2. Create `scripts/lean-lint-check.sh`

If `scripts/lean-lint-check.sh` does not exist, create it from
[lean-lint-check.sh.template](assets/lean-lint-check.sh.template) and make it
executable.

If the script already exists, inspect it and ask the user before changing it.
Merge missing Lean quality checks without replacing project-specific Lake or
lint-driver logic unless approved.

The script:

- runs `lake build`
- runs `lake lint` when a project lint driver is configured
- skips lint with a clear message when no Lake lint driver is configured
- supports `LEAN_LINT_REQUIRE_DRIVER=1` to fail if lint is unavailable

### 3. Inspect the project's strictness surface

Inspect `lakefile.lean` or `lakefile.toml`.

- If the project centralizes shared `leanOptions`, prefer `autoImplicit = false`
  or the equivalent project-wide option there instead of scattering
  file-local exceptions.
- Preserve any existing `lintDriver`, `builtinLint?`, or custom `lean_exe`
  linters. Do NOT replace a stronger repository-specific lint surface.
- If the project already exposes a checked formatter command, keep it under a
  separate stable hook name such as `lean-format`.

### 4. Validate

Run the following commands, or tell the user to run them:

1. `lake build`
2. `./scripts/lean-lint-check.sh`

If lint fails, fix the code or project lint configuration. Do NOT weaken the
built-in linter scope globally just to get the hook green.

## Important Notes

- Do NOT invent an auto-format hook that rewrites files during commit.
- Keep formatting editor- or project-command driven unless the repository
  already has a checked formatter surface.
- Lake does not provide a universal `--clippy` or `--lint-all` flag. Preserve
  any project-specific lint driver and run `lake lint` through that driver.
- Set `LEAN_LINT_REQUIRE_DRIVER=1` only when the repository requires a configured
  lint driver before commits or CI may pass.
