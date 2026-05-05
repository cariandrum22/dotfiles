# Setup Lean Lint

Add Lean quality checks for a Lake project. This skill standardizes on
`lake build` plus `lake lint`, using the built-in Clippy-style linters by
default. Keep the formatting surface project-specific and check-only.

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

Create `scripts/lean-lint-check.sh` from
[lean-lint-check.sh.template](assets/lean-lint-check.sh.template) and make it
executable.

The script:

- runs `lake build`
- runs `lake lint --clippy` by default
- supports `LEAN_LINT_SCOPE=default|clippy|all` to adjust the built-in linter
  scope

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
- Use `LEAN_LINT_SCOPE=all` only when the project is already ready for the
  broader lint set; `clippy` is the strict default for general Lean projects.
