# Setup Shell Lint

Add shellcheck and shfmt hooks to the project's `flake.nix`.

## Prerequisites

- The project must have `git-hooks.nix` integrated in `flake.nix`. If not, tell the user to run `/setup-git-hooks` first.

## Steps

### 1. Add shell lint hooks to `flake.nix`

Add the hooks from [hooks.nix.template](assets/hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate hooks that already exist — if a hook already exists, skip it.

Hooks added:

- **shellcheck** — Static analysis for shell scripts (default settings = all checks enabled, strictest mode)
- **shfmt** — Shell script formatter

### 2. Verify no `.shellcheckrc` exists

The intended strictness is **all checks enabled** (shellcheck default). Do NOT create a `.shellcheckrc` file. If one already exists, leave it as is but inform the user that the skill assumes all checks are enabled by default.

## Strictness Details

### shellcheck

No configuration file (`.shellcheckrc`) is used. This means:
- All warning categories are enabled (error, warning, info, style)
- No rules are disabled
- This is the strictest possible configuration

### shfmt

The hook runs with default settings via git-hooks.nix. For projects that need custom formatting rules (e.g. tab indentation, case indent, space redirects), the user should add an `.editorconfig` with shell-specific settings or wrap shfmt in a custom hook.

## Important Notes

- Do NOT remove or modify any existing hooks.
- Do NOT create `.shellcheckrc` — the absence of this file is intentional for maximum strictness.
- If `shfmt` is explicitly set to `enable = false` in the project, ask the user whether they want to enable it before adding.
