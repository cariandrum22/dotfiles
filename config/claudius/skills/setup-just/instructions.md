# Setup Just

Create a `justfile` with mandatory baseline recipes and strict shell safety settings.

**The argument `$ARGUMENTS` is the project name** used for metadata in the justfile (e.g. `my-project`). If no argument is given, infer from the current directory name.

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

- `just` must be available in the environment. If the project uses Nix Flakes, `just` MUST be declared in `flake.nix` `devShells.default.buildInputs` (or equivalent). If `just` is not available, tell the user to add `pkgs.just` to their dev shell or install `just` via their system package manager. Do NOT proceed without confirming availability.

## Steps

### 1. Create the justfile

If `justfile` (or `Justfile`) already exists in the project root, skip creation and go to Step 2.

If no justfile exists, create `justfile` (lowercase, no extension) from [justfile.template](assets/justfile.template). Replace `__PROJECT_NAME__` with the project name from `$ARGUMENTS`.

### 2. Verify mandatory settings

The following settings SHOULD be present at the top of the justfile, before any
recipe. If the justfile was created by this skill in the current run, add them.

If an existing justfile is missing any setting or uses different values, inspect
the current content, explain the behavior change, and ask the user before adding
or replacing mandatory settings. Preserve project-specific shell behavior unless
the user approves the baseline change.

```just
set shell := ["bash", "-euo", "pipefail", "-c"]
set positional-arguments
set export
```

These enforce:

- **`-e`**: Exit immediately on error
- **`-u`**: Treat unset variables as errors
- **`-o pipefail`**: Propagate pipe failures
- **positional-arguments**: Allow `$1`, `$2` in recipes
- **export**: Export all just variables as environment variables

Do NOT add `set dotenv-load` — dotenv loading is project-specific and must be opted in explicitly by the user.

Do NOT add `set windows-shell` unless the project explicitly requires Windows support.

### 3. Verify mandatory recipes

The following six recipes SHOULD be present. If the justfile was created by this
skill in the current run, add them.

If an existing justfile is missing any recipe, inspect nearby setup recipes,
propose wrapper or stub additions, and ask the user before editing. Do NOT modify
the implementation of existing recipes that already match the required signature.

| Recipe | Signature | Purpose |
|---|---|---|
| `default` | `default:` | Run `just --list --unsorted` |
| `help` | `help:` | Display `project_name` and `just --list --unsorted` |
| `install` | `install: install-deps install-tools install-hooks` | Delegate to the three sub-recipes |
| `install-deps` | `install-deps:` | Install project dependencies |
| `install-tools` | `install-tools:` | Install development tools |
| `install-hooks` | `install-hooks:` | Install git hooks |

If a recipe exists under a different name that serves the same purpose (e.g.
`setup` instead of `install`), do NOT rename it. For existing justfiles, ask
before adding the mandatory recipe name as a wrapper that delegates to the
existing one using `just <existing-recipe>`.

### 4. Verify section marker

Mandatory recipes MUST be grouped under this comment block:

```just
# =========================================================================
# Mandatory Recipes
# =========================================================================
```

If this marker does not exist in a justfile created by this skill in the current
run, add it immediately above the first mandatory recipe. For existing justfiles,
ask the user before adding the marker. Do NOT move or reorder any existing
recipes that are outside the mandatory block.

### 5. Nix Flake coordination (conditional)

Skip this step if `flake.nix` does not exist in the project root.

If `flake.nix` exists:

- The `install-deps` recipe body SHOULD be `@echo "Managed by flake.nix"` (Nix handles dependencies).
- The `install-tools` recipe body SHOULD be `@echo "Managed by flake.nix"` (Nix handles tools).
- Do NOT duplicate package installation logic from `flake.nix` in justfile recipes.

Add this comment above the mandatory block when a flake is detected:

```just
# NOTE: This project uses Nix Flakes. Dependencies and tools are managed
# by flake.nix. The install-deps and install-tools recipes are stubs.
```

Do NOT add this comment if `flake.nix` does not exist.

### 6. Validate

Run `just --evaluate` to confirm the justfile parses without errors. Also run
`just --fmt --check --unstable` for a justfile created from this skill's
template. If either command fails, fix syntax or canonical formatting before
finishing. Do NOT leave a broken or non-canonical generated justfile.

## Important Notes

- The file MUST be named `justfile` (lowercase, no extension). Do NOT use `Justfile`, `justfile.just`, or any other variant.
- `set shell` is non-negotiable. Projects MUST use bash strict mode. If a specific recipe needs different shell behavior, use a shebang line (`#!/usr/bin/env bash` with custom options) within that recipe only.
- Do NOT add optional recipes (build, test, lint, format, deploy, docker, release, ci, etc.) — those belong in project-specific extensions or other skills.
- Do NOT add color variables, decorative output, or emoji to the mandatory template. Keep it minimal and functional.
- Do NOT add a `version` variable that reads from a `VERSION` file — version management is project-specific.
- If the user has an existing justfile with custom recipes, preserve all of them unchanged. Only add missing mandatory recipes and settings.
- This skill is idempotent. Running it multiple times MUST NOT create duplicates or alter existing custom content.
