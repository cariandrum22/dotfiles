# Setup Nix Flake

Create a minimal `flake.nix` with an empty devShell and a `.envrc` for direnv integration. This skill produces the foundation that all other Nix-based skills (`/setup-git-hooks`, `/setup-nix-lint`, etc.) build upon.

**The argument `$ARGUMENTS` is a short project description** used in the flake's `description` field (e.g. `Development environment for my-api`). If no argument is given, infer from the current directory name and format as `Development environment for <dir-name>`.

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

- `nix` must be installed with flakes enabled (`experimental-features = nix-command flakes`). If not available, tell the user to install Nix first. Do NOT proceed without it.
- `direnv` should be installed. If not available, warn the user but proceed with `flake.nix` creation only.

## Steps

### 1. Check for existing flake

If `flake.nix` already exists in the project root, do NOT overwrite it. Skip to Step 3.

### 2. Create `flake.nix`

Create `flake.nix` in the project root from [flake.nix.template](assets/flake.nix.template). Replace `__PROJECT_DESCRIPTION__` with the description from `$ARGUMENTS`.

The template provides:

- **`nixpkgs`** input pinned to `nixos-unstable`
- **`flake-utils`** input for multi-system support
- **`devShells.default`** with empty `packages` and `shellHook`
- A `let` block where downstream skills (e.g. `/setup-git-hooks`) can add bindings

Do NOT add any of the following to the template:

- Language-specific inputs (rust-overlay, poetry2nix, etc.)
- Language-specific packages in `packages`
- `nixConfig` or binary cache settings
- Additional outputs (`packages`, `apps`, `formatter`, `checks`)

These belong in downstream skills or project-specific customization.

### 3. Create `.envrc`

If `.envrc` already exists in the project root, verify it contains `use flake`.
If it does, skip this step. If it does not, inspect the current content, explain
the proposed `use flake` addition, and ask the user before appending it.

If `.envrc` does not exist, create it from [envrc.template](assets/envrc.template).

Do NOT run `direnv allow` — the user must do this explicitly as a security decision.

### 4. Ensure `.envrc.local` is git-ignored

If `.gitignore` exists, check whether it contains `.envrc.local`. If not,
inspect the current content, explain the proposed ignore entry, and ask the user
before appending it.

If `.gitignore` does not exist, create it with `.envrc.local` as the sole entry.

`.envrc.local` is for machine-specific overrides (tokens, paths) and MUST NOT be tracked.

### 5. Ensure `flake.lock` is tracked

If `.gitignore` contains `flake.lock`, warn the user: `flake.lock` MUST be committed for reproducibility. Do NOT modify `.gitignore` automatically — inform the user and let them decide.

If `flake.lock` does not exist yet, tell the user to run `nix flake lock` to generate it.

### 6. Validate

If `flake.nix` was created or already existed, run `nix flake check` mentally to verify the structure is sound. Do NOT actually run it unless the user asks — it can trigger downloads.

## Downstream Skills

After this skill completes, the following skills can be applied in order:

1. `/setup-git-hooks` — adds `git-hooks.nix` input and devShell wiring
2. `/setup-nix-lint` — adds Nix lint hooks
3. Language-specific lint skills — add packages and hooks
4. `/setup-nix-ci-lint` — generates CI from final hook configuration

## Important Notes

- This skill creates the **minimal viable flake**. It is intentionally empty. Do NOT populate `packages` with tools — that is the responsibility of downstream skills and the user.
- The `flake-utils.lib.eachDefaultSystem` pattern is required. Do NOT use `eachSystem` with a custom system list or raw per-system outputs. Downstream skills (especially `/setup-git-hooks`) expect this pattern.
- Do NOT add `flake-compat` or `default.nix` / `shell.nix` shims. This skill targets flake-native workflows only.
- Do NOT add overlays. If a downstream skill needs overlays (e.g. rust-overlay), that skill is responsible for adding them.
- The `.envrc` uses `return 1` (not `exit 1`) because direnv sources the file — `exit` would kill the user's shell.
- This skill is idempotent. Running it on a project that already has `flake.nix` and `.envrc` will only verify and fill gaps without modifying existing content.
