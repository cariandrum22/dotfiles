# Setup SQL Lint

Add SQLFluff linting and formatting checks for `.sql` files. This skill standardizes on SQLFluff and defaults to PostgreSQL unless a different dialect is provided.

**The argument `$ARGUMENTS` is an optional list of `key=value` pairs**:

- `dialect`: SQLFluff dialect (default: `postgres`)
- `templater`: SQLFluff templater (default: `raw`)
- `max_line_length`: Line length limit (default: `100`)

If no argument is given, use the defaults above.

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

- `sqlfluff` must be available in the environment.
- For Nix Flake projects: ensure `pkgs.sqlfluff` is listed in `devShells.default.packages` (or `buildInputs`).
- For non-Nix projects: install SQLFluff with your Python package manager (do not run the command automatically).

## Steps

### 1. Determine hook integration method

Check whether `flake.nix` exists in the project root **and** contains a `git-hooks` (or `pre-commit-hooks`) input.

- If **yes**: follow **Path A** (Nix git-hooks.nix).
- If **no**: follow **Path B** (pre-commit local hooks).

Do NOT apply both paths. Choose exactly one.

---

### Path A: Nix git-hooks.nix

#### A-1. Add hooks to `flake.nix`

Add the hook from [hooks.nix.template](assets/hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate if it already exists.

Hook added:

- **sqlfluff-lint** — Runs `sqlfluff lint` (check-only, no auto-fix)

#### A-2. Ensure SQLFluff is available in the dev shell

Verify that `pkgs.sqlfluff` is listed in `devShells.default.packages` (or `buildInputs`). If not, add it.

---

### Path B: Pre-commit local hooks

#### B-1. Add hooks to `.pre-commit-config.yaml`

If `.pre-commit-config.yaml` does not exist, create it.

Add the hook from [pre-commit-hooks.yaml.template](assets/pre-commit-hooks.yaml.template) to the `repos` list. Do NOT duplicate if it already exists.

Hook added:

- **sqlfluff-lint** — Runs `sqlfluff lint` on staged `.sql` files

---

### 2. Create `.sqlfluff`

If `.sqlfluff` does not exist, create it in the project root using the template
in [sqlfluff.template](assets/sqlfluff.template).

If `.sqlfluff` already exists, inspect it and ask the user before changing it.
Merge only missing baseline values that are compatible with the current dialect,
templater, and project-specific rule choices. Do not replace the file wholesale.

Replace the placeholders:

- `__DIALECT__`
- `__TEMPLATER__`
- `__MAX_LINE_LENGTH__`

Use the defaults if no argument is provided.

### 3. Strictness policy

- Do NOT add `exclude_rules` unless there is an explicit, documented exception.
- Keep hooks check-only. Do NOT add `sqlfluff fix` to hooks or CI.

### 4. Validate

Run the following commands (or tell the user to run them):

1. `sqlfluff lint .`
2. `sqlfluff fix .` (manual formatting only, never in hooks)

If lint violations occur, fix them manually or with `sqlfluff fix` and review the diff.

## Optional templater guidance

If templating becomes necessary, set `templater` explicitly and add any required templater-specific configuration. If the project uses dbt or Jinja templating, configure SQLFluff accordingly before enabling lint enforcement in CI.

## Important Notes

- Do NOT run `sqlfluff fix` in pre-commit or CI hooks.
- Do NOT weaken rule enforcement by adding broad `exclude_rules` without explicit rationale.
- Keep the dialect explicit and consistent across the team.
