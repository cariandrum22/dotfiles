---
name: setup-sql-lint
description: Set up SQLFluff linting and formatting checks for SQL files with a strict, Postgres-first configuration. Use when adding SQL quality gates.
disable-model-invocation: true
argument-hint: [dialect=postgres templater=raw max_line_length=100]
---

# Setup SQL Lint

Add SQLFluff linting and formatting checks for `.sql` files. This skill standardizes on SQLFluff and defaults to PostgreSQL unless a different dialect is provided.

**The argument `$ARGUMENTS` is an optional list of `key=value` pairs**:

- `dialect`: SQLFluff dialect (default: `postgres`)
- `templater`: SQLFluff templater (default: `raw`)
- `max_line_length`: Line length limit (default: `100`)

If no argument is given, use the defaults above.

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

Add the hook from [hooks.nix.template](hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate if it already exists.

Hook added:

- **sqlfluff-lint** — Runs `sqlfluff lint` (check-only, no auto-fix)

#### A-2. Ensure SQLFluff is available in the dev shell

Verify that `pkgs.sqlfluff` is listed in `devShells.default.packages` (or `buildInputs`). If not, add it.

---

### Path B: Pre-commit local hooks

#### B-1. Add hooks to `.pre-commit-config.yaml`

If `.pre-commit-config.yaml` does not exist, create it.

Add the hook from [pre-commit-hooks.yaml.template](pre-commit-hooks.yaml.template) to the `repos` list. Do NOT duplicate if it already exists.

Hook added:

- **sqlfluff-lint** — Runs `sqlfluff lint` on staged `.sql` files

---

### 2. Create `.sqlfluff`

Create `.sqlfluff` in the project root using the template in [sqlfluff.template](sqlfluff.template).

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
