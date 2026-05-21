# Setup File Hygiene

Add general file quality and hygiene hooks to the project's `flake.nix`.

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

### 1. Add file hygiene hooks to `flake.nix`

Add the hooks from [hooks.nix.template](assets/hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate hooks that already exist — if a hook already exists, skip it.

Hooks added:

**Formatting & whitespace:**
- **editorconfig-checker** — Validates `.editorconfig` rules when `.editorconfig` exists; disabled otherwise
- **final-newline-check** — Checks tracked non-empty text files end with a newline without rewriting them
- **trailing-whitespace-check** — Checks tracked text files for trailing whitespace without rewriting them
- **mixed-line-endings** — Detects mixed line endings (LF/CRLF) without rewriting files

**File checks:**
- **check-added-large-files** — Blocks files over 200KB
- **check-case-conflicts** — Detects filename case conflicts
- **check-executables-have-shebangs** — Ensures executables have shebangs
- **check-shebang-scripts-are-executable** — Ensures shebang scripts are executable
- **check-symlinks** — Validates symlinks
- **check-vcs-permalinks** — Checks for non-permalink VCS URLs
- **forbid-new-submodules** — Prevents adding git submodules
- **check-merge-conflicts** — Detects unresolved merge conflict markers

**Data format validation:**
- **check-json** — Validates JSON syntax
- **check-toml** — Validates TOML syntax
- **check-yaml** — Validates YAML syntax

**Documentation:**
- **markdownlint** — Lints Markdown files using `.markdownlint.json` config when present, or markdownlint defaults otherwise

**Spelling:**
- **typos** — Detects typos using `.typos.toml` config when present, or typos defaults otherwise

### 2. Remind the user about config files

After completing the setup, remind the user about the following config files. Do
NOT create them automatically — just list any that are missing:

- `.editorconfig` — EditorConfig rules (optional; editorconfig-checker is disabled when missing)
- `.markdownlint.json` — markdownlint configuration (optional; defaults are used when missing)
- `.typos.toml` — typos configuration (optional; defaults are used when missing)

## Important Notes

- Do NOT remove or modify any existing hooks.
- Keep file hygiene hooks check-only by default. Do NOT enable mutating hooks
  such as `end-of-file-fixer` or `trim-trailing-whitespace` unless the user
  explicitly accepts auto-fix behavior.
- Keep `mixed-line-endings` configured with `--fix=no`. Its upstream default
  rewrites files, which violates the check-only policy.
- The `check-added-large-files` hook uses `--maxkb=200`. If the project has large generated files that should be excluded, the user should add `excludes` entries.
- The `typos` hook uses `.typos.toml` via `configPath` only when the file exists. If the project has directories that should be excluded (generated code, vendored deps), the user should add `exclude` entries to the typos settings or create `.typos.toml`.
