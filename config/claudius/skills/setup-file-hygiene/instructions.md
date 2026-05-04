# Setup File Hygiene

Add general file quality and hygiene hooks to the project's `flake.nix`.

## Prerequisites

- The project must have `git-hooks.nix` integrated in `flake.nix`. If not, tell the user to run `/setup-git-hooks` first.

## Steps

### 1. Add file hygiene hooks to `flake.nix`

Add the hooks from [hooks.nix.template](assets/hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate hooks that already exist — if a hook already exists, skip it.

Hooks added:

**Formatting & whitespace:**
- **editorconfig-checker** — Validates `.editorconfig` rules
- **end-of-file-fixer** — Ensures files end with a newline
- **trim-trailing-whitespace** — Removes trailing whitespace
- **mixed-line-endings** — Detects mixed line endings (LF/CRLF)

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
- **markdownlint** — Lints Markdown files using `.markdownlint.json` config

**Spelling:**
- **typos** — Detects typos using `.typos.toml` config

### 2. Remind the user about config files

After completing the setup, remind the user that these hooks expect the following config files. Do NOT create them — just list any that are missing:

- `.editorconfig` — EditorConfig rules
- `.markdownlint.json` — markdownlint configuration
- `.typos.toml` — typos configuration

## Important Notes

- Do NOT remove or modify any existing hooks.
- The `check-added-large-files` hook uses `--maxkb=200`. If the project has large generated files that should be excluded, the user should add `excludes` entries.
- The `typos` hook uses `.typos.toml` via `configPath`. If the project has directories that should be excluded (generated code, vendored deps), the user should add `exclude` entries to the typos settings.
