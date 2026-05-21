# Setup C/C++ Lint

Add clang-format and clang-tidy checks for C and C++ source/header files (`.c`, `.cc`,
`.cpp`, `.cxx`, `.h`, `.hh`, `.hpp`, `.hxx`). This skill enforces strict formatting and
static analysis with check-only hooks.

**The argument `$ARGUMENTS` is an optional list of `key=value` pairs**:

- `build_dir`: Directory that contains `compile_commands.json` (default: `build`)

If no argument is given, use the default above.

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

- `clang-format` and `clang-tidy` must be available in the environment.
- For Nix Flake projects: ensure `pkgs.llvmPackages.clang-tools` (or equivalent) is in `devShells.default.packages`.
- For non-Nix projects: install clang tooling using your system package manager.
- A `compile_commands.json` database must exist in the build directory.

## Steps

### 1. Determine hook integration method

Check whether `flake.nix` exists in the project root **and** contains a `git-hooks` (or `pre-commit-hooks`) input.

- If **yes**: follow **Path A** (Nix git-hooks.nix).
- If **no**: follow **Path B** (pre-commit local hooks).

Do NOT apply both paths. Choose exactly one.

---

### Path A: Nix git-hooks.nix

#### A-1. Add hooks to `flake.nix`

Add the hooks from [hooks.nix.template](assets/hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate if they already exist.

Hooks added:

- **clang-format-check** — Runs `clang-format --dry-run --Werror` on C/C++ files
- **clang-tidy-check** — Runs `./scripts/clang-tidy-check.sh` on C/C++ files

#### A-2. Ensure clang tools are in the dev shell

Verify clang tooling is listed in `devShells.default.packages` (or `buildInputs`). If not, add it.

---

### Path B: Pre-commit local hooks

#### B-1. Add hooks to `.pre-commit-config.yaml`

If `.pre-commit-config.yaml` does not exist, create it.

Add the hooks from [pre-commit-hooks.yaml.template](assets/pre-commit-hooks.yaml.template) to the `repos` list. Do NOT duplicate if they already exist.

Hooks added:

- **clang-format-check** — Runs `clang-format --dry-run --Werror` on staged C/C++ files
- **clang-tidy-check** — Runs `./scripts/clang-tidy-check.sh` on staged C/C++ files

---

### 2. Create `.clang-format`

If `.clang-format` does not exist, create it in the project root using the
template in [clang-format.template](assets/clang-format.template).

If `.clang-format` already exists, inspect it and ask the user before changing
it. Prefer preserving the existing style and only merge clearly missing baseline
options when the user approves.

### 3. Create `.clang-tidy`

If `.clang-tidy` does not exist, create it in the project root using the
template in [clang-tidy.template](assets/clang-tidy.template).

If `.clang-tidy` already exists, inspect it and ask the user before changing it.
Merge missing safety-critical checks without removing project-specific checks,
warnings, or ignores.

### 4. Create `scripts/clang-tidy-check.sh`

If `scripts/clang-tidy-check.sh` does not exist, create it using the template in
[clang-tidy-check.sh.template](assets/clang-tidy-check.sh.template). Make it
executable (`chmod +x`).

If the script already exists, inspect it and ask the user before replacing it.
Prefer updating only the missing behavior needed by this skill.

The script:

- Requires `compile_commands.json` in `build_dir` (default: `build`)
- Reads `CLANG_TIDY_BUILD_DIR` to override the build directory at runtime
- Falls back to all git-tracked C/C++ files when invoked without file arguments

### 5. Ensure `compile_commands.json` exists

This skill requires `compile_commands.json` in the build directory. If it does not exist, configure your build system to emit it.

Examples (choose the one that matches your build system):

- Meson: ensure your build directory contains `compile_commands.json` after configuration.
- CMake: configure with `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON`.
- Make: use a compilation database generator (e.g. Bear) to produce `compile_commands.json`.

### 6. Validate

Run the following commands (or tell the user to run them):

1. `clang-format --dry-run --Werror <files>`
2. `CLANG_TIDY_BUILD_DIR=build ./scripts/clang-tidy-check.sh <files>`

If formatting violations occur, run `clang-format -i <files>` manually and review the diff.

## Important Notes

- Hooks are check-only; do NOT enable auto-fix in pre-commit or CI.
- `clang-tidy` requires a valid compilation database. Do NOT bypass this requirement.
- Keep `WarningsAsErrors: '*'` in `.clang-tidy` to preserve strictness.
