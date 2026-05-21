# Setup TF Template Lint

Add a strict validation hook for `.tftpl` files by rendering them with `templatefile()` in Terraform/OpenTofu console. This detects syntax errors and missing variables early.

**The argument `$ARGUMENTS` is an optional list of `key=value` pairs**:

- `engine`: `tofu` or `terraform` (default: `tofu` if available, otherwise `terraform`)
- `vars_file`: Variables JSON file path (default: `tftpl.vars.json`)
- `console_dir`: Directory used for console evaluation (default: project root)

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

- `tofu` or `terraform` must be available in the environment.
- The project must provide a variables JSON file for template rendering.

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

- **tftpl-validate** — Runs `./scripts/tftpl-validate.sh` (check-only)

---

### Path B: Pre-commit local hooks

#### B-1. Add hooks to `.pre-commit-config.yaml`

If `.pre-commit-config.yaml` does not exist, create it.

Add the hook from [pre-commit-hooks.yaml.template](assets/pre-commit-hooks.yaml.template) to the `repos` list. Do NOT duplicate if it already exists.

Hook added:

- **tftpl-validate** — Runs `./scripts/tftpl-validate.sh` on staged `.tftpl` files

---

### 2. Create `tftpl.vars.json`

If `tftpl.vars.json` does not exist, create it in the project root using the
template in [tftpl.vars.json.template](assets/tftpl.vars.json.template).
Populate it with all variables required by your templates.

If the variables file already exists, inspect it and ask the user before
changing it. Merge only missing variables required by the templates; do not
delete project-specific sample values.

### 3. Create `scripts/tftpl-validate.sh`

If `scripts/tftpl-validate.sh` does not exist, create it using the template in
[tftpl-validate.sh.template](assets/tftpl-validate.sh.template). Make it
executable (`chmod +x`).

If the script already exists, inspect it and ask the user before changing it.
Merge missing validation behavior without replacing project-specific engine,
console directory, or variable-loading logic unless approved.

The script:

- Selects `tofu` or `terraform` based on availability (or `TFTPL_ENGINE` override)
- Loads variables from `TFTPL_VARS_FILE` (default: `tftpl.vars.json`)
- Evaluates each `.tftpl` file using `templatefile()` in console

### 4. Validate

Run the following command (or tell the user to run it):

`./scripts/tftpl-validate.sh <files>`

If any template fails to render, fix the syntax or update `tftpl.vars.json`.

## Important Notes

- This hook is check-only. Do NOT auto-generate template outputs in hooks or CI.
- The validation script must discard rendered template stdout. Do NOT print
  rendered `.tftpl` bodies in pre-commit or CI logs unless the user explicitly
  asks for a debug run and the output has been reviewed for sensitive values.
- Keep `tftpl.vars.json` complete and explicit. Missing variables must fail the hook.
- If templates require module context or providers, run the hook in a directory where console evaluation is valid.
