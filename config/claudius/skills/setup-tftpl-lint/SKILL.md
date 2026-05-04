---
name: setup-tftpl-lint
description: Set up templatefile rendering checks for Terraform/OpenTofu .tftpl files. Use when adding strict template validation.
disable-model-invocation: true
argument-hint: [engine=tofu vars_file=tftpl.vars.json console_dir=.]
---

# Setup TF Template Lint

Add a strict validation hook for `.tftpl` files by rendering them with `templatefile()` in Terraform/OpenTofu console. This detects syntax errors and missing variables early.

**The argument `$ARGUMENTS` is an optional list of `key=value` pairs**:

- `engine`: `tofu` or `terraform` (default: `tofu` if available, otherwise `terraform`)
- `vars_file`: Variables JSON file path (default: `tftpl.vars.json`)
- `console_dir`: Directory used for console evaluation (default: project root)

If no argument is given, use the defaults above.

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

Add the hook from [hooks.nix.template](hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate if it already exists.

Hook added:

- **tftpl-validate** — Runs `./scripts/tftpl-validate.sh` (check-only)

---

### Path B: Pre-commit local hooks

#### B-1. Add hooks to `.pre-commit-config.yaml`

If `.pre-commit-config.yaml` does not exist, create it.

Add the hook from [pre-commit-hooks.yaml.template](pre-commit-hooks.yaml.template) to the `repos` list. Do NOT duplicate if it already exists.

Hook added:

- **tftpl-validate** — Runs `./scripts/tftpl-validate.sh` on staged `.tftpl` files

---

### 2. Create `tftpl.vars.json`

Create `tftpl.vars.json` in the project root using the template in [tftpl.vars.json.template](tftpl.vars.json.template). Populate it with all variables required by your templates.

### 3. Create `scripts/tftpl-validate.sh`

Create `scripts/tftpl-validate.sh` using the template in [tftpl-validate.sh.template](tftpl-validate.sh.template). Make it executable (`chmod +x`).

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
- Keep `tftpl.vars.json` complete and explicit. Missing variables must fail the hook.
- If templates require module context or providers, run the hook in a directory where console evaluation is valid.
