# Setup OpenTofu Lint & Format

Add OpenTofu/Terraform code quality hooks to the project's `flake.nix`.

## Prerequisites

- The project must have `git-hooks.nix` integrated in `flake.nix`. If not, tell the user to run `/setup-git-hooks` first.

## Steps

### 1. Add OpenTofu hooks to `flake.nix`

Add the hooks from [hooks.nix.template](assets/hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate hooks that already exist — if a hook already exists, skip it.

Hooks added:

- **tofu-format** — Runs `tofu fmt` on `.tf` and `.tfvars` files
- **hclfmt** — Formats `.hcl` files (e.g. `.terraform.lock.hcl`, Packer configs)
- **tflint** — Terraform/OpenTofu linter for best practices and errors
- **tofu-validate-no-backend** — Reads root module directories from `.tofu-root-modules`, requires each root to have `.terraform.lock.hcl`, then runs `tofu init -backend=false -lockfile=readonly` and `tofu validate` for each root module. Runs serially to avoid init conflicts.
- **terraform-validate** — Explicitly disabled (replaced by tofu-validate-no-backend)

### 2. Ensure `pkgs.opentofu` is available

The hooks reference `pkgs.opentofu` for tofu-format and tofu-validate-no-backend. Most packages are pulled in via `pre-commit-check.enabledPackages`, but verify that `opentofu` is accessible. If the project already has `opentofu` in its packages, no action needed.

### 3. Declare root modules and check provider reproducibility

For each root module that CI validates or production applies:

- Add the root module directory to `.tofu-root-modules`, one path per line. Use `.` for a single
  root at the repository root. Blank lines and `#` comments are allowed.
- Track `.terraform.lock.hcl`.
- Prefer bounded provider constraints (`~>` or explicit upper bounds) over bare `>=`.
- Keep OpenTofu itself pinned by the flake/toolchain rather than relying on host state.
- Verify `TF_DATA_DIR=<temp-dir> tofu init -backend=false -lockfile=readonly && tofu validate` works
  without cloud credentials or cached local backend state.

If a project is new and has no provider lock file yet, initialize it intentionally once and commit the
lock file before enabling the hook as a required gate. Do not list a root module until its lock file
is committed.

Reusable modules should normally be validated through one or more locked root modules that consume
them. Do not run `tofu init -lockfile=readonly` directly in module-only directories unless they have
their own committed lock file or a dedicated validation harness.

If a repository intentionally contains only reusable modules and validates them elsewhere, set
`TOFU_VALIDATE_ALLOW_NO_LOCKED_ROOTS=1` on the `tofu-validate-no-backend` hook with an inline comment
explaining the external validation path. Do not use this escape hatch for repositories with root
modules that should have committed provider locks.

## Important Notes

- Do NOT remove or modify any existing hooks.
- The `tofu-validate-no-backend` hook uses `pass_filenames = false` so module-only changes still
  validate the repository's locked root modules instead of trying to initialize reusable module
  directories directly.
- The hook fails when `.tf` files exist but `.tofu-root-modules` is missing, unless
  `TOFU_VALIDATE_ALLOW_NO_LOCKED_ROOTS=1` is explicitly set for a module-only repository.
- The hook fails when a directory listed in `.tofu-root-modules` is missing or lacks
  `.terraform.lock.hcl`.
- The `tofu-validate-no-backend` hook uses `require_serial = true` because `tofu init` can conflict when run in parallel across directories.
- The hook sets `AWS_EC2_METADATA_DISABLED=true` and uses temporary `TF_CLI_CONFIG_FILE` and
  `TF_DATA_DIR` values to avoid interference from real backends or cached local `.terraform` state
  during validation.
- Validation hooks should be check-only gates. They may create local `.terraform` working data, but should not rewrite provider locks during CI.
