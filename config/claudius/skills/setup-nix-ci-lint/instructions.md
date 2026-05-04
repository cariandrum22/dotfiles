# Setup Nix CI Lint

Generate `.github/workflows/lint.yml` by reading the project's `flake.nix` hook configuration and building a GitHub Actions workflow using Determinate Systems actions.

## Steps

### 1. Read the project's `flake.nix`

Read `flake.nix` in the project root and extract:
- The list of enabled hooks in `hooks = { ... }`
- Whether commitlint hooks exist (commit-msg / pre-push stages)
- Any hooks that may need initialization or special CI handling

### 2. Create `.github/workflows/lint.yml`

Create the workflow using [lint.yml.template](assets/lint.yml.template) as the base structure. The base provides:
- Trigger on `pull_request` and `push` to all branches
- Checkout with `fetch-depth: 0` (needed for commitlint range checks)
- `DeterminateSystems/determinate-nix-action@v3` for Nix installation
- `DeterminateSystems/magic-nix-cache-action@v13` for build caching
- `nix develop -c pre-commit run --all-files` to run all hooks

### 3. Add hook-specific initialization steps

Inspect the hooks from step 1. If any of the following hooks are enabled, insert the corresponding initialization step **before** the "Run pre-commit hooks" step:

| Hook | Initialization step |
|---|---|
| `tflint` | `nix run nixpkgs#tflint -- --init` |

If no hooks need initialization, skip this step.

### 4. Determine hooks to skip in CI

Some hooks require resources not available in CI (e.g. cloud credentials, running backends). If any of the following hooks are enabled, add them to the `SKIP` environment variable on the "Run pre-commit hooks" step:

| Hook | Reason to skip |
|---|---|
| `tofu-validate-no-backend` | Requires `tofu init` which may need provider credentials |

Format: `SKIP: hook1,hook2` as an `env` entry. If nothing needs to be skipped, do not add the `env` block.

### 5. Add project-specific dependency installation steps

Read the `devShells.default` in `flake.nix`. If the project has application dependencies that need installation before hooks can run (e.g. `bundle install`, `pnpm install`), add installation steps **before** the "Run pre-commit hooks" step. Use `nix develop -c bash -c '...'` to run them within the Nix dev shell.

If no dependency installation is needed (common for pure IaC projects), skip this step.

### 6. Add commitlint CI steps

If commitlint hooks are present in the flake, append the commit message linting steps. These steps are defined by the `/setup-commitlint` skill's CI template — read the `setup-commitlint` skill asset `assets/ci-steps.yml.template` and append its contents after the "Run pre-commit hooks" step.

If commitlint hooks are not present, skip this step.

## Important Notes

- If `.github/workflows/lint.yml` already exists, ask the user before overwriting.
- Do NOT hardcode project-specific details. Derive everything from the flake.nix configuration.
- The `fetch-depth: 0` on checkout is required for commitlint `--from`/`--to` range checks to work. Always include it even if commitlint is not yet configured, to avoid needing to change it later.
- The Determinate Systems actions handle Nix installation and caching. Do NOT use `cachix/install-nix-action` or other alternatives.
