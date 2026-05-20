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
- minimal `contents: read` permissions by default
- SHA-pinned GitHub Actions for Nix installation and build caching
- `nix develop -c pre-commit run --all-files --show-diff-on-failure` to run all hooks

For mature infrastructure repositories, do not leave tag-based `uses:` references in the final
workflow. If an Action version needs to change, resolve the intended tag to a full commit SHA with
`git ls-remote https://github.com/<owner>/<repo>.git 'refs/tags/<tag>^{}'` and keep the
human-readable tag in the step name or an adjacent comment. Use the peeled `^{}` ref for annotated
tags; if it is absent because the tag is lightweight, use the direct `refs/tags/<tag>` value.

### 3. Add hook-specific initialization steps

Inspect the hooks from step 1. If any of the following hooks are enabled, insert the corresponding initialization step **before** the "Run pre-commit hooks" step:

| Hook | Initialization step |
|---|---|
| `tflint` | `nix develop -c tflint --init` |

If no hooks need initialization, skip this step.

When using `git-hooks.nix`, the enabled hook package for `tflint` may be a per-file wrapper rather
than the real CLI. Verify `nix develop -c bash -lc 'head -n 2 "$(command -v tflint)"'` before relying
on it. If it is a wrapper, add the real `pkgs.tflint` to `devShells.default.packages` before
`pre-commit-check.enabledPackages` so `nix develop -c tflint --init` resolves to the real binary
while the pre-commit hook still uses its generated wrapper entry.

### 4. Preserve local/CI parity

Do not skip hooks in CI by default. The repo-wide lint workflow should exercise the same checks as
`pre-commit run --all-files` in the project dev shell.

If a hook cannot run in CI, first try to make it deterministic and backend-free. For OpenTofu
validation, prefer `tofu init -backend=false` with locked providers, metadata lookup disabled, and
no cloud credentials. Add `SKIP` only when the hook still requires an unavailable external resource,
and document the concrete reason inline next to the `SKIP` entry.

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
- Keep lint workflow permissions at `contents: read` unless a step demonstrably needs broader access.
- Run hook initialization inside the project flake/dev shell so CI cannot silently drift from the pinned local toolchain.
- Confirm hook initialization commands resolve to real tool CLIs, not git-hooks wrapper scripts.
