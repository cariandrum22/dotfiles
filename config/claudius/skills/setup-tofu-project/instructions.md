# Setup OpenTofu Project

Set up a complete linting, formatting, security scanning, CI, and commit message enforcement stack for an OpenTofu IaC project. This skill orchestrates the following individual skills in order.

**The argument `$ARGUMENTS` is a comma-separated list of allowed commitlint scopes.** If no argument is given, ask the user for the list of scopes.

## Execution Order

Apply each skill in the following order. For each step, follow the instructions defined in that skill. If a skill's prerequisites are already satisfied (e.g. hooks already exist), skip the redundant parts but still verify correctness.

### Step 1: `/setup-git-hooks`
Add `git-hooks.nix` infrastructure to `flake.nix` (input, helpers, devShell wiring).

### Step 2: `/setup-nix-lint`
Add Nix lint hooks (nixfmt, statix, deadnix) and `nix fmt` formatter output.

### Step 3: `/setup-shell-lint`
Add shell script linting (shellcheck, shfmt).

### Step 4: `/setup-file-hygiene`
Add file quality checks (editorconfig, whitespace, large files, data format validation, markdownlint, typos).

### Step 5: `/setup-secrets-scan`
Add secret detection (gitleaks, ripsecrets, detect-aws-credentials, detect-private-keys).

### Step 6: `/setup-tofu-lint`
Add OpenTofu code quality hooks (tofu fmt, hclfmt, tflint, tofu validate).

### Step 7: `/setup-iac-security`
Add IaC security scanning (tfsec, trivy).

### Step 8: `/setup-commitlint $ARGUMENTS`
Add commitlint with Conventional Commits rules, git hooks, and CI enforcement using the provided scopes.

### Step 9: `/setup-nix-ci-lint`
Generate `.github/workflows/lint.yml` from the flake.nix hook configuration using Determinate Systems actions.

## After Completion

Summarize what was set up and list any config files the user needs to create:
- `.editorconfig`
- `.markdownlint.json`
- `.typos.toml`

Only list files that do not already exist in the project root.

## Important Notes

- Each step is idempotent — if hooks or config already exist, they are skipped, not duplicated.
- The execution order matters: `setup-git-hooks` must run first as all other skills depend on it.
- `/setup-nix-ci-lint` must run last because it reads the final hook configuration to generate CI.
- This skill is designed for OpenTofu IaC projects. For application projects with different language tooling, use the individual skills selectively instead.
