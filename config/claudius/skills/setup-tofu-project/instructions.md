# Setup OpenTofu Project

Set up a complete linting, formatting, security scanning, CI, and commit message enforcement stack for an OpenTofu IaC project. This skill orchestrates the following individual skills in order.

**The argument `$ARGUMENTS` is optional** and may provide a comma-separated list
of allowed commitlint scopes. If no argument is given, run `/setup-commitlint`
without explicit scopes so it can infer the repository taxonomy.

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

### Step 8: `/setup-commitlint`
Add commitlint with Conventional Commits rules, git hooks, and CI enforcement.
Pass `$ARGUMENTS` through only when explicit scopes were provided; otherwise let
`/setup-commitlint` infer and optimize scopes from the repository layout.

### Step 9: `/setup-nix-ci-lint`
Generate `.github/workflows/lint.yml` from the flake.nix hook configuration using Determinate Systems actions.

### Step 10: `/setup-github-guardrails tofu-infra`
Normalize workflow names, job names, permissions, required-check recommendations, and Action pinning
against the OpenTofu infrastructure profile.

## After Completion

Summarize what was set up and list any required config files the user needs to create:
- `.editorconfig`

Only list files that do not already exist in the project root.

Also mention these optional override files if they do not exist and the project would benefit from custom rules:
- `.markdownlint.json` — markdownlint uses defaults when this is missing
- `.typos.toml` — typos uses defaults when this is missing

Also summarize any remaining reproducibility gaps:
- root modules missing `.terraform.lock.hcl`
- provider constraints with no upper bound
- lint workflows that skip local hooks in CI
- GitHub Actions left on mutable tag references
- deploy/apply workflows that bypass the required lint gate

## Important Notes

- Each step is idempotent — if hooks or config already exist, they are skipped, not duplicated.
- The execution order matters: `setup-git-hooks` must run first as all other skills depend on it.
- `/setup-nix-ci-lint` must run after all hooks are in place because it reads the final hook configuration to generate CI.
- `/setup-github-guardrails` must run after workflows exist so it can normalize the hosted enforcement surface.
- This skill is designed for OpenTofu IaC projects. For application projects with different language tooling, use the individual skills selectively instead.
