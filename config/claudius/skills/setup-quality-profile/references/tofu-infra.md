# OpenTofu Infrastructure Quality Profile

Use this profile for a repository that primarily manages Terraform or OpenTofu code, typically with
environment directories and reusable modules.

## Commit Scope Taxonomy

Recommended `scope-enum` values:

- `ci`
- `deps`
- `docs`
- `envs`
- `modules`
- `nix`
- `scripts`
- `security`
- `tooling`

Keep the list small. Scopes should map to real repository domains, not every single cloud service or
resource name.

## Workflow Naming

Preferred workflow display names:

- `CI - Lint` for repo-wide linting and policy checks
- `Deploy - <Environment>` for environment-specific apply or release workflows
- `Release - Infrastructure` only if the repository versions release artifacts

Avoid generic names such as `CI`, `Deploy`, or `Checks`.

## Job Naming

Prefer stable human-readable job `name:` fields:

- `Lint`
- `Plan`
- `Apply`

If a workflow only has one job, still set a job name explicitly so branch protection checks remain
stable.

## Step Naming

Use `Verb + target` naming and be explicit about infrastructure tooling.

Good examples:

- `Initialize tflint plugins`
- `Run pre-commit hooks`
- `Lint commit messages (push)`
- `Lint PR title`

Avoid generic steps such as `Setup` or `Run checks`.

## Local Hook Naming

Preferred hook names:

- `tofu-format`
- `hclfmt`
- `tflint`
- `tofu-validate-no-backend`
- `tfsec`
- `trivy-config`
- `shellcheck`
- `shfmt`
- `commitlint`
- `commitlint-pre-push`

Keep hook names aligned with the tool name when the repository surface is already singular and
infrastructure-specific.

## Linter and Security Policy

The infrastructure baseline should include:

- Nix linting for the flake
- shellcheck and shfmt
- file hygiene checks
- secrets scanning
- `tofu fmt`
- `hclfmt`
- `tflint`
- `tofu-validate-no-backend`
- `tfsec`
- `trivy config`

## CI Policy

- Use `fetch-depth: 0` in lint workflows.
- Run `tflint --init` before `pre-commit` when `tflint` is enabled.
- Skip `tofu-validate-no-backend` in CI only when provider initialization is not feasible there.
  When skipped, document the reason inline next to `SKIP`.
- Keep commitlint and PR-title lint in the same `CI - Lint` workflow.

## Required Checks

Typical minimum:

- `CI - Lint / Lint`

Add plan or apply checks to branch protection only when they are stable, non-environmental, and
expected for every pull request.
