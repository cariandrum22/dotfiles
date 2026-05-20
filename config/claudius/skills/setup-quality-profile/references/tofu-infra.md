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

Provider and module hygiene:

- Track `.terraform.lock.hcl` for every root module that is validated or applied.
- Use bounded provider constraints such as `~> 6.30.0` or `>= 6.30.0, < 7.0.0`; avoid bare
  `>=` constraints in mature infrastructure repositories.
- Keep OpenTofu/Terraform itself exactly pinned through the Nix flake or equivalent toolchain lock.
- Make validation backend-free and credential-free where possible; providers should not contact
  real cloud APIs during static validation.
- Prefer root-module layout that lets CI discover and validate every independently applied unit.

## CI Policy

- Use `fetch-depth: 0` in lint workflows.
- Run `tflint --init` inside the project dev shell before `pre-commit` when `tflint` is enabled
  (for example `nix develop -c tflint --init`).
- The lint workflow should run the same hook set as the local quality loop. Do not skip
  `tofu-validate-no-backend` for pure IaC repositories unless a concrete external dependency
  remains after attempting backend-free validation. When skipped, document the reason inline next
  to `SKIP`.
- Run `pre-commit` with `--show-diff-on-failure` so formatting drift is visible in CI logs.
- Keep commitlint and PR-title lint in the same `CI - Lint` workflow.
- Use minimal workflow permissions. Repo-wide lint normally needs only `contents: read`.
- Pin GitHub Actions by full commit SHA in mature infrastructure repositories. Keep the intended
  semantic tag (`v4`, `v3`, etc.) in a comment and re-resolve it explicitly during upgrades.

## Delivery Policy

- Separate "update deployment manifest", "validate CI", and "apply infrastructure" into distinct
  gates when the repository is used by customers or MVP environments.
- If a workflow both commits desired-state changes and applies them, document the residual risk and
  ensure the workflow cannot bypass the same lint and validation checks expected for pull requests.
- Apply jobs that assume cloud roles need explicit `id-token: write`; validation jobs should not
  inherit that permission.

## Required Checks

Typical minimum:

- `CI - Lint / Lint`

Add plan or apply checks to branch protection only when they are stable, non-environmental, and
expected for every pull request.
