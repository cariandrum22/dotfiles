## CI Policy - Common Principles

### Scope

This document defines organization-wide continuous integration requirements that apply to all CI providers. Service-specific policies (GitHub Actions, GitLab CI/CD, etc.) extend this baseline and must not contradict the prescriptions listed here.

### Local Execution Baseline

- Every workflow must support deterministic local execution that mirrors the remote runner environment.
- Projects SHALL provide reproducible container images or developer environments (for example, via Nix Flakes or Dockerfiles) used by both local and remote runs.
- Local runs MUST default to the same environment variables, secrets placeholders, and caching directories as remote pipelines.

### Secrets and Configuration

- Never store secrets in repository files or sample configs committed to source control.
- Provide `.example` variants for required files (`.secrets`, `.env.ci`, etc.) and document injection mechanisms.
- Secrets passed to local runners MUST be supplied via environment variables or secret management tooling; never hardcode values in CI scripts.

### Pipeline Structure

- Define explicit stages: `prepare`, `validate`, `test`, `build`, `security`, `deploy`, and `cleanup`. If a stage is unnecessary, document its omission.
- Fail fast on linting and static analysis before executing integration or deployment steps.
- Enforce branch protections: pipelines MUST execute on merge requests and protected branches before merge.

### Caching and Artifacts

- Cache keys SHALL include lockfiles or manifest checksums to avoid stale dependency reuse.
- Upload coverage, lint, and security scan artifacts for auditability; retain for at least 30 days or your organizational default.
- Artifacts containing secrets must be encrypted at rest or purged before archival.

### Observability and Alerts

- Publish pipeline duration, failure rate, and flaky test statistics to centralized monitoring.
- Gate merges on zero failing tests; flaky tests require documented tracking and remediation.
- Configure notifications to relevant channels (Slack, email, PagerDuty) for pipeline failures, including on manual reruns.

### Provider Extensions

- GitHub Actions: follow `ci-policy-github-actions.md` for runner selection, `act` configuration, and workflow examples.
- GitLab CI/CD: follow `ci-policy-gitlab.md` for runner registration, `.gitlab-ci.yml` templates, and configuration defaults.
- Additional providers must document their deltas relative to this baseline before onboarding.
