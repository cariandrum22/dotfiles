# Setup Repository Baseline

Establish the source-of-truth repository policy surface for a modern
collaborative OSS repository before adding hosted enforcement or local hook
automation.

**The argument `$ARGUMENTS` is optional** and may provide
`<primary-owner>[,<security-contact>]`.

If `$ARGUMENTS` is omitted:

- infer the repository name from the current directory or git remote
- infer the default branch from `origin/HEAD` when possible, otherwise assume
  `main`
- infer the primary owner from the GitHub remote when possible
- ask the user only for values that cannot be inferred safely

Do NOT invent email addresses, team handles, or support URLs. If
`security-contact` cannot be inferred from existing repository metadata, ask the
user before creating `SECURITY.md`.

## Goal

Create or fill gaps in the following baseline files:

- `CONTRIBUTING.md`
- `SECURITY.md`
- `CHANGELOG.md`
- `docs/runbooks/release.md`
- `.github/CODEOWNERS`
- `.github/pull_request_template.md`
- `.github/ISSUE_TEMPLATE/bug_report.yml`
- `.github/ISSUE_TEMPLATE/feature_request.yml`

## Principles

- Repository documents are the primary source of truth.
- Hosted enforcement belongs in `/setup-github-guardrails`, not here.
- Local hooks and developer feedback loops belong in `/setup-local-quality-loop`,
  not here.
- Use `/setup-quality-profile` when bootstrapping a repository to the shared
  house style end to end.
- Keep policy concise, contributor-readable, and stack-agnostic.
- Preserve existing project-specific content. Merge missing sections instead of
  overwriting files wholesale.

## Steps

### 1. Inspect repository state

Determine these values before editing:

- repository name
- default branch
- primary owner for `CODEOWNERS`
- security contact for `SECURITY.md`

Inspect existing files first. If any of the target files already exist, preserve
project-specific details and only fill gaps.

Prefer these inference paths:

1. existing repository files
2. git remote metadata
3. current directory name
4. user confirmation

For `CODEOWNERS`, a GitHub team handle is preferred for organization-owned
repositories. If only a single maintainer handle is known, use that as the
fallback owner.

### 2. Create or update `CONTRIBUTING.md`

If `CONTRIBUTING.md` does not exist, create it from
[CONTRIBUTING.md.template](assets/CONTRIBUTING.md.template).

If it already exists, ensure it covers these baseline policies:

- how to propose changes and discuss larger changes early
- branch-from-default-branch workflow
- preference for small, reviewable pull requests
- expectation to update tests, docs, and changelog when behavior changes
- pull request title guidance as the canonical change summary
- review etiquette and self-review expectations

Do NOT hard-code language-specific commands or local tooling instructions here.
The file should remain valid whether the repository uses Nix, pre-commit,
lefthook, plain scripts, or no local automation.

### 3. Create or update `SECURITY.md`

If `SECURITY.md` does not exist, create it from
[SECURITY.md.template](assets/SECURITY.md.template).

If it already exists, ensure it includes:

- a clear non-public reporting path
- supported versions or branches
- requested report contents
- coordinated disclosure expectations

Default policy for a new repository:

- the default branch is supported
- older releases are unsupported unless the repository already maintains them

Do NOT leave placeholder contact values in the final file.

### 4. Create or update `CHANGELOG.md`

If `CHANGELOG.md` does not exist, create it from
[CHANGELOG.md.template](assets/CHANGELOG.md.template).

If it already exists, ensure there is an `Unreleased` section and that the file
is appropriate for contributor-facing release notes.

Prefer Keep a Changelog structure unless the repository already has another
clear changelog convention.

### 5. Create or update the release runbook

If `docs/runbooks/release.md` does not exist, create it from
[release.md.template](assets/release.md.template).

If it already exists, ensure it covers:

- release preconditions
- changelog and version preparation
- signed annotated tag creation
- publishing a GitHub release
- post-release verification and rollback thinking

Keep the runbook process-oriented. Do NOT embed CI implementation details or
service-specific deployment steps unless the repository already requires them.

### 6. Create or update GitHub collaboration templates

#### `.github/CODEOWNERS`

If the file does not exist, create it from
[CODEOWNERS.template](assets/CODEOWNERS.template).

If it exists:

- preserve granular path ownership rules
- ensure there is a fallback owner entry for `*`
- avoid removing stricter existing ownership patterns

#### `.github/pull_request_template.md`

If the file does not exist, create it from
[pull_request_template.md.template](assets/pull_request_template.md.template).

If it exists, ensure it asks for:

- summary
- verification
- risk or rollout notes
- documentation or changelog updates

#### Issue templates

Create these only when equivalent files do not already exist:

- `.github/ISSUE_TEMPLATE/bug_report.yml` from
  [bug_report.yml.template](assets/bug_report.yml.template)
- `.github/ISSUE_TEMPLATE/feature_request.yml` from
  [feature_request.yml.template](assets/feature_request.yml.template)

If the repository already has issue forms, merge missing fields rather than
duplicating templates under different names.

Do NOT create `.github/ISSUE_TEMPLATE/config.yml` unless the repository already
uses one or the user provides the required contact links and issue-routing
policy.

### 7. Finish cleanly

Summarize:

- which files were created
- which files were updated
- which values were inferred
- which follow-up steps remain for hosted enforcement or local tooling

If the repository still lacks CI, branch protection, PR-title lint, release
automation, or local hooks, recommend `/setup-quality-profile` for a one-shot
bootstrap or `/setup-github-guardrails` and `/setup-local-quality-loop`
separately when the user wants finer control.

## Important Notes

- This skill defines policy and contributor workflow, not enforcement.
- Prefer PR title policy and changelog discipline over heavy commit-local rules
  when the repository uses squash merge.
- Keep documents short and durable. Avoid stack-specific implementation details.
- This skill must be idempotent. Running it multiple times must not create
  duplicate templates or wipe existing repository-specific guidance.
