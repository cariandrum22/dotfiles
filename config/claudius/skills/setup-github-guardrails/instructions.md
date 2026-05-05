# Setup GitHub Guardrails

Align GitHub-hosted workflow policy with a shared quality profile.

**The argument `$ARGUMENTS` is required** and must be one of:

- `rails-monorepo`
- `tofu-infra`
- `polyglot-oss`

## Profile References

Read the matching quality-profile reference before editing:

- `rails-monorepo`:
  [../setup-quality-profile/references/rails-monorepo.md](../setup-quality-profile/references/rails-monorepo.md)
- `tofu-infra`:
  [../setup-quality-profile/references/tofu-infra.md](../setup-quality-profile/references/tofu-infra.md)
- `polyglot-oss`:
  [../setup-quality-profile/references/polyglot-oss.md](../setup-quality-profile/references/polyglot-oss.md)

Extract:

- workflow naming rules
- job display-name rules
- step naming rules
- stable required-check names
- profile-specific permission expectations

## Steps

### 1. Inspect the current GitHub surface

Inspect:

- `.github/workflows/*.yml`
- `.github/CODEOWNERS`
- pull-request templates
- whether commitlint already enforces PR titles

Do NOT overwrite unrelated workflows wholesale. Normalize them incrementally.

### 2. Normalize workflow names

Use these patterns:

- `CI - <Purpose>` for validation workflows
- `Deploy - <Target>` for delivery workflows
- `Release - <Target>` for publication workflows

Avoid bare names such as `CI`, `Checks`, `Build`, or `Deploy`.

### 3. Normalize job and step names

For jobs that surface as required checks:

- add or keep a stable human-readable `name:`
- avoid names that encode temporary implementation detail

For steps:

- use `Verb + target`
- add path qualifiers in parentheses when the repo has multiple app surfaces
- prefer names that still make sense when viewed in the Actions UI without reading the YAML

### 4. Keep permissions minimal

Default to:

```yaml
permissions:
  contents: read
```

Add broader permissions only when a workflow actually needs them, for example `id-token: write` for
OIDC-based cloud access.

### 5. Preserve the lint policy surface

Ensure the repo-wide lint workflow keeps:

- `fetch-depth: 0`
- commitlint range checks
- PR-title lint when commitlint is present
- stable job names that can be required in branch protection

### 6. Recommend or apply branch protection carefully

If the user explicitly asks for hosted enforcement and `gh` is available and authenticated,
configure branch protection only after listing the exact checks that will be required.

Otherwise, summarize the recommended required checks and leave the hosted settings unchanged.

### 7. Finish cleanly

Summarize:

- which workflow names changed
- which job names changed
- which checks should be required
- whether hosted branch-protection settings were only documented or also applied

## Important Notes

- Workflow and job names are part of the repository contract. Changing them can break
  branch-protection rules and required-check configuration.
- Prefer a few stable workflows over many overlapping ones.
- This skill handles hosted GitHub policy. Use `/setup-local-quality-loop` for local hooks and
  shared lint CI generation.
