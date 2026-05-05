# Setup Local Quality Loop

Establish a fast local-and-CI feedback loop for a Nix-based repository without inventing a new
project-specific policy surface.

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

- the allowed commitlint scopes
- the expected `CI - Lint` naming surface
- the preferred hook names
- any profile-specific CI initialization or skip rules

## Steps

### 1. Establish the shared Nix hook foundation

Apply these component skills in order:

1. `/setup-git-hooks`
2. `/setup-nix-lint`
3. `/setup-shell-lint`
4. `/setup-file-hygiene`
5. `/setup-secrets-scan`

If the repository does not contain a root `flake.nix`, stop and tell the user that this skill
expects a Nix-flake project.

### 2. Add profile-specific local hook packs

#### `rails-monorepo`

The shared foundation is the mandatory baseline.

Do NOT force `/setup-biome` automatically. Frontend or extension surfaces that need Vue, GraphQL,
browser-extension, security, or functional linting should be handled by
`/setup-typed-eslint-monorepo` and should keep domain-oriented root hook names as described in the
profile reference.

#### `tofu-infra`

Add these component skills before generating CI:

1. `/setup-tofu-lint`
2. `/setup-iac-security`

#### `polyglot-oss`

The shared foundation is the mandatory baseline.

Do NOT force language-specific hook packs from this skill. For polyglot
repositories, apply the language skills later from `/setup-quality-profile`
after scanning the repository surface:

- `/setup-rust-lint`
- `/setup-haskell-lint`
- `/setup-lean-lint`
- `/setup-python-lint`
- `/setup-ruby-lint`
- `/setup-ts-typecheck`
- `/setup-biome`
- `/setup-typed-eslint-monorepo`

When normalizing hook names, prefer the stable language-surface names from the
profile reference unless the repository already has a stronger domain-oriented
surface.

### 3. Add commitlint with the profile scopes

For `rails-monorepo` and `tofu-infra`, run `/setup-commitlint` with the exact
scope list from the profile reference.

For `polyglot-oss`, run `/setup-commitlint` without explicit scopes so it can
infer the repository taxonomy, then preserve the shared baseline scopes from
the profile reference and trim anything that does not map to a real durable
domain in the target repository.

### 4. Generate the repo-wide lint workflow

Run `/setup-nix-ci-lint` after all hooks are in place.

### 5. Normalize the naming surface

After generation, ensure the local and CI surface matches the profile:

- repo-wide lint workflow name is `CI - Lint`
- required jobs have stable human-readable `name:` values
- commitlint and PR-title lint run after `pre-commit`
- dependency installation happens before `pre-commit`
- `fetch-depth: 0` is preserved
- `SKIP` is used only for profile-approved hooks and documented inline

### 6. Finish cleanly

Summarize:

- which hooks were added
- which commit scopes were used
- whether any hook names were normalized
- which CI steps still need repository-specific commands

## Important Notes

- This skill establishes the shared feedback loop. Stack-specific application or platform skills
  should be layered on top by `/setup-quality-profile`.
- Keep root hook names stable even if the underlying command changes.
- Do NOT silently drop commitlint or PR-title lint from the lint workflow.
