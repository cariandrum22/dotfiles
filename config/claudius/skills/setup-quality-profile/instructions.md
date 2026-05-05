# Setup Quality Profile

Apply a shared quality profile by composing the existing repository-baseline, local-quality-loop,
and GitHub-guardrails layers with profile-specific stack guidance.

**The argument `$ARGUMENTS` is required** and must begin with one of:

- `rails-monorepo`
- `tofu-infra`
- `polyglot-oss`

An optional suffix may provide `<primary-owner>[,<security-contact>]`, for example:

- `rails-monorepo,@example/platform,@example/security`
- `tofu-infra,@example/infrastructure`
- `polyglot-oss,@example/maintainers,@example/security`

## Profile References

Read the matching profile reference before changing anything:

- `rails-monorepo`: [references/rails-monorepo.md](references/rails-monorepo.md)
- `tofu-infra`: [references/tofu-infra.md](references/tofu-infra.md)
- `polyglot-oss`: [references/polyglot-oss.md](references/polyglot-oss.md)

Extract these values from the reference:

- commitlint scope taxonomy
- workflow, job, and step naming conventions
- stable required check names
- stack-specific linting expectations
- acceptable CI skip rules

## Steps

### 1. Confirm the profile fits the repository

Inspect the repository layout before editing.

Use `rails-monorepo` when the repository has a Rails API plus other app or package surfaces under a
shared root.

Use `tofu-infra` when the repository primarily manages Terraform or OpenTofu code, typically with
`envs/` and `modules/` or similar directories.

Use `polyglot-oss` when the repository mixes one or more general-purpose
language surfaces such as Rust, Haskell, Lean, Python, Ruby, or TypeScript and
does not clearly fit the Rails-monorepo or OpenTofu-infra archetypes.

If the repository does not clearly fit any profile, stop and tell the user
which assumptions failed.

### 2. Apply the repository policy baseline

Run `/setup-repository-baseline` first.

Pass through any inferred or user-provided owner and security-contact values so the baseline
documents, `CODEOWNERS`, and templates match the profile rollout.

### 3. Apply the shared local quality loop

Run `/setup-local-quality-loop <profile>`.

This establishes the shared Nix hook foundation, commitlint, repo-wide lint CI, and the stable
naming surface for the core `CI - Lint` workflow.

### 4. Add profile-specific stack guardrails

#### `rails-monorepo`

Run `/setup-rails-api`.

Then run `/setup-typed-eslint-monorepo` for the frontend and package surfaces that need more than
Biome.

If the repo also has plain TypeScript packages without framework-specific linting needs, use
`/setup-ts-typecheck` and optionally `/setup-biome` selectively for those simpler surfaces.

#### `tofu-infra`

The shared local quality loop already adds the core IaC hook pack for this profile.

After it finishes, inspect for any repository-specific release or deployment scripts and keep their
names aligned with the profile reference.

#### `polyglot-oss`

Inspect the repository before applying language skills. Do NOT blindly apply
every language-specific skill.

Apply the matching skills only for real surfaces:

- Rust (`Cargo.toml`, `crates/`, or a Cargo workspace):
  `/setup-rust-lint`
- Haskell (`.cabal`, `cabal.project`, `stack.yaml`):
  `/setup-haskell-lint`
- Lean (`lean-toolchain` and `lakefile.lean` or `lakefile.toml`):
  `/setup-lean-lint`
- Python (`pyproject.toml`, `.python-version`, or clear Python package surface):
  `/setup-python-lint`
- Generic Ruby (`Gemfile` or `gems.rb`, outside Rails-monorepo scope):
  `/setup-ruby-lint`
- TypeScript:
  use `/setup-typed-eslint-monorepo` for framework-heavy, browser-extension,
  GraphQL, security-sensitive, or functional-rule-heavy surfaces; otherwise use
  `/setup-ts-typecheck` and optionally `/setup-biome`

If the Ruby surface also uses Sorbet, layer `/setup-sorbet` after
`/setup-ruby-lint`.

If the repository actually contains a Rails API plus additional app surfaces
under one root, stop and switch to `rails-monorepo` instead of partially
simulating it under `polyglot-oss`.

### 5. Align the GitHub-hosted guardrails

Run `/setup-github-guardrails <profile>`.

This keeps workflow names, check names, PR-title enforcement, and optional branch-protection
recommendations aligned with the same profile reference.

### 6. Finish cleanly

Summarize:

- which component skills were applied
- which commitlint scopes were used
- which workflow and job names were created or normalized
- which checks should be required in branch protection
- any manual follow-up still needed for stack-specific linting

## Important Notes

- Profiles are opinionated starting points, not blind scaffolding.
- Stable workflow and job names are part of the repository API because branch protection and merge
  policy depend on them.
- Keep commit scopes aligned to the repository taxonomy. Remove stale scopes when a domain does not
  exist in the target repository.
- Prefer a shared repo-wide lint workflow plus a small number of stack-specific workflows over many
  overlapping checks.
