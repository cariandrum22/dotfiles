# Skills Refactor Roadmap

This roadmap turns the current skills collection into a more natural structure
for modern OSS teams that want shared repository guardrails first and local
tooling second.

Status: `setup-repository-baseline` is now the first implemented primary skill.
The remaining items below are still open.

## Target shape

### 1. Repository governance first

Introduce a primary skill such as `setup-repository-baseline` that establishes:

- `CONTRIBUTING.md`
- `SECURITY.md`
- `CODEOWNERS`
- PR / issue templates
- merge strategy
- release and changelog policy
- source-of-truth guidance for commit / PR / release conventions

This should be the first skill users discover when they want to bootstrap a new
repository for collaborative development.

### 2. Hosted enforcement second

Introduce `setup-github-guardrails` for hosted or CI-backed enforcement:

- GitHub Actions baseline
- PR-title lint
- changelog / release workflow scaffolding
- optional commitlint in CI
- alignment with branch protection assumptions

The main policy should live here and in repository documents, not only in local
hooks.

### 3. Local tooling as an adapter layer

Introduce `setup-local-quality-loop` that chooses one of these paths:

- keep existing local hooks as-is
- adopt `pre-commit`
- adopt `git-hooks.nix`
- adopt `lefthook`
- skip local hooks

Current skills such as `setup-git-hooks`, `setup-commitlint`,
`setup-file-hygiene`, `setup-shell-lint`, and `setup-secrets-scan` should
become building blocks behind this adapter instead of the first user-facing
entry points.

### 4. Optional Nix workspace

Keep Nix explicit and optional through a dedicated skill such as
`setup-nix-workspace`.

This skill can compose:

- `setup-nix-flake`
- `setup-git-hooks`
- `setup-nix-lint`
- `setup-nix-ci-lint`

but it should not define the repository's policy surface.

### 5. Domain baselines stay, but compose shared baselines

Existing orchestrators should survive, but change responsibilities:

- `setup-rails-api` should evolve toward `setup-rails-project`
- `setup-tofu-project` should remain primary

Both should compose:

1. repository baseline
2. GitHub guardrails
3. local quality loop selection
4. language / framework-specific quality gates

## Near-term changes

1. Replace or remove `gemini-web-search`
   Use native web search or MCP-backed search instead of a CLI transport skill.

2. Demote Git and hook implementation details
   Treat `setup-commitlint`, `setup-git-hooks`, `setup-file-hygiene`,
   `setup-secrets-scan`, and `setup-nix-ci-lint` as components rather than
   primary entry points.

3. Keep strong language-specific building blocks
   Preserve `setup-biome`, `setup-ts-typecheck`, `setup-python-lint`,
   `setup-c-lint`, and `setup-sql-lint` as directly useful standalone skills.

4. Narrow the top-level catalog
   After new primary skills exist, reduce discoverability of component skills
   that are mostly implementation details.

## Ordering for the actual refactor

1. Create new primary skills without deleting the current ones.
2. Rewire orchestrators to call the new primary shared baselines.
3. Reclassify old implementation-focused skills as secondary or internal.
4. Remove replaced workflow workarounds like `gemini-web-search`.
