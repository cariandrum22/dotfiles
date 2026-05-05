# Setup commitlint

Set up commitlint with robust Conventional Commits rules, local git hooks, and
CI enforcement.

**The argument `$ARGUMENTS` is optional** and may provide a comma-separated list
of desired scopes for this project (e.g.
`api,web,ci,deps,docs,nix,scripts,tooling`).

If `$ARGUMENTS` is omitted, infer and optimize the scope taxonomy from the
existing repository structure and any existing `commitlint.config.cjs`.

Read
[references/scope-taxonomy.md](references/scope-taxonomy.md)
before deriving the final scope list.

## Prerequisites

- The project must have `git-hooks.nix` integrated in `flake.nix` (with
  `mkHook`/`mkHookWithStages` helpers available). If not, tell the user to run
  the `setup-git-hooks` skill first.
- `commitlint` package must be available in the dev shell. If not already
  present, add `pkgs.commitlint` to `devShells.default.packages`.

## Steps

### 1. Determine the optimized scope taxonomy

Build the final scope list before writing any files.

If `commitlint.config.cjs` already exists, read its current `scope-enum` first
and treat it as the continuity baseline.

Then inspect the repository for stable collaboration surfaces, including:

- top-level directories such as `apps/`, `packages/`, `crates/`, `modules/`,
  `envs/`, `docs/`, `scripts/`, `infra/`, `tests/`
- workspace manifests and package boundaries
- workflow names and job structure under `.github/workflows/`
- release, security, verification, or compliance subtrees that represent real
  owned domains

Derive scopes using these rules:

- prefer stable logical domains, not every leaf directory
- prefer lower-case hyphenated names
- preserve existing useful scopes to avoid unnecessary churn
- remove stale scopes that no longer map to real repository areas
- merge over-granular scopes into a broader domain when individual areas are
  not independently owned or reviewed
- keep canonical shared scopes when the repository clearly has those concerns:
  `ci`, `deps`, `docs`, `docker`, `nix`, `release`, `scripts`, `security`,
  `tooling`, `config`, `tests`, `infra`
- add domain scopes only when they are durable repo surfaces, such as `api`,
  `web`, `server`, `client`, `addon`, `ffi`, `jose`, `verification`, `fstar`,
  `tamarin`, or `kani`

If `$ARGUMENTS` is provided, treat it as the desired baseline, then:

- normalize spelling and duplicates
- warn or prune clearly stale scopes
- add missing shared scopes only when the repository structure makes the omission
  clearly harmful

Prefer a lean taxonomy. A good scope list is broad enough to remain stable over
time and narrow enough to help routing, review, and release notes.

### 2. Create or update `commitlint.config.cjs`

Create or update the file in the project root using the template in
[commitlint.config.cjs.template](assets/commitlint.config.cjs.template).
Replace `__SCOPES__` with the optimized scopes formatted as JavaScript array
entries.

If the file already exists:

- preserve the current non-scope rules unless they are weaker than the template
- update only the scope list and any clearly missing baseline rules
- preserve scope ordering where possible to reduce needless diff churn

### 3. Create `scripts/commitlint-pre-push.sh`

Create the file using the template in
[commitlint-pre-push.sh.template](assets/commitlint-pre-push.sh.template). Make
it executable (`chmod +x`).

Create the `scripts/` directory if it does not exist.

### 4. Add commitlint hooks to `flake.nix`

Add the following two hooks inside the `hooks = { ... }` block in `flake.nix`:

```nix
commitlint = mkHookWithStages "commitlint" "bash -c 'commitlint --edit'" [ "commit-msg" ];
commitlint-pre-push =
  mkHookWithStages "commitlint-pre-push" "bash -c './scripts/commitlint-pre-push.sh'"
    [ "pre-push" ];
```

Do NOT duplicate if they already exist. If they exist in a different style
(e.g. inline script), replace them with the above.

### 5. Add CI workflow steps

If `.github/workflows/lint.yml` exists, add the commit message linting steps
below. If a different lint workflow exists, add them there. If no lint workflow
exists, create `.github/workflows/lint.yml`.

Add these three steps **after** the "Run pre-commit hooks" step (or at the end
of the job if no such step exists). Use the template in
[ci-steps.yml.template](assets/ci-steps.yml.template).

Do NOT duplicate if commit lint steps already exist. If they exist but use a
simpler strategy (e.g. `HEAD~1`), replace them with the merge-base approach
from the template.

## Important Notes

- Scopes are project-specific and should reflect stable repository domains, not
  transient implementation details.
- The `scope-empty` rule is set to `[0]` (disabled) so that scopes are
  optional.
- Do NOT modify any existing CI steps; only add the commit lint steps.
- Prefer continuity over churn. If a repository already has a good scope
  taxonomy, optimize it incrementally rather than renaming everything at once.
- Prefer omitting a low-signal scope over adding a noisy or unstable one.
