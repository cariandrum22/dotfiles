# Setup commitlint

Set up commitlint with robust Conventional Commits rules, local git hooks, and
CI enforcement.

**The argument `$ARGUMENTS` is a comma-separated list of allowed scopes** for
this project (e.g. `api,web,ci,deps,docs,nix,scripts,tooling`). If no argument
is given, ask the user for the list of scopes.

## Prerequisites

- The project must have `git-hooks.nix` integrated in `flake.nix` (with
  `mkHook`/`mkHookWithStages` helpers available). If not, tell the user to run
  the `setup-git-hooks` skill first.
- `commitlint` package must be available in the dev shell. If not already
  present, add `pkgs.commitlint` to `devShells.default.packages`.

## Steps

### 1. Create `commitlint.config.cjs`

Create the file in the project root using the template in
[commitlint.config.cjs.template](assets/commitlint.config.cjs.template).
Replace `__SCOPES__` with the scopes from the argument formatted as the
JavaScript array entries.

### 2. Create `scripts/commitlint-pre-push.sh`

Create the file using the template in
[commitlint-pre-push.sh.template](assets/commitlint-pre-push.sh.template). Make
it executable (`chmod +x`).

Create the `scripts/` directory if it does not exist.

### 3. Add commitlint hooks to `flake.nix`

Add the following two hooks inside the `hooks = { ... }` block in `flake.nix`:

```nix
commitlint = mkHookWithStages "commitlint" "bash -c 'commitlint --edit'" [ "commit-msg" ];
commitlint-pre-push =
  mkHookWithStages "commitlint-pre-push" "bash -c './scripts/commitlint-pre-push.sh'"
    [ "pre-push" ];
```

Do NOT duplicate if they already exist. If they exist in a different style
(e.g. inline script), replace them with the above.

### 4. Add CI workflow steps

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

- Scopes are project-specific and reflect the project's directory structure or
  logical areas.
- The `scope-empty` rule is set to `[0]` (disabled) so that scopes are
  optional.
- Do NOT modify any existing CI steps; only add the commit lint steps.
