# Setup Haskell Lint

Add Fourmolu, HLint, and a strict compile check for a Haskell project. This
skill standardizes on check-only formatting, text linting, and a build step
that treats warnings as errors.

**The argument `$ARGUMENTS` is optional** and may provide `build_tool=cabal`,
`build_tool=stack`, or `build_tool=auto` (default).

## Prerequisites

- The project must have Haskell package metadata such as a `.cabal` file,
  `cabal.project`, or `stack.yaml`.
- `fourmolu`, `hlint`, and the chosen build tool must be available.
- For Nix Flake projects: ensure the dev shell exposes `fourmolu`, `hlint`, and
  the chosen build tool.
- For non-Nix projects: tell the user to install the missing tools. Do NOT run
  installation commands automatically.

## Steps

### 1. Determine hook integration method

Check whether `flake.nix` exists in the project root and already contains a
`git-hooks` or `pre-commit-hooks` input.

- If yes: follow Path A (Nix `git-hooks.nix`).
- If no: follow Path B (pre-commit local hooks).

Do NOT apply both paths.

---

### Path A: Nix git-hooks.nix

#### A-1. Add hooks to `flake.nix`

Add the hooks from [hooks.nix.template](assets/hooks.nix.template) into the
`hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate
hooks that already exist.

Hooks added:

- `fourmolu-check` - runs `fourmolu --mode check .`
- `hlint-check` - runs `hlint .`
- `haskell-strict-check` - runs the strict build script

#### A-2. Ensure tooling is in the dev shell

Verify `fourmolu`, `hlint`, and the chosen build tool are available in the dev
shell. Preserve existing package-management patterns if the project already has
one.

---

### Path B: Pre-commit local hooks

#### B-1. Add hooks to `.pre-commit-config.yaml`

If `.pre-commit-config.yaml` does not exist, create it.

Add the hooks from
[pre-commit-hooks.yaml.template](assets/pre-commit-hooks.yaml.template) to the
`repos` list. Do NOT duplicate hooks that already exist.

Hooks added:

- `fourmolu-check` - runs `fourmolu --mode check .`
- `hlint-check` - runs `hlint .`
- `haskell-strict-check` - runs the strict build script

### 2. Create or update `.hlint.yaml`

If `.hlint.yaml` does not exist, create it from
[.hlint.yaml.template](assets/.hlint.yaml.template).

If the file already exists, preserve the current project-specific hints and add
only missing baseline groups:

- `future`
- `generalise`
- `generalise-for-conciseness`

Do NOT overwrite existing ignores or project-specific restrictions.

### 3. Create `scripts/haskell-strict-check.sh`

Create `scripts/haskell-strict-check.sh` from
[haskell-strict-check.sh.template](assets/haskell-strict-check.sh.template) and
make it executable.

The script:

- auto-detects `cabal` or `stack` unless overridden
- applies strict GHC warning flags
- treats warnings as errors at hook and CI time

### 4. Inspect persistent warning policy

Inspect `.cabal`, `cabal.project`, `package.yaml`, or `stack.yaml`.

If the repository already persists strict warning flags in package metadata,
preserve them.

If it does not, inform the user that the hook and CI surface still enforce a
strict build and recommend persisting the warning policy in project metadata
later if they want local `build` commands to match the same strictness.

### 5. Validate

Run the following commands, or tell the user to run them:

1. `fourmolu --mode check .`
2. `hlint .`
3. `./scripts/haskell-strict-check.sh`

If formatting fails, tell the user to run `fourmolu -i` manually and review the
diff.

If HLint or the strict build fails, fix the code or narrow project-specific
exceptions deliberately. Do NOT add broad ignore rules just to silence the
baseline.

## Important Notes

- Do NOT use `fourmolu -i` or `hlint --refactor` inside hooks or CI.
- Prefer Fourmolu defaults. Do NOT create `fourmolu.yaml` unless the repository
  has a documented reason to diverge.
- The strict build script is meant to expose warning debt. If a generated or
  vendored subtree is noisy, narrow the scope of the build target rather than
  weakening the global warning set.
