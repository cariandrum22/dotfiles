---
name: setup-just-lint
description: Set up justfile formatting check and syntax validation hooks. Use when adding justfile quality checks to a project.
disable-model-invocation: true
argument-hint: (no arguments required)
---

# Setup Just Lint

Add justfile formatting validation and syntax checking to the project's pre-commit hooks.

As of 2026-02, the justfile ecosystem provides **no third-party linters**. This skill uses the two built-in verification commands:

- `just --fmt --check --unstable` — formatting conformance (check-only, no auto-fix)
- `just --evaluate` — variable resolution and recipe parse validation

## Prerequisites

- `just` (>= 1.0.0) must be available in the environment.
- For Nix Flake projects: the project must have `git-hooks.nix` integrated in `flake.nix` (with `mkHook`/`mkHookWithStages` helpers available). If not, tell the user to run `/setup-git-hooks` first.
- For non-Nix projects: `pre-commit` must be installed.

## Steps

### 1. Determine hook integration method

Check whether `flake.nix` exists in the project root **and** contains a `git-hooks` (or `pre-commit-hooks`) input.

- If **yes**: follow **Path A** (Nix git-hooks.nix).
- If **no**: follow **Path B** (pre-commit local hooks).

Do NOT apply both paths. Choose exactly one.

---

### Path A: Nix git-hooks.nix

#### A-1. Add hooks to `flake.nix`

Add the hooks from [hooks.nix.template](hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate hooks that already exist — if a hook already exists, skip it.

Hooks added:

- **just-fmt-check** — Validates that the justfile conforms to the canonical format produced by `just --fmt`. Check-only; does not modify files.
- **just-evaluate** — Validates that all variables, recipes, and expressions in the justfile parse and resolve correctly.

#### A-2. Ensure `just` is in devShell packages

Verify that `pkgs.just` is listed in `devShells.default.packages` (or `buildInputs`). If not, add `pkgs.just`. The hooks cannot run without it.

---

### Path B: Pre-commit local hooks

#### B-1. Add hooks to `.pre-commit-config.yaml`

If `.pre-commit-config.yaml` does not exist, create it.

Add the hooks from [pre-commit-hooks.yaml.template](pre-commit-hooks.yaml.template) to the `repos` list. Do NOT duplicate hooks that already exist — if hooks with IDs `just-fmt-check` or `just-evaluate` already exist, skip them.

Hooks added:

- **just-fmt-check** — `just --fmt --check --unstable`, `pass_filenames: false`, triggered by `justfile` or `*.just` files.
- **just-evaluate** — `just --evaluate`, `pass_filenames: false`, triggered by `justfile` or `*.just` files.

#### B-2. Remove abandoned third-party hooks

If `.pre-commit-config.yaml` contains a reference to `instrumentl/pre-commit-just`, remove it. This repository has been abandoned since January 2024 and is no longer maintained. Replace with the local hooks from B-1.

---

### 2. Add justfile meta recipes (both paths)

If the project's `justfile` does not already contain formatting and validation recipes, add the following block **after** the mandatory recipes section (below the `# Mandatory Recipes` marker if present, otherwise at the end):

```just
# =========================================================================
# Justfile Quality
# =========================================================================

# Check justfile formatting (check-only, does not modify)
meta-check:
    @just --fmt --check --unstable

# Format this justfile in place
meta-format:
    @just --fmt --unstable

# Validate justfile syntax
meta-evaluate:
    @just --evaluate
```

If any of these recipe names already exist, skip them. Do NOT rename or modify existing recipes.

### 3. Validate

Run `just --fmt --check --unstable` to confirm the justfile passes the formatting check. If it fails:

- Do NOT auto-fix with `just --fmt --unstable` — inform the user of the failure and let them decide. Auto-formatting has known bugs that can break `else if` blocks and strip comments.
- Show the user the diff output so they can review and fix manually.

## Known Limitations

As of `just` v1.46.0 (2026-01):

| Issue | Impact | Mitigation |
|---|---|---|
| `--fmt` breaks `else if` statements ([#2569](https://github.com/casey/just/issues/2569)) | Auto-format can produce invalid justfiles | This skill uses check-only mode; no auto-fix in hooks |
| `--fmt` strips non-doc comments before recipes ([#2695](https://github.com/casey/just/issues/2695)) | Comments may be lost on format | Users must review `meta-format` output before committing |
| `--fmt` requires `--unstable` flag | May change behavior in future `just` releases | Pin `just` version in `flake.nix` or CI if stability is critical |
| No configurable formatting rules | Cannot adjust indent size, line width, etc. | Accept the canonical style as-is |

## Important Notes

- Do NOT remove or modify any existing hooks.
- Do NOT use `instrumentl/pre-commit-just` — it is abandoned and unmaintained.
- Do NOT enable auto-fix (`just --fmt --unstable` without `--check`) in pre-commit hooks or CI. The known bugs make auto-formatting destructive in edge cases. Provide `meta-format` as an explicit, user-initiated recipe only.
- The `--unstable` flag is required for all `--fmt` operations. This is a `just` upstream constraint, not a policy choice.
- The `meta-format` recipe is provided for convenience but carries risk. Document the known bugs in the project's contributing guide if this recipe is used.
- If the project uses modular justfiles (`mod` imports with `*.just` files), `just --fmt` can only format the root justfile. Imported files with undefined variables will fail ([#1824](https://github.com/casey/just/issues/1824)). Warn the user if modular imports are detected.
