---
name: setup-nix-lint
description: Set up Nix lint hooks (nixfmt, statix, deadnix) and nix fmt formatter in a Nix flake project. Use when adding Nix-specific linting to a project.
disable-model-invocation: true
argument-hint: (no arguments required)
---

# Setup Nix Lint & Formatter

Add Nix-specific lint hooks and `nix fmt` formatter output to the project's `flake.nix`.

## Prerequisites

- The project must have `git-hooks.nix` integrated in `flake.nix` (with `mkHook`/`mkHookWithStages` helpers available). If not, tell the user to run `/setup-git-hooks` first.

## Steps

### 1. Add Nix lint hooks to `flake.nix`

Add the hooks from [hooks.nix.template](hooks.nix.template) into the `hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate hooks that already exist — if a hook already exists, skip it.

Hooks added:

- **nixfmt** — Nix code formatter
- **statix** — Nix anti-pattern linter
- **deadnix** — Dead code detection for Nix (`noLambdaArg`, `noLambdaPatternNames`, `noUnderscore`)

### 2. Add the `formatter` output

Add the formatter output from [formatter.nix.template](formatter.nix.template) to the flake outputs (sibling of `checks` and `devShells`). Skip if a `formatter` output already exists.

This provides `nix fmt` support via a wrapper that:
- Formats specific files when arguments are given
- Formats all git-tracked `.nix` files when no arguments are given

## Important Notes

- Do NOT remove or modify any existing hooks.
- These are Nix-specific tools only. Other linters (shellcheck, editorconfig, typos, gitleaks, etc.) belong in separate skills.
