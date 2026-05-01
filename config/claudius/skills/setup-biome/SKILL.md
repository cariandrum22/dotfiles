---
name: setup-biome
description: Set up Biome for JavaScript/TypeScript/JSX/TSX linting and formatting with strict rules. Use when adding code quality checks to a JS/TS project.
disable-model-invocation: true
argument-hint: (no arguments required)
---

# Setup Biome

Add Biome as the unified linter and formatter for JavaScript, TypeScript, JSX, TSX, JSON, and CSS files.

Biome replaces both ESLint and Prettier with a single tool. This skill configures the **strictest practical ruleset** suitable for production projects.

## Prerequisites

- The project must have a `package.json` in the root.
- For Nix Flake projects: the project must have `git-hooks.nix` integrated in `flake.nix` (with `mkHook`/`mkHookWithStages` helpers available). If not, tell the user to run `/setup-git-hooks` first.
- For non-Nix projects: `pre-commit` must be installed, or the user must handle hook integration manually.

## Steps

### 1. Install Biome

#### Nix Flake projects

Verify that `pkgs.biome` is listed in `devShells.default.packages` (or `buildInputs`). If not, add it.

#### Non-Nix projects

Check `package.json` for `@biomejs/biome` in `devDependencies`. If absent, tell the user to run:

```bash
npm install --save-dev --save-exact @biomejs/biome
```

The `--save-exact` flag is required. Biome follows a policy of introducing breaking changes in minor releases; pinning the exact version prevents unexpected breakage.

Do NOT run this command automatically — the user must choose their package manager.

### 2. Create `biome.json`

If `biome.json` (or `biome.jsonc`) already exists, go to Step 3.

Create `biome.json` in the project root from [biome.json.template](biome.json.template). Replace `__BIOME_VERSION__` with the installed Biome version (e.g. `2.3.14`). Determine the version from:

- Nix: `biome --version` in the dev shell
- npm: the version in `package.json` `devDependencies`

If the version cannot be determined, omit the `$schema` field entirely rather than guessing.

### 3. Verify strict configuration

If `biome.json` already existed, verify the following settings are present. If any are missing or weaker, **do NOT overwrite** — inform the user of the discrepancies and let them decide.

Required settings:

| Setting | Required Value | Rationale |
|---|---|---|
| `linter.enabled` | `true` | Must not be disabled |
| `formatter.enabled` | `true` | Must not be disabled |
| `organizeImports.enabled` | `true` | Import ordering must be enforced |
| `formatter.lineEnding` | `"lf"` | Unix line endings only |
| `vcs.enabled` | `true` | Respect `.gitignore` |
| `linter.rules.recommended` | `true` | Baseline rule coverage |
| `correctness.noUnusedImports` | `"error"` | Dead imports must not pass |
| `correctness.noUnusedVariables` | `"error"` | Dead variables must not pass |

### 4. Add `package.json` scripts

Add the following scripts to `package.json` if they do not already exist. Do NOT overwrite existing scripts with the same names.

```json
{
  "scripts": {
    "lint": "biome ci",
    "lint:fix": "biome check --write",
    "format": "biome format --write",
    "format:check": "biome format"
  }
}
```

- `lint` uses `biome ci` (check-only, proper exit codes for CI).
- `lint:fix` uses `biome check --write` (applies all safe fixes).
- `format` uses `biome format --write` (format only, no lint fixes).
- `format:check` uses `biome format` (check-only).

### 5. Add pre-commit hooks

Determine the hook integration method (same logic as `/setup-just-lint` Step 1).

#### Path A: Nix git-hooks.nix

Add the hook from [hooks.nix.template](hooks.nix.template) into the `hooks = { ... }` block. Do NOT duplicate if it already exists.

Hook added:

- **biome-ci** — Runs `biome ci` (lint + format check, no auto-fix). Fails on any violation.

#### Path B: Pre-commit local hooks

Add the hook from [pre-commit-hooks.yaml.template](pre-commit-hooks.yaml.template) to `.pre-commit-config.yaml`. Do NOT duplicate if a `biome-ci` hook already exists.

### 6. Remove conflicting tools (conditional)

If the project has **both** Biome and any of the following, warn the user about the conflict. Do NOT remove anything automatically.

| Conflicting tool | Detection | Recommendation |
|---|---|---|
| ESLint | `.eslintrc*`, `eslint.config.*`, `eslint` in `devDependencies` | Migrate rules to Biome, then remove ESLint |
| Prettier | `.prettierrc*`, `prettier.config.*`, `prettier` in `devDependencies` | Biome replaces Prettier entirely |
| `eslint-plugin-prettier` | In `devDependencies` | Remove — Biome handles formatting |
| `eslint-config-prettier` | In `devDependencies` | Remove — no ESLint/Prettier conflict to resolve |

List all detected conflicts and tell the user to resolve them. Provide the Biome migration guide URL: `https://biomejs.dev/guides/migrate-eslint-prettier/`

### 7. Validate

Run `biome ci` (or tell the user to run it) to confirm the configuration is valid and all files pass. If there are violations:

- Formatting violations: tell the user to run `biome check --write` to auto-fix.
- Lint violations: list them and let the user decide how to resolve.

## Test File Overrides

The template includes an `overrides` block that relaxes certain rules for test files (`*.test.ts`, `*.spec.ts`, etc.):

- `noExplicitAny`: off (test mocks may need `any`)
- `noNonNullAssertion`: off (test assertions often use `!`)

These relaxations apply **only** to test files. Production code is unaffected.

## Important Notes

- Do NOT use `biome check --write` in pre-commit hooks. Hooks MUST be check-only (`biome ci`). Auto-fixing in hooks can produce unexpected changes that bypass review.
- Do NOT enable `nursery` rules. They are unstable and may change behavior between Biome versions.
- Do NOT add `biome.json` to `.gitignore`. It MUST be committed so all team members use the same configuration.
- The `--save-exact` flag for npm install is non-negotiable. Biome introduces breaking changes in minor versions.
- If the project uses Vue, Svelte, or Astro: Biome's support for these frameworks is experimental (as of v2.3). Warn the user and suggest framework-specific lint plugins as supplements if needed.
- Type checking (`tsc --noEmit`) is NOT provided by this skill. For TypeScript type validation, use `/setup-ts-typecheck`.
- This skill does NOT configure code coverage. For coverage setup, integrate with the relevant testing framework skill.
