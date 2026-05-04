# Setup TypeScript Type Check

Add a `tsc --noEmit` hook to enforce TypeScript type correctness at commit time and in CI.

This skill provides **type checking only**. It does NOT lint or format code — use `/setup-biome` for that. The two skills are complementary:

- `/setup-biome` — syntax rules, style, formatting (fast, no type information)
- `/setup-ts-typecheck` — full type system validation (slower, requires `tsconfig.json`)

## Prerequisites

- The project must have a `tsconfig.json` in the root (or a `tsconfig.json` referenced by a root-level config). If not, tell the user to create one first. Do NOT generate `tsconfig.json` — TypeScript configuration is project-specific.
- `tsc` must be available in the environment:
  - Nix: `pkgs.typescript` in `devShells.default.packages`
  - npm: `typescript` in `devDependencies`
- For Nix Flake projects: the project must have `git-hooks.nix` integrated in `flake.nix`. If not, tell the user to run `/setup-git-hooks` first.
- For non-Nix projects: `pre-commit` must be installed.

## Steps

### 1. Verify `tsconfig.json` strictness

Read the project's `tsconfig.json` (follow `extends` if present). Verify the following `compilerOptions` are set. If any are missing or weaker, **inform the user** of the discrepancies but do NOT modify `tsconfig.json` automatically.

| Option | Required Value | Rationale |
|---|---|---|
| `strict` | `true` | Enables all strict type-checking options as a group |
| `noUncheckedIndexedAccess` | `true` | Index signatures return `T \| undefined` |
| `noImplicitOverride` | `true` | Requires `override` keyword |
| `exactOptionalPropertyTypes` | `true` | Distinguishes `undefined` from missing property |
| `forceConsistentCasingInFileNames` | `true` | Prevents case-sensitive import issues on case-insensitive filesystems |
| `skipLibCheck` | `true` | Skips type checking of `.d.ts` files for performance |

If `strict` is `false` or absent, warn the user that the type checking hook will be less effective.

### 2. Add `package.json` script

Add the following script to `package.json` if it does not already exist. Do NOT overwrite an existing `typecheck` script.

```json
{
  "scripts": {
    "typecheck": "tsc --noEmit"
  }
}
```

### 3. Add pre-commit hook

Determine the hook integration method.

#### Path A: Nix git-hooks.nix

Add the hook from [hooks.nix.template](assets/hooks.nix.template) into the `hooks = { ... }` block. Do NOT duplicate if it already exists.

Hook added:

- **tsc-noEmit** — Runs `tsc --noEmit` to validate the entire project's type correctness. Fails on any type error.

Ensure `pkgs.typescript` is in `devShells.default.packages`. If not, add it.

#### Path B: Pre-commit local hooks

Add the hook from [pre-commit-hooks.yaml.template](assets/pre-commit-hooks.yaml.template) to `.pre-commit-config.yaml`. Do NOT duplicate if a `tsc-noEmit` hook already exists.

### 4. Validate

Run `tsc --noEmit` to confirm the project compiles without type errors. If there are errors, list them for the user. Do NOT suppress errors or add `// @ts-ignore` comments.

## Performance Considerations

`tsc --noEmit` checks the **entire project**, not just staged files. For large codebases this can be slow (10–30+ seconds). Options to mitigate:

- **Project references** (`composite: true` + `references`): enables incremental type checking
- **`tsconfig.json` `include` narrowing**: limit checked files
- **Move to CI only**: remove the pre-commit hook and rely on CI for type checking

If the project has more than ~500 TypeScript files, warn the user that the pre-commit hook may be slow and suggest CI-only enforcement as an alternative.

## Important Notes

- Do NOT modify `tsconfig.json` without user consent. TypeScript configuration varies significantly between projects (path aliases, module resolution, JSX settings, etc.).
- Do NOT use `tsc` for projects that use a custom type checker (e.g. `vue-tsc` for Vue, `astro check` for Astro). If `vue-tsc` or similar is detected in `devDependencies`, tell the user to adjust the hook entry accordingly.
- Do NOT add `--incremental` to the hook command. Incremental type checking writes `.tsbuildinfo` files that may not be desired in all projects.
- `tsc --noEmit` does NOT run on individual files — it always processes the entire project based on `tsconfig.json`. The `pass_filenames: false` setting in the hook template reflects this.
- This skill does NOT configure Biome or any linter. Type checking and linting are separate concerns. Use `/setup-biome` for lint and format.
