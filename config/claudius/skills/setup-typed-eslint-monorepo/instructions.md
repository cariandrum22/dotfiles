# Setup Typed ESLint Monorepo

Set up or normalize typed ESLint for monorepo surfaces where Biome would lose
important framework, security, or domain-specific checks.

**The argument `$ARGUMENTS` is optional** and may provide
`profile=<rails-monorepo|polyglot-oss>` plus a comma-separated list of target
surfaces such as:

- `web,addon`
- `apps/web,apps/addon`
- `apps/web,packages/graphql-types`
- `profile=polyglot-oss apps/web,packages/graphql-types`

If `$ARGUMENTS` is omitted, inspect the repository and infer the candidate
surfaces and profile. Use `rails-monorepo` only when the repository clearly has
a Rails API plus frontend or package surfaces under a shared root. Use
`polyglot-oss` for general-purpose monorepos that do not fit the Rails profile.
If the profile remains ambiguous, default the profile reference to
`polyglot-oss` and report that assumption.

## References

Read the quality-profile reference that matches the selected profile before
editing:

- [../setup-quality-profile/references/rails-monorepo.md](../setup-quality-profile/references/rails-monorepo.md)
- [../setup-quality-profile/references/polyglot-oss.md](../setup-quality-profile/references/polyglot-oss.md)

Do not read or apply the `rails-monorepo` profile when this skill is invoked
from `polyglot-oss` or when the repository lacks a Rails API surface.

Then read these local references:

- [references/surface-selection.md](references/surface-selection.md)
- [references/rule-patterns.md](references/rule-patterns.md)

Use the selected profile reference for naming and CI policy. Use the local
references for ESLint-vs-Biome selection and surface-specific rule patterns.

## Existing Configuration Policy

Before changing an existing repository file, inspect the current content and ask the user to confirm the proposed change. This applies even when the change is additive, such as merging config keys, appending CI steps, adding package scripts, normalizing workflow names, or updating tool versions.

Do not ask when creating a missing file from this skill's template or when the user explicitly requested applying all changes without confirmation. Preserve project-specific settings and avoid replacing entire files unless the user approves that replacement.

When an existing file is involved, present a concise change plan before editing:

- `file`: target path
- `current_state`: what exists and whether this skill owns it
- `operation`: `skip`, `merge`, `update`, `replace`, or `create-adjacent`
- `proposed_delta`: exact setting, block, command, or path change to add or modify
- `risk`: compatibility, policy, or behavior risk
- `question`: the approval needed from the user

Default to `skip` or `merge`. Use `replace` only when the user explicitly
approves replacing that file.

If the user explicitly requested applying all changes without confirmation, do
not wait for approval after presenting the plan. Still follow the plan, preserve
project-specific settings, and do not use `replace` unless the user explicitly
allowed replacement.

Otherwise, if multiple existing files are affected, batch them in one plan and
wait for approval before editing any of them.

## Steps

### 1. Identify the JS and TS surfaces

Inspect likely monorepo roots such as:

- `apps/*`
- `packages/*`
- other workspace directories referenced by `pnpm-workspace.yaml`,
  `package.json`, or similar

For each candidate surface, inspect:

- `package.json`
- `eslint.config.*` or `.eslintrc*`
- `tsconfig*.json`
- framework markers such as `vue`, browser extension manifests, GraphQL tooling,
  or security-sensitive runtime code

If `$ARGUMENTS` is present, restrict changes to that explicit surface list.

### 2. Decide whether the surface should keep ESLint

Keep or add typed ESLint when a surface needs one or more of:

- Vue single-file component support
- browser extension or mixed browser and Node runtime linting
- GraphQL-aware lint scripts or document validation
- `eslint-plugin-security`
- `eslint-plugin-functional`
- narrow file-pattern overrides for tests, stories, stores, scripts, or
  generated code

If a surface is plain JS or TS without those needs, do NOT force ESLint there.
Recommend `/setup-biome` or keep the existing simpler stack instead.

### 3. Normalize package-local scripts

For each ESLint-managed surface, prefer package-local scripts such as:

- `lint`
- `typecheck`
- `format:check`

Keep or add auxiliary scripts only when the surface already needs them, for
example:

- `lint:graphql`
- `lint:md`

Use the framework-appropriate type checker:

- Vue: prefer `vue-tsc`
- plain TypeScript: `tsc --noEmit`

Do NOT replace an existing stronger or more specific typecheck command.

### 4. Normalize the ESLint config shape

Prefer flat config (`eslint.config.*`) for new work.

For each ESLint-managed surface:

- use `typescript-eslint` typed configs
- set `parserOptions.project` to the surface-local `tsconfig` files
- set `tsconfigRootDir` correctly for the surface
- keep ignores local to the surface build outputs and generated files

Add framework or domain plugins only where justified:

- Vue app: `eslint-plugin-vue` and `vue-eslint-parser`
- browser or mixed runtime code: `eslint-plugin-security` when handling
  untrusted inputs or filesystem or process boundaries
- immutability-heavy UI or extension logic: `eslint-plugin-functional` with
  targeted overrides

Do NOT blindly copy the same ruleset to every surface. Match the rule pattern to
the surface type.

### 5. Keep overrides intentional and narrow

When a rule is too strict for a subset of files, narrow it by path or file
pattern rather than disabling it globally.

Typical override targets:

- `**/*.vue`
- `**/*.test.*`
- `**/*.spec.*`
- `**/*.stories.*`
- `**/stores/**/*.ts`
- `scripts/**/*.mjs`
- e2e or live-test directories

Document the reason in the config only when the intent would otherwise be hard
to infer.

### 6. Integrate with root hooks and CI

If the repository uses Nix root hooks, keep hook names domain-oriented:

- `web-lint`
- `addon-lint`
- `packages-lint`

Prefer hook commands that delegate to package scripts, for example:

```nix
web-lint = mkHook "web-lint" "bash -c 'cd apps/web && pnpm lint && pnpm typecheck && pnpm format:check'";
```

In CI:

- install dependencies before running `pre-commit`
- use explicit step names with path qualifiers
- keep ESLint-managed surfaces separate from plain Biome-managed surfaces when
  they have different dependency or framework needs

### 7. Finish cleanly

Summarize:

- which surfaces stayed on typed ESLint
- which surfaces should use Biome instead
- which scripts were added or normalized
- which root hooks or CI steps were added or renamed
- any surface that still needs manual framework-specific lint commands

## Important Notes

- This skill is for monorepo surfaces that need ESLint because of framework or
  domain complexity. It is not a blanket replacement for `/setup-biome`.
- Prefer package-local lint entry points and root-level domain hooks.
- When a surface already has a stronger typed ESLint setup, merge missing pieces
  rather than rewriting the config wholesale.
