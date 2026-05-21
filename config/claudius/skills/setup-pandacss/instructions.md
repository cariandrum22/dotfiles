# Setup PandaCSS

Set up or harden PandaCSS for a React/TypeScript frontend, especially a React Router 7 + Vite app.

This skill is based on practices proven useful in a React Router 7 admin-console refactor:

- Generate `styled-system/` as a build-time artifact, not hand-written code.
- Use semantic tokens, recipes, slot recipes, and `styled-system/jsx` for reusable UI.
- Keep route-level `css()` usage static, token-backed, and local to page layout.
- Use explicit CSS cascade layers and React Router-compatible stylesheet loading.
- Add check-only enforcement for inline styles, raw colors, and missing Panda config basics.

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

## Prerequisites

- The project must have a `package.json`.
- The project should be React, React Router, Vite, Storybook, or another JS/TS frontend that can run PandaCSS.
- `node` must be available where hooks run.
- If the project uses Nix Flakes and git-hooks.nix, ensure `/setup-git-hooks` has already been applied.
- If the project needs JS/TS linting or type checking, compose this skill with `/setup-biome` and `/setup-ts-typecheck`.

## Steps

### 1. Identify the app mode

Detect the frontend shape before editing:

| Evidence | Mode | Styling integration |
|---|---|---|
| `vite.config.*`, `createBrowserRouter`, `RouterProvider` | React Router Data/SPA mode on Vite | Import one root CSS file from the client entry (`src/main.tsx`, `src/entry.client.tsx`, etc.) |
| `react-router.config.*`, `@react-router/dev`, route modules with `links` exports | React Router Framework mode | Expose CSS through route/root `links` using Vite `?url` imports |
| Storybook config under `.storybook/` | Component catalog | Include Storybook stories in Panda extraction when stories call Panda APIs |

Do not convert routing mode as part of this skill. Only adapt PandaCSS to the app mode that already exists.

### 2. Install PandaCSS dependencies

Check `package.json`.

Required dev dependency:

- `@pandacss/dev`

Optional dependencies, only if the project already uses or wants them:

- `@park-ui/panda-preset`
- `@ark-ui/react`

Add package-manager commands using the project's existing manager. Do not switch package managers.

Examples:

```bash
pnpm add -D @pandacss/dev
pnpm add @ark-ui/react
pnpm add -D @park-ui/panda-preset
```

For Nix Flake projects, verify `pkgs.nodejs` is listed in `devShells.default.packages` or an
equivalent dev-shell package list. If it is missing, add it before wiring the `pandacss-practices`
hook. The practice checker runs with `node scripts/check-pandacss-practices.mjs`, so the hook must
not depend on an unpinned host Node installation.

### 3. Add or normalize Panda config

If `panda.config.ts` does not exist, create it from the baseline below.

If `panda.config.ts` already exists, inspect it, report the proposed config
delta, and ask the user before changing it. Merge only missing compatible
settings. Preserve existing tokens, presets, `conditions`, include/exclude
patterns, `globalCss`, output paths, and project-specific extraction behavior
unless the user approves changing them.

Required baseline:

```ts
import { defineConfig } from '@pandacss/dev';

const isStorybook = process.env['STORYBOOK'] === 'true';

export default defineConfig({
  strictTokens: true,
  preflight: true,
  include: ['./src/**/*.{ts,tsx}'].concat(isStorybook ? './.storybook/**/*.{ts,tsx}' : []),
  exclude: [],
  outdir: 'styled-system',
  jsxFramework: 'react',
});
```

Adjust rather than blindly overwrite:

- Keep `strictTokens: true` unless the migration is explicitly staged and the user accepts temporary looseness.
- Set `preflight` deliberately. Prefer `true` for new Panda-first apps; preserve an existing reset only when changing it would be a larger visual migration.
- Keep `include` aligned with every route, component, and Storybook file that calls Panda APIs.
- Use `conditions` for theme/data-state selectors that repeat across components.
- Keep `globalCss` minimal: `html`, `body`, `#root`, color mode, font smoothing, and unavoidable browser defaults only.
- Add `staticCss` for recipe variants that are chosen at runtime and cannot be seen by Panda extraction.
- Keep generated output in one place, normally `styled-system/`, and configure TypeScript/Vite aliases for it.

### 4. Add CSS entry and cascade layers

Use exactly one root stylesheet for Panda layers.

For Vite SPA/Data mode:

1. If the root CSS file does not exist, create one such as `src/App.css`.
2. If the root CSS file already exists, inspect it, report the proposed Panda
   layer addition, and ask the user before changing it.
3. Import it once from the app entry. If the app entry already exists, inspect
   it and ask the user before adding or changing imports.
4. Keep Panda layer order explicit:

```css
@layer reset, third-party, base, tokens, recipes, utilities;
```

Include third-party layers only when needed. Panda's own relative order must remain:

```text
reset -> base -> tokens -> recipes -> utilities
```

For React Router Framework mode:

1. Import CSS URLs with Vite `?url`.
2. Return them from `links` in the root or route module.

```tsx
import appStylesHref from './App.css?url';

export const links = () => [{ rel: 'stylesheet', href: appStylesHref }];
```

Do not inject stylesheets imperatively in React components or loaders.

### 5. Add generated-system aliases

Update TypeScript and Vite aliases when missing.

`tsconfig.json`:

```json
{
  "compilerOptions": {
    "baseUrl": ".",
    "paths": {
      "styled-system/*": ["./styled-system/*"]
    }
  },
  "include": ["src", "styled-system", "panda.config.ts"]
}
```

`vite.config.ts`:

```ts
resolve: {
  alias: {
    'styled-system': resolve(rootDir, 'styled-system'),
  },
},
```

Preserve existing aliases and path configuration.

### 6. Add scripts

Add scripts without overwriting existing names:

```json
{
  "scripts": {
    "panda:codegen": "panda codegen",
    "panda:codegen:watch": "panda codegen --watch",
    "panda:practices": "node scripts/check-pandacss-practices.mjs"
  }
}
```

If `styled-system/` is ignored, CI and local setup must run `panda:codegen` before type checking,
tests, Storybook, and production builds.

### 7. Add practice enforcement

Copy [panda-practices-check.mjs.template](assets/panda-practices-check.mjs.template) to:

```text
scripts/check-pandacss-practices.mjs
```

Then add either hook template when the project uses that backend:

- Nix git-hooks.nix: [hooks.nix.template](assets/hooks.nix.template)
- Pre-commit local hooks: [pre-commit-hooks.yaml.template](assets/pre-commit-hooks.yaml.template)

The check is intentionally narrow and check-only. It fails on:

- Missing `panda.config.*`
- Missing `strictTokens: true`
- Missing `jsxFramework: 'react'`
- Missing `panda:codegen`
- Inline `style=` in JSX/TSX
- Raw hex/RGB/HSL colors outside token/theme files
- `var(--...)` references outside token/theme/global style files or explicitly allowlisted runtime-var files

If a project legitimately needs measured runtime layout, prefer setting a CSS variable on a wrapper and consuming it
from a Panda recipe or slot recipe. If that is still too strict, add a small project-specific allowlist to the script
instead of weakening the whole policy. For narrow one-off runtime variables, add an inline
`pandacss-allow-runtime-var` comment with a short rationale.

The checker accepts comma-separated env overrides for narrow project differences:

- `PANDACSS_CHECK_ROOTS`
- `PANDACSS_CHECK_TOKEN_PATHS`
- `PANDACSS_CHECK_RUNTIME_VAR_PATHS`
- `PANDACSS_CHECK_IGNORE_PATHS`

### 8. Use Panda idioms in code changes

Apply these implementation rules during migration:

- Put reusable component visuals in `cva`, `sva`, or `styled-system/jsx` components.
- Use `sva` for multi-part components such as Sidebar, Page, Field, Table, DataList, and ToggleSwitch.
- Use `cva` for single-slot components such as Button, Text, Heading, Input, Textarea, Card, and Message.
- Reserve `css()` and layout patterns such as `flex`, `grid`, `hstack`, and `vstack` for route-local layout.
- Prefer semantic tokens (`fg`, `fg.muted`, `bg.panel`, `border`, `primary`) over palette tokens.
- Use bracket escape values only for intentional one-offs, and prefer to move repeated values into tokens.
- Do not use React Router loaders/actions for styling side effects; derive visual state in components from loader data,
  navigation state, action results, data attributes, or recipe variants.
- Keep Storybook using the same root CSS and color-mode attributes as the app.

### 9. Validate

Run the strongest available local loop:

```bash
pnpm panda:codegen
pnpm panda:practices
pnpm typecheck
pnpm test:run
pnpm build
pnpm build-storybook
```

If the project uses a different package manager, translate the commands without changing script names.

For UI migrations, also inspect Storybook or run visual regression tests when available.

## Important Notes

- Do not commit `styled-system/` when the project intentionally ignores generated output. Document and enforce codegen
  instead.
- Do not rely on Panda extraction for runtime-constructed token names, class names, or variant values. Use recipe variants,
  data attributes, CSS variables, or `staticCss`.
- Do not add large global CSS files to compensate for missing recipes. Global CSS is the exception path.
- Do not remove existing CSS frameworks in the same change unless the user explicitly asked for a full migration.
- Do not suppress the practice checker with broad path exclusions. Prefer fixing components or adding narrow allowlist
  comments/paths with a short rationale.
