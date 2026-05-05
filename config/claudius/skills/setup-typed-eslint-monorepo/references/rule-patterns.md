# Rule Patterns

Use these patterns when normalizing typed ESLint across monorepo surfaces.

## Vue Web App

Prefer:

- `typescript-eslint` typed configs
- `eslint-plugin-vue`
- `vue-eslint-parser`
- `eslint-plugin-security` when the app handles URLs, tokens, or untrusted input
- `eslint-plugin-functional` only with narrow overrides

Typical overrides:

- disable some immutability rules in `**/*.vue`
- relax strict unsafe rules in tests and stories
- disable type-aware linting in Node-only scripts when appropriate

Use `vue-tsc` for `typecheck` instead of raw `tsc`.

## Browser Add-on

Prefer:

- typed `typescript-eslint`
- `eslint-plugin-security`
- `eslint-plugin-functional` when state mutation needs tight control

Typical overrides:

- allow conditional or expression statements where browser APIs require them
- keep immutability rules strict in production code
- relax rules only for tests or generated files

Use explicit browser globals or extension runtime globals rather than broad
global disables.

## Plain TS Package

If the package is plain TS without framework-specific needs, do not introduce a
heavy ESLint stack just to mirror the app surfaces. Prefer Biome plus a
typecheck command unless the package already depends on ESLint-specific rules.

## Root Hook and CI Pattern

Keep root hooks domain-oriented and delegate to package scripts:

- `web-lint`
- `addon-lint`

Prefer CI step names such as:

- `Install web dependencies (apps/web)`
- `Install add-on dependencies (apps/addon)`
- `Audit web dependencies (apps/web)`

Avoid hook or step names that expose a temporary tool choice when the real owned
surface is the domain.
