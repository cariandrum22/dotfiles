# Surface Selection

Use this reference to decide whether a monorepo surface should keep typed ESLint
or can be simplified to Biome.

## Keep Typed ESLint

Typed ESLint is the better fit when a surface needs one or more of:

- Vue single-file component linting
- custom parsers such as `vue-eslint-parser`
- browser extension runtime boundaries
- `eslint-plugin-security`
- `eslint-plugin-functional`
- narrow path-based overrides by file class
- GraphQL-aware lint scripts or markdown or docs lint scripts tightly coupled to
  the package

These are the common signs that a surface is not just formatting plus basic
syntax lint.

## Prefer Biome

Biome is usually enough when the surface is:

- plain TypeScript or JavaScript
- not using Vue or another parser-dependent framework
- not using security or functional lint plugins
- not relying on many surface-specific override buckets
- mainly looking for formatting, unused imports, and baseline correctness rules

## Mixed Repositories

It is normal for one repository to use both:

- typed ESLint on `apps/web`
- typed ESLint on `apps/addon`
- Biome on simpler shared packages

Do not force uniformity if it reduces useful signal.
