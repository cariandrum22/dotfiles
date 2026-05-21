# Setup Stylelint

Add stylelint with strict, functional-style CSS/SCSS rules to the project.

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

### 1. Install packages

Use the package manager already used by the project. Do not switch package managers.

Examples:

```bash
npm install --save-dev stylelint stylelint-config-standard-scss stylelint-order stylelint-declaration-strict-value
pnpm add -D stylelint stylelint-config-standard-scss stylelint-order stylelint-declaration-strict-value
yarn add -D stylelint stylelint-config-standard-scss stylelint-order stylelint-declaration-strict-value
bun add -d stylelint stylelint-config-standard-scss stylelint-order stylelint-declaration-strict-value
```

Do NOT run these commands automatically. The user must choose or confirm the package manager.

### 2. Create `.stylelintrc.yml`

If `.stylelintrc.yml` does not exist, create it using
[stylelintrc.yml.template](assets/stylelintrc.yml.template).

If a Stylelint config already exists (`.stylelintrc*` or `stylelint.config.*`),
inspect it and ask the user before changing it. Merge missing rules and plugins
without replacing project-specific syntax, ignore patterns, or framework
overrides.

### 3. Add lint script to `package.json`

Add the following to the `scripts` section in `package.json` only when the script
names are absent. Do not overwrite existing scripts with the same names; report
the mismatch and ask the user how to proceed.

```json
"lint:css": "stylelint 'app/assets/stylesheets/**/*.{css,scss}'",
"lint:css:fix": "stylelint 'app/assets/stylesheets/**/*.{css,scss}' --fix"
```

### 4. Add to CI

If `.github/workflows/ci.yml` exists, inspect it and ask the user before
changing it. Add the following step to the `lint` job only when there is no
equivalent stylesheet lint step already present. Do NOT duplicate an existing
stylelint or `lint:css` step. Preserve the existing workflow triggers, jobs,
dependency setup, package-manager setup, and job structure unless the user
approves changing them:

```yaml
- name: Lint stylesheets
  run: <package-manager-run-command> lint:css
```

Use the project's package manager command, for example `npm run lint:css`, `pnpm lint:css`, `yarn lint:css`, or `bun run lint:css`. Ensure dependencies are installed earlier in the workflow.

## Important Notes

- `stylelint-config-standard-scss` extends the standard config with SCSS support and sensible defaults.
- `stylelint-order` enforces a consistent property order for readability and maintainability.
- `stylelint-declaration-strict-value` forces variables/functions for colors, fonts, and sizes — preventing magic values and promoting design tokens.
- The config enforces functional/compositional patterns: no `@extend`, prefer `@mixin`/`@use`, no `!important`, no ID selectors.
- Max nesting depth of 3 and max selector compound count of 3 keep specificity flat.
- Run the matching package-manager command for `lint:css:fix` to auto-fix fixable violations.
