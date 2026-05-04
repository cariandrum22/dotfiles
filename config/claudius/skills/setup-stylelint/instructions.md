# Setup Stylelint

Add stylelint with strict, functional-style CSS/SCSS rules to the project.

## Steps

### 1. Install npm packages

Run the following to install stylelint and plugins:

```bash
yarn add -D stylelint stylelint-config-standard-scss stylelint-order stylelint-declaration-strict-value
```

If the project uses `npm` instead of `yarn`, use `npm install --save-dev` instead.

### 2. Create `.stylelintrc.yml`

Create the file using [stylelintrc.yml.template](assets/stylelintrc.yml.template).

### 3. Add lint script to `package.json`

Add the following to the `scripts` section in `package.json`:

```json
"lint:css": "stylelint 'app/assets/stylesheets/**/*.{css,scss}'",
"lint:css:fix": "stylelint 'app/assets/stylesheets/**/*.{css,scss}' --fix"
```

### 4. Add to CI

If `.github/workflows/ci.yml` exists, add the following step to the `lint` job:

```yaml
- name: Lint stylesheets
  run: yarn lint:css
```

## Important Notes

- `stylelint-config-standard-scss` extends the standard config with SCSS support and sensible defaults.
- `stylelint-order` enforces a consistent property order for readability and maintainability.
- `stylelint-declaration-strict-value` forces variables/functions for colors, fonts, and sizes — preventing magic values and promoting design tokens.
- The config enforces functional/compositional patterns: no `@extend`, prefer `@mixin`/`@use`, no `!important`, no ID selectors.
- Max nesting depth of 3 and max selector compound count of 3 keep specificity flat.
- Run `yarn lint:css:fix` to auto-fix fixable violations.
