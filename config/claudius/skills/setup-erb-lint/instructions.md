# Setup ERB Lint

Add ERB template linting, safe HTML parsing, and formatting to the project.

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

### 1. Add gems to `Gemfile`

Add to the `development, test` group. Do NOT duplicate gems that already exist.

```ruby
gem 'erb_lint', require: false
gem 'better_html', require: false
gem 'htmlbeautifier', require: false
```

### 2. Create `.erb-lint.yml`

If `.erb-lint.yml` does not exist, create it in the Rails root using
[erb-lint.yml.template](assets/erb-lint.yml.template).

If `.erb-lint.yml` already exists, inspect it and ask the user before changing
it. Merge missing baseline linters without removing project-specific excludes,
linters, or parser settings.

### 3. Create `config/initializers/better_html.rb`

If `config/initializers/better_html.rb` does not exist, create it using
[better_html.rb.template](assets/better_html.rb.template). This enforces safe
HTML parsing rules that prevent XSS in ERB templates.

If the initializer already exists, inspect it and ask the user before changing
it. Preserve project-specific BetterHtml settings and only merge missing safety
settings when approved.

### 4. Add to CI

If `.github/workflows/ci.yml` exists, inspect it and ask the user before
changing it. Add the following step to the `lint` job only when there is no
equivalent ERB lint step already present. Do NOT duplicate an existing
`erblint`, `erb_lint`, or ERB template lint step. Preserve the existing workflow
configuration: triggers, jobs, dependency setup, Bundler setup, and job
structure unless the user approves changing them:

```yaml
- name: Lint ERB templates
  run: bundle exec erblint --lint-all
```

### 5. Optionally add pre-commit hook

If the project uses git-hooks.nix, suggest adding an erb-lint hook:

```nix
erb-lint = mkHook "erb-lint" "bash -c 'cd apps/<app-dir> && bundle exec erblint --lint-all'";
```

## Important Notes

- `better_html` enforces that ERB templates produce valid HTML and prevents unsafe interpolation patterns that could lead to XSS.
- `htmlbeautifier` provides consistent ERB formatting. erb_lint can auto-correct formatting issues using it.
- The configuration is strict: accessibility linting is enabled, self-closing tags are enforced, and unsafe HTML patterns are blocked.
