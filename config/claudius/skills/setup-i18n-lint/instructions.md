# Setup i18n Lint

Add i18n-tasks (translation management) and rubocop-i18n (hardcoded string detection) to the project.

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

- RuboCop must be installed. If not, tell the user to run `/setup-rubocop` first.

## Steps

### 1. Add gems to `Gemfile`

Add to the `development, test` group. Do NOT duplicate gems that already exist.

```ruby
gem 'i18n-tasks', require: false
gem 'rubocop-i18n', require: false
```

### 2. Add `rubocop-i18n` plugin to `.rubocop.yml`

Add `rubocop-i18n` to the `plugins` list in `.rubocop.yml` only if it is not
already present.

Then ensure the following rules are present. If equivalent rules already exist,
preserve the current values and ask the user before changing them. Do not append
duplicate rule blocks.

```yaml
# ── i18n ─────────────────────────────────────────────
I18n/RailsI18n:
  Enabled: true

I18n/GetText:
  Enabled: false
```

`RailsI18n` enforces `t()` / `I18n.t()` over hardcoded strings. `GetText` is disabled because this is a Rails project, not a GetText project.

### 3. Create `config/i18n-tasks.yml`

If `config/i18n-tasks.yml` does not exist, create it using
[i18n-tasks.yml.template](assets/i18n-tasks.yml.template).

If `config/i18n-tasks.yml` already exists, inspect it and ask the user before
changing it. Merge missing baseline settings without replacing locale paths,
ignore patterns, scanner settings, or project-specific data configuration.

Infer the locale values before writing the file:

- `__BASE_LOCALE__` — the Rails `I18n.default_locale`, or the only locale found under `config/locales`, or ask the user if unclear
- `__LOCALES__` — YAML inline list of supported locales found under `config/locales` or `I18n.available_locales`, for example `[en, ja]`

Do not leave locale placeholders in `config/i18n-tasks.yml`. Do not assume `ja` unless the repository already uses Japanese as its default locale.

### 4. Add to CI

If `.github/workflows/ci.yml` exists, inspect it and ask the user before
changing it. Add the following step to the `lint` job only when there is no
equivalent i18n-tasks step already present. Do NOT duplicate an existing
`i18n-tasks missing`, `i18n-tasks unused`, or translation-health step. Preserve
the existing workflow configuration: triggers, jobs, dependency setup, Bundler
setup, and job structure unless the user approves changing them:

```yaml
- name: Check for missing and unused translations
  run: |
    bundle exec i18n-tasks missing --format terminal
    bundle exec i18n-tasks unused --format terminal
```

## Important Notes

- `i18n-tasks` analyzes source code and locale files to find missing translations (keys used in code but not in YAML) and unused translations (keys in YAML but not in code).
- `rubocop-i18n` with `RailsI18n` catches hardcoded user-facing strings in Ruby code that should use `t()`.
- Run `bundle exec i18n-tasks normalize` to sort and normalize locale files.
- Run `bundle exec i18n-tasks health` for a quick overview of translation health.
