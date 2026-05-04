# Setup i18n Lint

Add i18n-tasks (translation management) and rubocop-i18n (hardcoded string detection) to the project.

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

Add `rubocop-i18n` to the `plugins` list in `.rubocop.yml`.

Then append the following rules:

```yaml
# ── i18n ─────────────────────────────────────────────
I18n/RailsI18n:
  Enabled: true

I18n/GetText:
  Enabled: false
```

`RailsI18n` enforces `t()` / `I18n.t()` over hardcoded strings. `GetText` is disabled because this is a Rails project, not a GetText project.

### 3. Create `config/i18n-tasks.yml`

Create the file using [i18n-tasks.yml.template](assets/i18n-tasks.yml.template).

### 4. Add to CI

If `.github/workflows/ci.yml` exists, add the following step to the `lint` job:

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
