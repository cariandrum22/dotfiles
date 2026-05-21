# Setup Capybara Lint

Add rubocop-capybara to enforce best practices in system tests.

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

### 1. Add gem to `Gemfile`

Add to the `development, test` group. Do NOT duplicate if already present.

```ruby
gem 'rubocop-capybara', require: false
```

### 2. Add plugin to `.rubocop.yml`

If `.rubocop.yml` does not exist, tell the user to run `/setup-rubocop` first.

If `.rubocop.yml` exists, inspect it and ask the user before changing it. Add
`rubocop-capybara` to the `plugins` list only if it is missing, and preserve the
existing plugin order and project-specific RuboCop settings.

### 3. Add strict Capybara rules

Ensure the following Capybara rules are present. If equivalent rules already
exist, preserve the current values and ask the user before changing them. Do not
append duplicate rule blocks.

```yaml
# ── Capybara ─────────────────────────────────────────
Capybara/CurrentPathExpectation:
  Enabled: true

Capybara/MatchStyle:
  Enabled: true

Capybara/NegationMatcher:
  EnforcedStyle: have_no

Capybara/SpecificActions:
  Enabled: true

Capybara/SpecificFinders:
  Enabled: true

Capybara/SpecificMatcher:
  Enabled: true

Capybara/VisibilityMatcher:
  Enabled: true
```

## Important Notes

- `SpecificFinders` enforces `find_by_id` over `find('#id')` for clarity.
- `SpecificMatcher` enforces specific matchers like `have_button` over generic `have_selector('button')`.
- `SpecificActions` enforces `click_button` over `find('button').click`.
- `NegationMatcher: have_no` enforces `have_no_text` over `not_to have_text` for faster test execution (no implicit wait on negation).
