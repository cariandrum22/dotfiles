# Setup Capybara Lint

Add rubocop-capybara to enforce best practices in system tests.

## Prerequisites

- RuboCop must be installed. If not, tell the user to run `/setup-rubocop` first.

## Steps

### 1. Add gem to `Gemfile`

Add to the `development, test` group. Do NOT duplicate if already present.

```ruby
gem 'rubocop-capybara', require: false
```

### 2. Add plugin to `.rubocop.yml`

Add `rubocop-capybara` to the `plugins` list in `.rubocop.yml`.

### 3. Add strict Capybara rules

Append the following to `.rubocop.yml`:

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
