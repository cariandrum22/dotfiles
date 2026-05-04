---
name: setup-rails-testing
description: Set up test infrastructure with SimpleCov coverage tracking for a Rails project. Use when adding test coverage measurement.
disable-model-invocation: true
argument-hint: (no arguments required)
---

# Setup Rails Testing Infrastructure

Add SimpleCov with branch coverage tracking to the project.

## Steps

### 1. Add gems to `Gemfile`

Add to the `development, test` group. Do NOT duplicate gems that already exist.

```ruby
gem 'simplecov', require: false
```

### 2. Create or update `test/test_helper.rb`

If `test/test_helper.rb` does not exist, create it using [test_helper.rb.template](test_helper.rb.template).

If it already exists, add the SimpleCov block at the very top of the file (before any other requires):

```ruby
require 'simplecov'
SimpleCov.start 'rails' do
  enable_coverage :branch
  minimum_coverage line: 90, branch: 80
end
```

### 3. Add `coverage/` to `.gitignore`

If not already present, add `coverage/` to `.gitignore`.

## Important Notes

- SimpleCov MUST be required at the very top of `test_helper.rb`, before `rails/test_help` or any application code is loaded. Otherwise, coverage will be incomplete.
- `enable_coverage :branch` enables branch coverage in addition to line coverage.
- The thresholds (line: 90%, branch: 80%) are enforced from the start. Setting them early prevents coverage from degrading as the codebase grows.
- If the project uses RSpec instead of Minitest, the setup goes in `spec/spec_helper.rb` or `spec/rails_helper.rb` instead.
