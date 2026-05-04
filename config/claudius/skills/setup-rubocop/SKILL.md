---
name: setup-rubocop
description: Set up RuboCop with rubocop-rails, rubocop-performance, rubocop-rake, rubocop-minitest, and rubocop-thread_safety for a Rails project. Use when adding Ruby/Rails code linting.
disable-model-invocation: true
argument-hint: (no arguments required)
---

# Setup RuboCop

Add RuboCop with Rails-oriented plugins and a strict configuration to the project.

## Steps

### 1. Add gems to `Gemfile`

Add the following gems to the `development, test` group. Do NOT duplicate gems that already exist.

```ruby
gem 'rubocop', require: false
gem 'rubocop-minitest', require: false
gem 'rubocop-performance', require: false
gem 'rubocop-rails', require: false
gem 'rubocop-rake', require: false
gem 'rubocop-thread_safety', require: false
```

### 2. Create `.rubocop.yml`

Create the file using [rubocop.yml.template](rubocop.yml.template). If `.rubocop.yml` already exists, merge missing plugins and rules into it without removing existing configuration.

### 3. Create `bin/rubocop` wrapper (optional)

If `bin/rubocop` does not exist, create it:

```ruby
#!/usr/bin/env ruby
# frozen_string_literal: true

ARGV.unshift("--config", File.expand_path("../.rubocop.yml", __dir__))
load Gem.bin_path("rubocop", "rubocop")
```

Make it executable.

## Important Notes

- Do NOT include `rubocop-sorbet`. That belongs in `/setup-sorbet`.
- `NewCops: enable` ensures newly added cops are automatically active.
- The Rails cops explicitly listed are safety-critical rules that should always be enabled regardless of defaults.
