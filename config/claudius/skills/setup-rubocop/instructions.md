# Setup RuboCop

Add RuboCop with Rails-oriented plugins and a strict configuration to the project.

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

Add the following gems to the `development, test` group. Do NOT duplicate gems that already exist.

```ruby
gem 'rubocop', require: false
gem 'rubocop-minitest', require: false
gem 'rubocop-performance', require: false
gem 'rubocop-rails', require: false
gem 'rubocop-rake', require: false
gem 'rubocop-thread_safety', require: false
```

### 2. Infer the Ruby target version

Before creating `.rubocop.yml`, infer the Ruby version from the first reliable source available:

1. `.ruby-version`
2. `.tool-versions`
3. `mise.toml`
4. `Gemfile` or `gems.rb` `ruby` declaration

Use the inferred major/minor version, for example `3.3` from `3.3.6`. If the version cannot be inferred confidently, ask the user before creating the file. Do not leave the `__RUBY_VERSION__` placeholder in `.rubocop.yml`.

### 3. Create `.rubocop.yml`

If `.rubocop.yml` does not exist, create it using
[rubocop.yml.template](assets/rubocop.yml.template), replacing
`__RUBY_VERSION__` with the inferred target version.

If `.rubocop.yml` already exists, inspect it and ask the user before changing
it. Merge only missing compatible plugins and rules; Do NOT duplicate plugin
entries or rule blocks. Preserve existing plugin order, inheritance, excludes,
and project-specific settings unless the user approves changing them.

### 4. Create `bin/rubocop` wrapper (optional)

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
