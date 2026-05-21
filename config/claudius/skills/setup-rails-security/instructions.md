# Setup Rails Security Scanning

Add Brakeman (application security) and bundler-audit (dependency vulnerability) scanning to the project.

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
gem 'brakeman', require: false
gem 'bundler-audit', require: false
```

### 2. Create `bin/brakeman` wrapper (optional)

If `bin/brakeman` does not exist, create it:

```ruby
#!/usr/bin/env ruby
# frozen_string_literal: true

ARGV.unshift("--ensure-latest")
load Gem.bin_path("brakeman", "brakeman")
```

Make it executable.

## Important Notes

- Brakeman scans application code for Rails-specific security vulnerabilities (SQL injection, XSS, mass assignment, etc.).
- bundler-audit checks gem dependencies against the Ruby Advisory Database for known CVEs.
- Both tools are best run in CI rather than as pre-commit hooks. Brakeman can be slow, and bundler-audit needs network access to update its advisory database.
- CI integration is handled by `/setup-rails-ci`.
