---
name: setup-rails-security
description: Set up Rails security scanning with Brakeman and bundler-audit. Use when adding security checks to a Rails project.
disable-model-invocation: true
argument-hint: (no arguments required)
---

# Setup Rails Security Scanning

Add Brakeman (application security) and bundler-audit (dependency vulnerability) scanning to the project.

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
