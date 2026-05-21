# Setup Rails Testing Infrastructure

Add SimpleCov with branch coverage tracking to the project.

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

### 1. Detect the test framework

Use RSpec when `rspec-rails` is present in `Gemfile` or the project has a
`spec/` directory. Otherwise use Minitest.

### 2. Add gems to `Gemfile`

Add to the `development, test` group. Do NOT duplicate gems that already exist.

```ruby
gem 'simplecov', require: false
```

### 3. Install SimpleCov in the test boot path

For Minitest, create or update `test/test_helper.rb`.

If `test/test_helper.rb` does not exist, create it using [test_helper.rb.template](assets/test_helper.rb.template).

If it already exists, first check whether SimpleCov is already configured in
`test/test_helper.rb`, `spec/spec_helper.rb`, or `spec/rails_helper.rb`. If it is
already configured, do not add a duplicate block; inspect the current thresholds
and ask the user before changing them.

If SimpleCov is not configured, add the SimpleCov block at the very top of the
selected helper file (before any other requires):

```ruby
require 'simplecov'
SimpleCov.start 'rails' do
  enable_coverage :branch
  minimum_coverage line: 90, branch: 80
end
```

For RSpec, add the same SimpleCov block at the very top of
`spec/spec_helper.rb` or `spec/rails_helper.rb`, before any other requires or
application code. Prefer `spec/spec_helper.rb` when it exists and the project's
spec files load it; otherwise use `spec/rails_helper.rb`.

If the selected RSpec helper file does not exist, create the minimal helper
needed by the project and place the SimpleCov block before requiring Rails or
RSpec support files. Do not add SimpleCov only to `test/test_helper.rb` in an
RSpec project.

### 4. Add `coverage/` to `.gitignore`

If `.gitignore` exists, check whether it already contains `coverage/`. If not,
show the proposed `coverage/` addition and ask the user before appending it.

If `.gitignore` does not exist, create it with `coverage/` as the sole entry.

## Important Notes

- SimpleCov MUST be required at the very top of `test/test_helper.rb`, `spec/spec_helper.rb`, or `spec/rails_helper.rb`, before `rails/test_help`, `rails_helper`, or any application code is loaded. Otherwise, coverage will be incomplete.
- `enable_coverage :branch` enables branch coverage in addition to line coverage.
- The thresholds (line: 90%, branch: 80%) are enforced from the start. Setting them early prevents coverage from degrading as the codebase grows.
- In CI, `bundle exec rspec` or `bin/rails test` enforces coverage as long as SimpleCov is loaded by the relevant helper before application boot.
