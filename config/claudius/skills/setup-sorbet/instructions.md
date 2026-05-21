# Setup Sorbet

Add Sorbet type checking with Tapioca RBI generation and rubocop-sorbet linting to the project.

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

Add the following gems. Do NOT duplicate gems that already exist.

In the `development, test` group:
```ruby
gem 'sorbet', require: false
gem 'tapioca', require: false
gem 'rubocop-sorbet', require: false
```

As a top-level (production) gem:
```ruby
gem 'sorbet-runtime'
```

### 2. Add `rubocop-sorbet` to `.rubocop.yml`

If `.rubocop.yml` does not exist, tell the user to run `/setup-rubocop` first.

If `.rubocop.yml` exists, inspect it and ask the user before changing it. Add
`rubocop-sorbet` to the `plugins` list only if it is missing, and preserve the
existing plugin order and project-specific RuboCop settings.

### 3. Create `sorbet/config`

Create the file if it does not exist:

```
--dir app
--dir lib
--dir sorbet
--ignore=/db/
--ignore=/bin/
--ignore=/vendor/
```

### 4. Create `sorbet/tapioca/config.yml`

Create the file if it does not exist:

```yaml
gem:
  exclude:
    - json
    - msgpack
```

### 5. Remind the user about initialization

After setup, remind the user to run:

```bash
bundle install
bundle exec tapioca init
bundle exec tapioca gems
bundle exec tapioca dsl
bundle exec srb tc
```

## Important Notes

- `sorbet-runtime` must be a production gem (not in `development, test` group) since `T::Struct`, `T.let`, etc. are used at runtime.
- `rubocop-sorbet` enforces consistent use of `typed:` sigils and Sorbet patterns. It requires RuboCop to be installed.
- Do NOT add `rubocop-sorbet` to `.rubocop.yml` if RuboCop is not set up yet.
