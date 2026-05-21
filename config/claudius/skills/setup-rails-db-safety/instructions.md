# Setup Rails Database Safety

Add strong_migrations (migration safety) and database_consistency (model-DB integrity) to the project.

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

Do NOT duplicate gems that already exist.

As a top-level (production) gem:
```ruby
gem 'strong_migrations'
```

In the `development, test` group:
```ruby
gem 'database_consistency', require: false
```

### 2. Create `config/initializers/strong_migrations.rb`

If `config/initializers/strong_migrations.rb` does not exist, create it using
[strong_migrations.rb.template](assets/strong_migrations.rb.template). Set
`target_postgresql_version` to match the project's PostgreSQL version.

If the initializer already exists, inspect it and ask the user before changing
it. Preserve existing `start_after`, custom checks, safety overrides, and
database-version policy unless the user approves a change.

Replace `__PG_VERSION__` with the PostgreSQL major version, or the major/minor version if the project already tracks that level of precision. Infer it from `config/database.yml`, Docker Compose service images, devcontainer settings, or deployment manifests. If it cannot be inferred confidently, ask the user before creating the initializer. Do not leave the `__PG_VERSION__` placeholder in Ruby code.

### 3. Create `.database_consistency.yml`

If `.database_consistency.yml` does not exist, create the file in the Rails root:

```yaml
enabled: true
```

If `.database_consistency.yml` already exists, inspect it and ask the user before
changing it. Do not remove project-specific ignore lists, severity settings, or
custom checks.

## Important Notes

- `strong_migrations` is a production gem because it hooks into `ActiveRecord::Migration` at runtime to block unsafe migrations.
- `database_consistency` is a development/test gem that checks model validations match database constraints. It requires a running database, so it should only run in CI (not as a pre-commit hook).
- `start_after` in the strong_migrations initializer should be updated to the latest migration timestamp after initial setup, so only new migrations are checked.
- CI integration is handled by `/setup-rails-ci`.
