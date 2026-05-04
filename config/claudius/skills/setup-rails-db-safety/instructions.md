# Setup Rails Database Safety

Add strong_migrations (migration safety) and database_consistency (model-DB integrity) to the project.

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

Create the file using [strong_migrations.rb.template](assets/strong_migrations.rb.template). Set `target_postgresql_version` to match the project's PostgreSQL version.

### 3. Create `.database_consistency.yml`

Create the file in the Rails root:

```yaml
enabled: true
```

## Important Notes

- `strong_migrations` is a production gem because it hooks into `ActiveRecord::Migration` at runtime to block unsafe migrations.
- `database_consistency` is a development/test gem that checks model validations match database constraints. It requires a running database, so it should only run in CI (not as a pre-commit hook).
- `start_after` in the strong_migrations initializer should be updated to the latest migration timestamp after initial setup, so only new migrations are checked.
- CI integration is handled by `/setup-rails-ci`.
