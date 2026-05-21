# Setup Rails CI

Generate a GitHub Actions CI workflow by reading the project's `Gemfile` to determine which quality tools are available.

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

### 1. Read the project's `Gemfile`

Determine which of the following tools are present:

- `brakeman`
- `bundler-audit`
- `database_consistency`
- `rubocop`
- `rspec-rails`
- `simplecov`
- `sorbet`

### 2. Determine the Ruby version

Before creating the workflow, infer the Ruby version from the first reliable source available:

1. `.ruby-version`
2. `.tool-versions`
3. `mise.toml`
4. `Gemfile` or `gems.rb` `ruby` declaration

Use `.ruby-version` as the workflow value only when that file exists. Otherwise use the inferred exact version string, for example `3.3.6`. If the version cannot be inferred confidently, ask the user before creating the workflow. Do not leave the `__RUBY_VERSION__` placeholder in `.github/workflows/ci.yml`.

### 3. Create or update the CI workflow

If `.github/workflows/ci.yml` does not exist, create it using
[ci.yml.template](assets/ci.yml.template) as the base.

If `.github/workflows/ci.yml` already exists, inspect the current workflow and
ask the user before changing it. Merge missing jobs, permissions, action pinning,
and tool steps into the existing structure. Do not replace the workflow
wholesale unless the user explicitly approves that replacement. Preserve
project-specific triggers, services, matrices, environment variables, path
filters, concurrency, and permissions unless they conflict with the requested CI
policy.

The template has three jobs:

- **`scan_ruby`** — Security and consistency checks
- **`lint`** — Code style and type checking
- **`test`** — Rails test execution and coverage gates

Replace every `__RUBY_VERSION__` placeholder with the value from Step 2.
Keep the workflow-level `permissions: contents: read` unless the project adds a
step that demonstrably needs a broader permission.

The template pins `actions/checkout` and `ruby/setup-ruby` by full commit SHA.
When upgrading either action, re-resolve the desired tag or branch with
`git ls-remote`, then update both the SHA and the adjacent resolved-date
comment. Do not switch these actions back to mutable tag references.

### 4. Populate jobs based on installed tools

**`scan_ruby` job** — add steps for each tool present:

| Tool | Step |
|---|---|
| `brakeman` | `bin/brakeman --no-pager` when `bin/brakeman` exists; otherwise `bundle exec brakeman --no-pager` |
| `bundler-audit` | `bundle exec bundler-audit check --update` |
| `database_consistency` | `bundle exec database_consistency` |

**`lint` job** — add steps for each tool present:

| Tool | Step |
|---|---|
| `rubocop` | `bin/rubocop -f github` when `bin/rubocop` exists; otherwise `bundle exec rubocop -f github` |
| `sorbet` | `bundle exec srb tc` |

**`test` job** — always add a Rails test step:

| Evidence | Step |
|---|---|
| `rspec-rails` in `Gemfile` | `bundle exec rspec` |
| otherwise | `bin/rails test` |

If `simplecov` is present, the test command is the coverage gate. Do not add a separate coverage-only command unless the project already has one.

If a tool is not in the Gemfile, do NOT add its step.

### 5. Handle database setup specially

`database_consistency` requires a running database. If it's present, the `scan_ruby` job needs database setup. Add the following to that job:

```yaml
services:
  postgres:
    image: postgres:16
    env:
      POSTGRES_PASSWORD: password
    ports:
      - 5432:5432
    options: >-
      --health-cmd pg_isready
      --health-interval 10s
      --health-timeout 5s
      --health-retries 5
```

And add a database setup step before the consistency check:

```yaml
- name: Set up database
  env:
    DATABASE_URL: postgres://postgres:password@localhost:5432/test
    RAILS_ENV: test
  run: bin/rails db:setup
```

If the test job needs a database, add the same database service and setup step to the `test` job before the test command. Use the project's actual adapter and CI database URL; do not force PostgreSQL for SQLite-only projects.

If `database_consistency` is NOT present and the test job does not need a database service, do NOT add the database service or setup step.

## Important Notes

- If `.github/workflows/ci.yml` already exists, merge/update it in place; ask
  the user before any broad replacement or policy-changing rewrite.
- The template runs push CI for all branches. Do not narrow it to `main` unless the repository explicitly requires that policy.
- This workflow uses SHA-pinned `ruby/setup-ruby` with `bundler-cache: true` for fast gem installation.
- The Ruby version must match the project source of truth; `.ruby-version` is common but not mandatory.
- This CI is for the Rails app itself. The Nix-based root `lint.yml` (generated by `/setup-nix-ci-lint`) handles pre-commit hooks separately.
