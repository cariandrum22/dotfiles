# Setup Rails API

Set up a complete quality stack for a Rails API project. This skill orchestrates the following individual skills in order.

## Execution Order

Apply each skill in the following order. For each step, follow the instructions defined in that skill. If a skill's prerequisites are already satisfied (e.g. gems already exist), skip the redundant parts but still verify correctness.

### Step 1: `/setup-rubocop`
Add RuboCop with rubocop-rails, rubocop-performance, rubocop-rake, rubocop-minitest, rubocop-thread_safety, and strict `.rubocop.yml`.

### Step 2: `/setup-sorbet`
Add Sorbet type checking with Tapioca and rubocop-sorbet.

### Step 3: `/setup-rails-security`
Add Brakeman and bundler-audit for security scanning.

### Step 4: `/setup-rails-db-safety`
Add strong_migrations and database_consistency for database safety.

### Step 5: `/setup-rails-testing`
Add SimpleCov with branch coverage tracking.

### Step 6: `/setup-rails-ci`
Generate `.github/workflows/ci.yml` from the Gemfile.

## After Completion

Summarize what was set up and remind the user to run:

```bash
bundle install
bundle exec tapioca init
bundle exec tapioca gems
bundle exec tapioca dsl
```

## Important Notes

- Each step is idempotent — if gems or config already exist, they are skipped, not duplicated.
- The execution order matters: `/setup-rubocop` must run before `/setup-sorbet` (rubocop-sorbet requires RuboCop). `/setup-rails-ci` must run last because it reads the Gemfile to determine which CI steps to generate.
- This skill focuses on Rails-specific tooling. For Nix infrastructure (git-hooks, nix lint, shell lint, secrets scan, commitlint, etc.), use the corresponding `/setup-*` skills separately.
- For the pre-commit `ruby-lint` hook in `flake.nix`, the user should configure it manually since it depends on the monorepo structure and Nix LD paths.
