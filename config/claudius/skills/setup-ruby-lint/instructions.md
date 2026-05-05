# Setup Ruby Lint

Add generic RuboCop checks for a Ruby project. This skill standardizes on
RuboCop with performance and thread-safety cops, plus optional Rake and
Minitest support when the repository actually uses those surfaces.

**The argument `$ARGUMENTS` is an optional Ruby target version** such as `3.3`.
If it is omitted, infer it from `.ruby-version`, `.tool-versions`, `mise.toml`,
`Gemfile`, or `gems.rb`. If it still cannot be inferred safely, ask the user.

## Prerequisites

- The project must have a root `Gemfile` or `gems.rb`.
- `bundle exec` must work for the repository.
- For Nix Flake projects: ensure the dev shell provides Ruby and Bundler, but
  preserve any existing Bundler workflow.

## Steps

### 1. Detect optional plugin surfaces

Inspect the repository before editing:

- If it has a `Rakefile` or `.rake` files, add `rubocop-rake`.
- If it already depends on Minitest or has a conventional `test/` tree, add
  `rubocop-minitest`.

Do NOT add Rails cops here. Rails-specific policy belongs in `/setup-rubocop`
or `/setup-rails-api`.

### 2. Add RuboCop gems to the development toolchain

Add these gems to the `development, test` group in `Gemfile` if they are not
already present:

```ruby
gem 'rubocop', require: false
gem 'rubocop-performance', require: false
gem 'rubocop-thread_safety', require: false
```

Add these only when the surface exists:

```ruby
gem 'rubocop-rake', require: false
gem 'rubocop-minitest', require: false
```

Do NOT remove existing project-specific RuboCop plugins unless they are clearly
obsolete and the user asked for cleanup.

### 3. Create or update `.rubocop.yml`

Create `.rubocop.yml` from [rubocop.yml.template](assets/rubocop.yml.template)
if it does not exist.

Replace:

- `__RUBY_VERSION__`
- `__OPTIONAL_PLUGINS__`

If `.rubocop.yml` already exists, merge missing baseline rules and plugins
without removing existing project-specific configuration.

### 4. Create `bin/rubocop` wrapper (optional)

If `bin/rubocop` does not exist, create it:

```ruby
#!/usr/bin/env ruby
# frozen_string_literal: true

ARGV.unshift('--config', File.expand_path('../.rubocop.yml', __dir__))
load Gem.bin_path('rubocop', 'rubocop')
```

Make it executable.

### 5. Determine hook integration method

Check whether `flake.nix` exists in the project root and already contains a
`git-hooks` or `pre-commit-hooks` input.

- If yes: follow Path A (Nix `git-hooks.nix`).
- If no: follow Path B (pre-commit local hooks).

Do NOT apply both paths.

---

### Path A: Nix git-hooks.nix

#### A-1. Add hooks to `flake.nix`

Add the hook from [hooks.nix.template](assets/hooks.nix.template) into the
`hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate
if it already exists.

Hook added:

- `ruby-lint` - runs `bundle exec rubocop --parallel --format quiet`

---

### Path B: Pre-commit local hooks

#### B-1. Add hooks to `.pre-commit-config.yaml`

If `.pre-commit-config.yaml` does not exist, create it.

Add the hook from
[pre-commit-hooks.yaml.template](assets/pre-commit-hooks.yaml.template) to the
`repos` list. Do NOT duplicate if it already exists.

Hook added:

- `ruby-lint` - runs `bundle exec rubocop --parallel --format quiet`

### 6. Validate

Run the following command, or tell the user to run it:

1. `bundle exec rubocop --parallel`

If the repository uses the generated `bin/rubocop` wrapper, `bundle exec
bin/rubocop --parallel` is also acceptable.

## Important Notes

- Hooks and CI must stay check-only. Do NOT add `-A`, `-a`, or other
  auto-correct flags to hooks.
- Keep the generic Ruby policy separate from Rails policy. Use
  `/setup-sorbet` when the repository also wants typed Ruby.
- Prefer a small baseline plugin set plus narrow, explicit additions over a
  large generic RuboCop stack.
