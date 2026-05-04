# Setup Sorbet

Add Sorbet type checking with Tapioca RBI generation and rubocop-sorbet linting to the project.

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

If `.rubocop.yml` exists, add `rubocop-sorbet` to the `plugins` list. If it does not exist, tell the user to run `/setup-rubocop` first.

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
