# Setup ERB Lint

Add ERB template linting, safe HTML parsing, and formatting to the project.

## Steps

### 1. Add gems to `Gemfile`

Add to the `development, test` group. Do NOT duplicate gems that already exist.

```ruby
gem 'erb_lint', require: false
gem 'better_html', require: false
gem 'htmlbeautifier', require: false
```

### 2. Create `.erb-lint.yml`

Create the file in the Rails root using [erb-lint.yml.template](assets/erb-lint.yml.template).

### 3. Create `config/initializers/better_html.rb`

Create the file using [better_html.rb.template](assets/better_html.rb.template). This enforces safe HTML parsing rules that prevent XSS in ERB templates.

### 4. Add to CI

If `.github/workflows/ci.yml` exists, add the following step to the `lint` job:

```yaml
- name: Lint ERB templates
  run: bundle exec erblint --lint-all
```

### 5. Optionally add pre-commit hook

If the project uses git-hooks.nix, suggest adding an erb-lint hook:

```nix
erb-lint = mkHook "erb-lint" "bash -c 'cd apps/<app-dir> && bundle exec erblint --lint-all'";
```

## Important Notes

- `better_html` enforces that ERB templates produce valid HTML and prevents unsafe interpolation patterns that could lead to XSS.
- `htmlbeautifier` provides consistent ERB formatting. erb_lint can auto-correct formatting issues using it.
- The configuration is strict: accessibility linting is enabled, self-closing tags are enforced, and unsafe HTML patterns are blocked.
