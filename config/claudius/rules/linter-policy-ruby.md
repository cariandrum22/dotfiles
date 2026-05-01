## Linter and Formatter Configuration Policy - Ruby

### Required Tools

1. **RuboCop** (REQUIRED - Linter/Formatter)
   - Comprehensive Ruby code analyzer and formatter
   - Run: `rubocop` or `rubocop -a` (safe auto-correct)
   - Install: `gem install rubocop` or add to Gemfile

2. **Sorbet** (REQUIRED - Type Checker)
   - Gradual type system for Ruby
   - Run: `srb tc`
   - Install: `gem install sorbet sorbet-runtime`

3. **Brakeman** (REQUIRED - Security Scanner)
   - Static analysis security vulnerability scanner
   - Run: `brakeman`
   - Install: `gem install brakeman`

4. **Reek** (RECOMMENDED - Code Smell Detector)
   - Detects code smells in Ruby
   - Run: `reek`
   - Install: `gem install reek`

5. **Solargraph** (REQUIRED - Language Server)
   - Language server for IDE support
   - Install: `gem install solargraph`

### RuboCop Configuration

Create `.rubocop.yml`:
```yaml
require:
  - rubocop-performance
  - rubocop-rspec
  - rubocop-rails  # If Rails project
  - rubocop-sorbet

AllCops:
  NewCops: enable
  TargetRubyVersion: 3.2
  SuggestExtensions: false
  Exclude:
    - 'bin/**/*'
    - 'db/schema.rb'
    - 'db/migrate/*'
    - 'vendor/**/*'
    - 'node_modules/**/*'

# Layout and Style - Balanced for readability
Layout/LineLength:
  Max: 120
  AllowedPatterns:
    - '\A\s*#'  # Allow long comments

Layout/MultilineMethodCallIndentation:
  EnforcedStyle: indented

Style/Documentation:
  Enabled: true
  Exclude:
    - 'spec/**/*'
    - 'test/**/*'
    - 'db/migrate/*'

Style/StringLiterals:
  EnforcedStyle: double_quotes
  ConsistentQuotesInMultiline: true

Style/FrozenStringLiteralComment:
  Enabled: true
  EnforcedStyle: always
  Exclude:
    - 'Gemfile'
    - 'Rakefile'

# Metrics - Balanced for maintainability
Metrics/MethodLength:
  Max: 20
  CountAsOne: ['array', 'hash', 'heredoc']

Metrics/ClassLength:
  Max: 200
  CountAsOne: ['array', 'hash', 'heredoc']

Metrics/AbcSize:
  Max: 20

Metrics/CyclomaticComplexity:
  Max: 10

Metrics/PerceivedComplexity:
  Max: 10

Metrics/BlockLength:
  Max: 25
  Exclude:
    - 'spec/**/*'
    - 'test/**/*'
    - '*.gemspec'
  AllowedMethods:
    - configure
    - context
    - describe
    - it
    - let
    - define

# Security and Safety
Security:
  Enabled: true

Lint/UselessAssignment:
  Enabled: true

# Performance
Performance:
  Enabled: true

Performance/CollectionLiteralInLoop:
  Enabled: true

# Modern Ruby Features
Style/HashSyntax:
  EnforcedStyle: ruby19_no_mixed_keys
  EnforcedShorthandSyntax: always

Style/NumericLiterals:
  Enabled: true
  MinDigits: 5

# Functional Programming Encouragement
Style/CollectionMethods:
  Enabled: true
  PreferredMethods:
    collect: 'map'
    collect!: 'map!'
    inject: 'reduce'
    detect: 'find'
    select: 'filter'

Style/SymbolProc:
  Enabled: true

Style/BlockDelimiters:
  EnforcedStyle: semantic
  AllowedMethods:
    - lambda
    - proc
    - it

# Sorbet Integration
Sorbet/FalseSigil:
  Enabled: true

Sorbet/TrueSigil:
  Enabled: true
  RequireSigilOnAllFiles: false  # Gradual adoption

Sorbet/ValidSigil:
  Enabled: true

Sorbet/EnforceSigilOrder:
  Enabled: true

# Rails Specific (if applicable)
Rails:
  Enabled: false  # Set to true for Rails projects

# RSpec Specific (if applicable)
RSpec:
  Enabled: false  # Set to true for RSpec projects
```

### Sorbet Configuration

Create `sorbet/config`:
```
--dir
.
--ignore
vendor/
--ignore
spec/
--ignore
test/
--enable-experimental-requires-ancestor
```

Add to files gradually:
```ruby
# typed: false -> typed: true -> typed: strict

# typed: strict
# frozen_string_literal: true

require "sorbet-runtime"

class User
  extend T::Sig

  sig { params(name: String, age: Integer).void }
  def initialize(name, age)
    @name = name
    @age = age
  end

  sig { returns(String) }
  attr_reader :name

  sig { returns(Integer) }
  attr_reader :age

  sig { params(other: User).returns(T::Boolean) }
  def older_than?(other)
    age > other.age
  end
end
```

### Brakeman Configuration

Create `.brakeman.yml`:
```yaml
:skip_checks:
  # Only skip with justification
  # - CheckName

:check_arguments: true

:safe_methods:
  # Add project-specific safe methods

:ignore_file: .brakeman.ignore

:output_formats:
  - html
  - json
  - text

:confidence_level: 3  # 1=High, 2=Medium, 3=Weak

:interactive_ignore: false

:quiet: false

:report_routes: true
:check_path_traversal: true
:check_sql_injection: true
```

### Reek Configuration

Create `.reek.yml`:
```yaml
detectors:
  TooManyStatements:
    enabled: true
    max_statements: 10
    
  TooManyInstanceVariables:
    enabled: true
    max_instance_variables: 5
    
  FeatureEnvy:
    enabled: true
    
  DuplicateMethodCall:
    enabled: true
    max_calls: 2
    
  LongParameterList:
    enabled: true
    max_params: 4
    
  NestedIterators:
    enabled: true
    max_allowed_nesting: 2
    
  UnusedParameters:
    enabled: true
    
  UtilityFunction:
    enabled: true
    public_methods_only: true

directories:
  "app/controllers":
    TooManyInstanceVariables:
      max_instance_variables: 10  # Controllers may need more
      
  "spec":
    enabled: false  # Don't analyze specs

exclude_paths:
  - vendor
  - db/migrate
```

### Pre-commit Configuration

Create `.pre-commit-config.yaml`:
```yaml
repos:
  - repo: local
    hooks:
      - id: rubocop
        name: RuboCop
        entry: bundle exec rubocop
        language: system
        types: [ruby]
        require_serial: true
        
      - id: sorbet
        name: Sorbet Type Check
        entry: bundle exec srb tc
        language: system
        types: [ruby]
        pass_filenames: false
        
      - id: brakeman
        name: Brakeman Security Scan
        entry: bundle exec brakeman -q --no-pager
        language: system
        types: [ruby]
        pass_filenames: false
        
      - id: reek
        name: Reek Code Smell Check
        entry: bundle exec reek
        language: system
        types: [ruby]
```

### Functional Programming Patterns

```ruby
# frozen_string_literal: true
# typed: strict

require "sorbet-runtime"

module FunctionalPatterns
  extend T::Sig

  # Prefer immutable transformations
  sig { params(numbers: T::Array[Integer]).returns(T::Array[Integer]) }
  def self.double_all(numbers)
    numbers.map { |n| n * 2 }
  end

  # Use higher-order functions
  sig { params(multiplier: Integer).returns(T.proc.params(n: Integer).returns(Integer)) }
  def self.multiplier(multiplier)
    ->(n) { n * multiplier }
  end

  # Leverage Enumerable methods
  sig { params(users: T::Array[User], min_age: Integer).returns(T::Array[String]) }
  def self.adult_names(users, min_age: 18)
    users
      .filter { |user| user.age >= min_age }
      .map(&:name)
      .sort
  end

  # Use Result pattern for error handling
  Result = T.type_alias { T.any([T::Hash[Symbol, T.untyped], T::Hash[Symbol, String]]) }

  sig { params(value: String).returns(Result) }
  def self.parse_integer(value)
    { ok: Integer(value) }
  rescue ArgumentError => e
    { error: e.message }
  end
end
```

### Testing Configuration

For RSpec, create `.rspec`:
```
--color
--require spec_helper
--format documentation
--order random
```

Add to `spec/spec_helper.rb`:
```ruby
# frozen_string_literal: true
# typed: false

require "simplecov"
SimpleCov.start do
  add_filter "/spec/"
  add_filter "/vendor/"
  minimum_coverage 90
end

RSpec.configure do |config|
  config.expect_with :rspec do |expectations|
    expectations.include_chain_clauses_in_custom_matcher_descriptions = true
  end

  config.mock_with :rspec do |mocks|
    mocks.verify_partial_doubles = true
  end

  config.shared_context_metadata_behavior = :apply_to_host_groups
  config.filter_run_when_matching :focus
  config.disable_monkey_patching!
  config.warnings = true
  config.order = :random
  Kernel.srand config.seed
end
```

### CI/CD Integration

GitHub Actions example:
```yaml
name: Ruby CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        ruby-version: ['3.1', '3.2', '3.3']
    
    steps:
    - uses: actions/checkout@v4
    
    - name: Set up Ruby
      uses: ruby/setup-ruby@v1
      with:
        ruby-version: ${{ matrix.ruby-version }}
        bundler-cache: true
    
    - name: Run RuboCop
      run: bundle exec rubocop
      
    - name: Run Sorbet
      run: bundle exec srb tc
      
    - name: Run Brakeman
      run: bundle exec brakeman -q
      
    - name: Run Reek
      run: bundle exec reek
      
    - name: Run tests
      run: bundle exec rspec
```

### Editor Integration

**VSCode**:
```json
{
  "[ruby]": {
    "editor.defaultFormatter": "misogi.ruby-rubocop",
    "editor.formatOnSave": true,
    "editor.formatOnType": true,
    "editor.tabSize": 2
  },
  "solargraph.diagnostics": true,
  "solargraph.formatting": false,
  "ruby.codeCompletion": "rcodetools",
  "ruby.intellisense": "rubyLocate",
  "ruby.useBundler": true,
  "ruby.useLanguageServer": true,
  "ruby.lint": {
    "rubocop": {
      "useBundler": true
    }
  }
}
```

### Gemfile Setup

```ruby
source "https://rubygems.org"

# Runtime dependencies
gem "sorbet-runtime"

group :development, :test do
  gem "rubocop", require: false
  gem "rubocop-performance", require: false
  gem "rubocop-rspec", require: false
  gem "rubocop-sorbet", require: false
  gem "sorbet", require: false
  gem "brakeman", require: false
  gem "reek", require: false
  gem "solargraph", require: false
  gem "simplecov", require: false
end
```

### Best Practices

1. **Gradual Type Adoption**: Start with `typed: false` and gradually increase
2. **Performance Monitoring**: Use `rubocop-performance` rules
3. **Security First**: Never skip Brakeman warnings without review
4. **Code Smells**: Address Reek warnings to improve design
5. **Documentation**: Use YARD format for public APIs
6. **Immutability**: Prefer functional transformations over mutations
7. **Error Handling**: Use Result pattern or exceptions consistently