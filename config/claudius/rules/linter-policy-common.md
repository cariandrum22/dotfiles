## Linter and Formatter Configuration Policy - Common Principles

### General Guidelines

1. **Prefer Modern Unified Tools**: Always choose modern, unified tools that combine multiple functions (linting, formatting, import sorting) over legacy single-purpose tools.

2. **Default to Strictest Settings**: Start with the strictest configuration and relax rules only when explicitly justified by the project requirements.

3. **Consistency Over Personal Preference**: Use community standards and widely-adopted style guides rather than custom configurations.

4. **Automation First**: All tools must support:
   - CLI execution for CI/CD integration
   - Auto-fix capabilities where available
   - Pre-commit hook integration

5. **Performance Matters**: Prefer tools written in compiled languages (Rust, Go) over interpreted languages for better performance in large codebases.

### Functional Programming Principles

**STRONGLY RECOMMENDED**: Adopt functional programming patterns where the language supports them effectively.

1. **Immutability First**:
   - Prefer immutable data structures and values
   - Avoid mutation of state except where performance requires it
   - Use const/final/readonly declarations by default

2. **Pure Functions**:
   - Functions should return the same output for the same input
   - Avoid side effects (I/O, mutation, random values) in core logic
   - Separate pure computation from side effects

3. **Function Composition**:
   - Build complex operations by composing simple functions
   - Prefer higher-order functions (map, filter, reduce) over loops
   - Use function chaining and pipelines where idiomatic

4. **Explicit Over Implicit**:
   - Make data flow explicit through function parameters and returns
   - Avoid hidden state and global variables
   - Prefer dependency injection over implicit dependencies

5. **Type Safety**:
   - Use strong typing to encode business logic
   - Leverage algebraic data types (Option/Maybe, Result/Either)
   - Make illegal states unrepresentable

### Language Suitability for Functional Programming

- **Naturally Functional**: Nix, Haskell, Elm, Clojure, F#
- **Strong Support**: Rust, TypeScript, Scala, Kotlin
- **Moderate Support**: JavaScript, Python, Swift
- **Limited Support**: Go, Java, C++
- **Not Recommended**: C, Assembly

When a language has limited functional support, prioritize clarity and idiomatic patterns over forcing functional paradigms.

### Required Tool Capabilities

Every linter/formatter configuration MUST include:
- **Linting**: Static code analysis for potential errors
- **Formatting**: Code style enforcement with auto-fix
- **Type Checking**: Where applicable to the language
- **Import Sorting**: Automated import organization

### Configuration Files

- Always create configuration files in the project root
- Use the tool's native configuration format (`.json`, `.toml`, `.yaml`)
- Include inline comments explaining non-obvious settings
- Version control all configuration files

### Integration Requirements

- **Editor Integration**: Provide setup instructions for VSCode and vim/neovim
- **CI/CD**: Include GitHub Actions or similar CI configuration
- **Pre-commit Hooks**: Set up pre-commit hooks to run before each commit
- **Documentation**: Add linting/formatting commands to README or CLAUDE.md

### Exclusions

Standard directories to exclude from all linters/formatters:
- `node_modules/`, `venv/`, `.venv/`, `vendor/`
- Build outputs: `dist/`, `build/`, `target/`, `out/`
- Cache directories: `.cache/`, `.pytest_cache/`, `.mypy_cache/`
- Version control: `.git/`
- IDE directories: `.vscode/`, `.idea/`

### Supported Language Policies

| Language | Policy File |
| --- | --- |
| Go | `linter-policy-go.md` |
| Haskell | `linter-policy-haskell.md` |
| JavaScript / TypeScript | `linter-policy-javascript-typescript.md` |
| Just | `linter-policy-just.md` |
| Nix | `linter-policy-nix.md` |
| PureScript | `linter-policy-purescript.md` |
| Python | `linter-policy-python.md` |
| Ruby | `linter-policy-ruby.md` |
| Rust | `linter-policy-rust.md` |
| Tamarin | `linter-policy-tamarin.md` |
| Terraform | `linter-policy-terraform.md` |
| TLA+ | `linter-policy-tlaplus.md` |

### Gap Tracking

- Languages without dedicated policies (for example, Java, Kotlin, C#, Scala) require new documents before project adoption.
- Update this section whenever a new language policy is introduced to maintain a single source of truth.
