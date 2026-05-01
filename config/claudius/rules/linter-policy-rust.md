## Linter and Formatter Configuration Policy - Rust

### Required Tools

1. **rustfmt** (REQUIRED - Formatter)
   - Official Rust formatter
   - Run: `cargo fmt`
   - Check: `cargo fmt -- --check`
   - Installed with Rust toolchain

2. **clippy** (REQUIRED - Linter)
   - Official Rust linter with 500+ lints
   - Run: `cargo clippy`
   - Strict: `cargo clippy -- -D warnings`
   - Installed with: `rustup component add clippy`

3. **cargo-audit** (REQUIRED - Security)
   - Audit dependencies for security vulnerabilities
   - Run: `cargo audit`
   - Install: `cargo install cargo-audit`

4. **rust-analyzer** (REQUIRED - LSP)
   - Language server for IDE support
   - Install: `rustup component add rust-analyzer`

### Configuration

Create `rustfmt.toml`:
```toml
# Stable features only
edition = "2021"
hard_tabs = false
tab_spaces = 4
newline_style = "Unix"
use_small_heuristics = "Default"
reorder_imports = true
reorder_modules = true
remove_nested_parens = true
use_try_shorthand = true
use_field_init_shorthand = true
force_explicit_abi = true

# Unstable features (requires nightly)
# imports_granularity = "Crate"
# group_imports = "StdExternalCrate"
```

Create `.clippy.toml`:
```toml
# Clippy configuration
msrv = "1.70.0"  # Minimum supported Rust version
avoid-breaking-exported-api = true
```

Add to `Cargo.toml`:
```toml
[lints.rust]
unsafe_code = "forbid"
missing_docs = "warn"

[lints.clippy]
all = "warn"
pedantic = "warn"
nursery = "warn"
cargo = "warn"
# Allow some pedantic lints
module_name_repetitions = "allow"
must_use_candidate = "allow"
```

### Pre-push Hook

Create `.cargo/config.toml`:
```toml
[alias]
check-all = [
    "check",
    "fmt -- --check",
    "clippy -- -D warnings",
    "test",
    "doc --no-deps"
]
```

### Shell Script for CI

Create `scripts/check.sh`:
```bash
#!/usr/bin/env bash
set -e

echo "Running cargo fmt..."
cargo fmt -- --check

echo "Running cargo clippy..."
cargo clippy --all-targets --all-features -- -D warnings

echo "Running cargo test..."
cargo test --all-features

echo "Running cargo doc..."
cargo doc --no-deps --all-features

echo "Running cargo audit..."
cargo audit
```

### Editor Integration

**VSCode**:
```json
{
  "rust-analyzer.checkOnSave.command": "clippy",
  "rust-analyzer.rustfmt.enableRangeFormatting": true,
  "[rust]": {
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "rust-lang.rust-analyzer"
  }
}
```

**Neovim**:
- Use rust-analyzer with nvim-lspconfig
- Configure format on save with rustfmt

### CI/CD Integration

GitHub Actions:
```yaml
- name: Install Rust
  uses: dtolnay/rust-toolchain@stable
  with:
    components: rustfmt, clippy

- name: Install cargo-audit
  run: cargo install cargo-audit

- name: Check formatting
  run: cargo fmt -- --check

- name: Run clippy
  run: cargo clippy --all-targets --all-features -- -D warnings

- name: Run tests
  run: cargo test --all-features

- name: Security audit
  run: cargo audit
```

### Workspace Configuration

For workspaces, create root `Cargo.toml`:
```toml
[workspace]
members = ["crate1", "crate2"]
resolver = "2"

[workspace.lints.rust]
unsafe_code = "forbid"

[workspace.lints.clippy]
all = "warn"
pedantic = "warn"
```

### Additional Tools (Optional but Recommended)

1. **cargo-machete** - Find unused dependencies
   - Install: `cargo install cargo-machete`
   - Run: `cargo machete`

2. **cargo-deny** - Lint dependencies
   - Install: `cargo install cargo-deny`
   - Run: `cargo deny check`

3. **cargo-expand** - Expand macros for debugging
   - Install: `cargo install cargo-expand`
   - Run: `cargo expand`

### Common Clippy Allows

For libraries, consider allowing:
```rust
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::must_use_candidate)]
```

For binaries, consider denying:
```rust
#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
```

### Functional Programming Patterns

Rust has **strong functional support** with ownership ensuring safety. Leverage these patterns:

1. **Immutability by Default**:
   ```rust
   // Good: Variables are immutable by default
   let x = 5;
   let y = x + 1;  // x is not modified
   
   // Good: Explicit mutability only when needed
   let mut counter = 0;
   counter += 1;  // Clear mutation intent
   
   // Good: Shadowing for transformation
   let data = "  hello  ";
   let data = data.trim();  // New binding, original unchanged
   ```

2. **Iterator Chains and Combinators**:
   ```rust
   // Good: Functional iterator chains
   let sum: i32 = numbers
       .iter()
       .filter(|&&x| x > 0)
       .map(|&x| x * x)
       .sum();
   
   // Good: Collect into different types
   let squared: Vec<_> = (1..=10)
       .map(|x| x * x)
       .collect();
   
   // Avoid: Manual loops
   let mut sum = 0;
   for &num in &numbers {
       if num > 0 {
           sum += num * num;
       }
   }
   ```

3. **Option and Result Combinators**:
   ```rust
   // Good: Chain Option/Result operations
   let result = input
       .parse::<i32>()
       .ok()
       .filter(|&x| x > 0)
       .map(|x| x * 2)
       .unwrap_or(0);
   
   // Good: Result chaining with ?
   fn process_data(path: &str) -> Result<String, Error> {
       let contents = std::fs::read_to_string(path)?;
       let processed = contents
           .lines()
           .filter(|line| !line.is_empty())
           .collect::<Vec<_>>()
           .join("\n");
       Ok(processed)
   }
   
   // Good: Functional error handling
   let value = get_config_value()
       .map_err(|e| format!("Config error: {}", e))
       .and_then(validate_config)
       .unwrap_or_else(|_| default_config());
   ```

4. **Higher-Order Functions and Closures**:
   ```rust
   // Good: Functions returning closures
   fn make_adder(x: i32) -> impl Fn(i32) -> i32 {
       move |y| x + y
   }
   
   // Good: Generic higher-order functions
   fn apply_twice<F, T>(f: F, x: T) -> T
   where
       F: Fn(T) -> T,
   {
       f(f(x))
   }
   
   // Good: Lazy evaluation with closures
   fn expensive_computation<F: FnOnce() -> T, T>(f: F) -> T {
       // Only compute when needed
       f()
   }
   ```

5. **Pattern Matching for Control Flow**:
   ```rust
   // Good: Exhaustive pattern matching
   match result {
       Ok(value) => process(value),
       Err(e) => handle_error(e),
   }
   
   // Good: if let for single patterns
   if let Some(value) = optional {
       use_value(value);
   }
   
   // Good: Functional match expressions
   let message = match status {
       Status::Ok => "Success",
       Status::Warning(msg) => msg,
       Status::Error(code) => &format!("Error: {}", code),
   };
   ```

6. **Functional Data Structures**:
   ```rust
   use std::collections::HashMap;
   
   // Good: Immutable updates
   let mut map = HashMap::new();
   map.insert("key", "value");
   
   // Good: Functional collection building
   let map: HashMap<_, _> = items
       .into_iter()
       .map(|item| (item.id, item))
       .collect();
   
   // Good: Persistent data structures (with im crate)
   use im::Vector;
   let v1 = Vector::from(vec![1, 2, 3]);
   let v2 = v1.push_back(4);  // v1 unchanged
   ```

7. **Clippy Lints for Functional Style**:
   ```toml
   # In Cargo.toml
   [lints.clippy]
   # Functional style encouragements
   map_unwrap_or = "warn"
   manual_map = "warn"
   option_if_let_else = "warn"
   or_fun_call = "warn"
   
   # Iterator improvements
   needless_collect = "warn"
   filter_map = "warn"
   flat_map_option = "warn"
   
   # Pattern matching
   single_match_else = "warn"
   match_like_matches_macro = "warn"
   ```

8. **Avoid Imperative Patterns**:
   - Prefer `iter()` chains over `for` loops
   - Use `fold` instead of mutable accumulators
   - Leverage `Option`/`Result` instead of null checks
   - Use `match` expressions instead of if-else chains
   - Prefer `const` and `static` for compile-time values