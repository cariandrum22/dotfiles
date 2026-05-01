## Build Tool Policy - Just

### Document Map
- [Mandatory Setup](#mandatory-setup)
- [Mandatory Recipes](#mandatory-recipes)
- [Nix Flake Integration](#nix-flake-integration)
- [Operational Guardrails](#operational-guardrails)
- [Reference Recipe Library](#reference-recipe-library)

## Mandatory Setup

### Scope and Intent

`just` is a modern command runner that provides a more user-friendly alternative to `make`. This policy ensures consistent, secure, and maintainable task automation across projects.

### Required Setup

1. **Installation**:
   ```bash
   # macOS
   brew install just
   
   # Linux/WSL
   curl --proto '=https' --tlsv1.2 -sSf https://just.systems/install.sh | bash -s -- --to ~/bin
   
   # Cargo
   cargo install just
   
   # Verify installation
   just --version
   ```

2. **Shell Configuration**:
   ```bash
   # Add to .bashrc/.zshrc for completions
   eval "$(just --completions bash)"  # or zsh/fish
   ```

### Baseline Justfile (Mandatory)

```just
# Justfile - Project task automation
# vim: set ft=just :

# Load environment variables
set dotenv-load := true

# Set shell for Windows compatibility
set windows-shell := ["powershell.exe", "-NoLogo", "-Command"]

# Default shell options for safety
set shell := ["bash", "-euo", "pipefail", "-c"]

# Allow positional arguments
set positional-arguments := true

# Export all just variables as environment variables
set export := true

# Project metadata
project_name := "my-project"
version := `cat VERSION 2>/dev/null || echo "0.0.0"`

# Colors for output (cross-platform)
RED := if os() == "windows" { "" } else { "\033[0;31m" }
GREEN := if os() == "windows" { "" } else { "\033[0;32m" }
YELLOW := if os() == "windows" { "" } else { "\033[0;33m" }
BLUE := if os() == "windows" { "" } else { "\033[0;34m" }
RESET := if os() == "windows" { "" } else { "\033[0m" }

# =========================================================================
# Mandatory Recipes (baseline required for every project)
# =========================================================================

# Default recipe to display help
default:
    @just --list --unsorted

# Display detailed help
help:
    @echo "{{BLUE}}{{project_name}} v{{version}}{{RESET}}"
    @echo ""
    @echo "{{GREEN}}Available commands:{{RESET}}"
    @just --list --unsorted
    @echo ""
    @echo "{{YELLOW}}Use 'just <command> --help' for more information{{RESET}}"

# =========================================================================
# Reference Recipes (extend only when needed)
# =========================================================================

# ============================================================================
# Development Commands

# Install all dependencies
install: install-deps install-tools install-hooks

# Install project dependencies
@install-deps:
    echo "{{BLUE}}Installing dependencies...{{RESET}}"
    if [ -f "package.json" ]; then npm ci; fi
    if [ -f "requirements.txt" ]; then pip install -r requirements.txt; fi
    if [ -f "Cargo.toml" ]; then cargo build; fi
    echo "{{GREEN}}✓ Dependencies installed{{RESET}}"

# Install development tools
install-tools:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "{{BLUE}}Installing development tools...{{RESET}}"
    
    tools=(
        "ripgrep"
        "fd"
        "bat"
        "hyperfine"
    )
    
    for tool in "${tools[@]}"; do
        if ! command -v "$tool" &> /dev/null; then
            echo "Installing $tool..."
            cargo install "$tool" || brew install "$tool" || echo "Failed to install $tool"
        fi
    done
    
    echo "{{GREEN}}✓ Tools installed{{RESET}}"

# Install git hooks
install-hooks:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "{{BLUE}}Installing git hooks...{{RESET}}"
    
    mkdir -p .git/hooks
    
    # Pre-commit hook
    cat > .git/hooks/pre-commit << 'EOF'
    #!/usr/bin/env bash
    just lint
    just test
    EOF
    
    chmod +x .git/hooks/pre-commit
    echo "{{GREEN}}✓ Git hooks installed{{RESET}}"

# ============================================================================
# Code Quality Commands
# ============================================================================

# Run all checks (lint, format, test)
check: lint format-check test

# Run linters
lint:
    @echo "{{BLUE}}Running linters...{{RESET}}"
    @just _lint-{{os()}}

# Platform-specific lint commands
_lint-linux:
    @if [ -f ".eslintrc.js" ]; then npm run lint; fi
    @if [ -f "pyproject.toml" ]; then ruff check .; fi
    @if [ -f "Cargo.toml" ]; then cargo clippy -- -D warnings; fi

_lint-macos: _lint-linux  # Same as Linux

_lint-windows:
    @if exist .eslintrc.js (npm run lint)
    @if exist pyproject.toml (ruff check .)
    @if exist Cargo.toml (cargo clippy -- -D warnings)

# Format code
format:
    @echo "{{BLUE}}Formatting code...{{RESET}}"
    @just _run-formatters fix

# Check formatting without changing files
format-check:
    @echo "{{BLUE}}Checking formatting...{{RESET}}"
    @just _run-formatters check

# Internal formatter runner
[private]
_run-formatters action:
    #!/usr/bin/env bash
    set -euo pipefail
    
    if [ -f ".prettierrc" ]; then
        if [ "{{action}}" = "fix" ]; then
            npx prettier --write .
        else
            npx prettier --check .
        fi
    fi
    
    if [ -f "pyproject.toml" ] && grep -q "ruff" pyproject.toml; then
        if [ "{{action}}" = "fix" ]; then
            ruff format .
        else
            ruff format --check .
        fi
    fi
    
    if [ -f "rustfmt.toml" ]; then
        if [ "{{action}}" = "fix" ]; then
            cargo fmt
        else
            cargo fmt -- --check
        fi
    fi

# Run tests
test *args='':
    @echo "{{BLUE}}Running tests...{{RESET}}"
    @just _test-runner {{args}}

# Platform-aware test runner
[private]
_test-runner *args:
    #!/usr/bin/env bash
    set -euo pipefail
    
    if [ -f "package.json" ] && grep -q "test" package.json; then
        npm test {{args}}
    elif [ -f "pytest.ini" ] || [ -f "pyproject.toml" ]; then
        pytest {{args}}
    elif [ -f "Cargo.toml" ]; then
        cargo test {{args}}
    else
        echo "{{YELLOW}}No test runner detected{{RESET}}"
    fi

# Run benchmarks
bench *args='':
    @echo "{{BLUE}}Running benchmarks...{{RESET}}"
    @if [ -f "Cargo.toml" ]; then cargo bench {{args}}; fi
    @if [ -f "package.json" ] && grep -q "bench" package.json; then npm run bench {{args}}; fi

# ============================================================================
# Build Commands
# ============================================================================

# Build the project
build profile='debug':
    @echo "{{BLUE}}Building {{profile}} profile...{{RESET}}"
    @just _build-{{profile}}

# Debug build
[private]
_build-debug:
    @if [ -f "Cargo.toml" ]; then cargo build; fi
    @if [ -f "package.json" ] && grep -q "build" package.json; then npm run build; fi

# Release build
[private]
_build-release:
    @if [ -f "Cargo.toml" ]; then cargo build --release; fi
    @if [ -f "package.json" ] && grep -q "build" package.json; then npm run build -- --production; fi

# Clean build artifacts
clean:
    @echo "{{BLUE}}Cleaning build artifacts...{{RESET}}"
    rm -rf dist/ build/ target/ node_modules/.cache
    @echo "{{GREEN}}✓ Clean complete{{RESET}}"

# ============================================================================
# Development Workflow Commands
# ============================================================================

# Start development server
dev:
    @echo "{{BLUE}}Starting development server...{{RESET}}"
    @just _dev-server

[private]
_dev-server:
    #!/usr/bin/env bash
    if [ -f "package.json" ] && grep -q "dev" package.json; then
        npm run dev
    elif [ -f "Cargo.toml" ]; then
        cargo watch -x run
    elif [ -f "main.py" ]; then
        python main.py
    else
        echo "{{RED}}No development server configuration found{{RESET}}"
        exit 1
    fi

# Watch for changes and run tests
watch:
    @echo "{{BLUE}}Watching for changes...{{RESET}}"
    @just _watch-files

[private]
_watch-files:
    #!/usr/bin/env bash
    if command -v watchexec &> /dev/null; then
        watchexec -e rs,js,py,json,toml,yaml -- just test
    elif command -v entr &> /dev/null; then
        find . -name "*.rs" -o -name "*.js" -o -name "*.py" | entr -c just test
    else
        echo "{{YELLOW}}Please install watchexec or entr{{RESET}}"
        exit 1
    fi

# ============================================================================
# CI/CD Commands
# ============================================================================

# Run CI pipeline locally
ci: clean install check build
    @echo "{{GREEN}}✓ CI pipeline complete{{RESET}}"

# Prepare release
release version:
    @echo "{{BLUE}}Preparing release {{version}}...{{RESET}}"
    @just _validate-version {{version}}
    @just _update-version {{version}}
    @just _tag-release {{version}}

# Validate semantic version
[private]
_validate-version version:
    #!/usr/bin/env bash
    if ! echo "{{version}}" | grep -qE '^v?[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9.-]+)?$'; then
        echo "{{RED}}Invalid version format: {{version}}{{RESET}}"
        echo "Expected: vX.Y.Z or X.Y.Z"
        exit 1
    fi

# Update version in project files
[private]
_update-version version:
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Strip 'v' prefix if present
    version="${{version}#v}"
    
    # Update various version files
    echo "$version" > VERSION
    
    if [ -f "package.json" ]; then
        npm version "$version" --no-git-tag-version
    fi
    
    if [ -f "Cargo.toml" ]; then
        sed -i.bak "s/^version = .*/version = \"$version\"/" Cargo.toml
        rm Cargo.toml.bak
    fi
    
    if [ -f "pyproject.toml" ]; then
        sed -i.bak "s/^version = .*/version = \"$version\"/" pyproject.toml
        rm pyproject.toml.bak
    fi

# Create git tag
[private]
_tag-release version:
    git add -A
    git commit -m "chore: release {{version}}"
    git tag -a "{{version}}" -m "Release {{version}}"
    @echo "{{GREEN}}✓ Tagged release {{version}}{{RESET}}"
    @echo "{{YELLOW}}Run 'git push && git push --tags' to publish{{RESET}}"

# ============================================================================
# Docker Commands
# ============================================================================

# Build Docker image
docker-build tag='latest':
    @echo "{{BLUE}}Building Docker image...{{RESET}}"
    docker build -t {{project_name}}:{{tag}} .

# Run Docker container
docker-run tag='latest' *args='':
    docker run -it --rm {{args}} {{project_name}}:{{tag}}

# Docker compose operations
compose cmd='up' *args='':
    docker-compose {{cmd}} {{args}}

# ============================================================================
# Utility Commands
# ============================================================================

# Generate documentation
docs:
    @echo "{{BLUE}}Generating documentation...{{RESET}}"
    @just _generate-docs

[private]
_generate-docs:
    #!/usr/bin/env bash
    if [ -f "Cargo.toml" ]; then
        cargo doc --no-deps --open
    elif [ -f "package.json" ] && grep -q "docs" package.json; then
        npm run docs
    else
        echo "{{YELLOW}}No documentation generator found{{RESET}}"
    fi

# Show project statistics
stats:
    @echo "{{BLUE}}Project Statistics:{{RESET}}"
    @echo "Lines of code:"
    @tokei --sort lines || cloc . || echo "Install tokei or cloc for statistics"

# Update dependencies
update-deps:
    @echo "{{BLUE}}Updating dependencies...{{RESET}}"
    @if [ -f "package.json" ]; then npm update; npm audit fix; fi
    @if [ -f "Cargo.toml" ]; then cargo update; fi
    @if [ -f "requirements.txt" ]; then pip install --upgrade -r requirements.txt; fi

# ============================================================================
# System Commands
# ============================================================================

# Check system dependencies
check-system:
    @echo "{{BLUE}}Checking system dependencies...{{RESET}}"
    @just _check-commands

[private]
_check-commands:
    #!/usr/bin/env bash
    required_commands=(
        "git"
        "curl"
        "bash"
    )
    
    optional_commands=(
        "docker"
        "node"
        "python3"
        "cargo"
    )
    
    echo "Required commands:"
    for cmd in "${required_commands[@]}"; do
        if command -v "$cmd" &> /dev/null; then
            echo "  {{GREEN}}[OK]{{RESET}} $cmd"
        else
            echo "  {{RED}}✗{{RESET}} $cmd"
        fi
    done
    
    echo ""
    echo "Optional commands:"
    for cmd in "${optional_commands[@]}"; do
        if command -v "$cmd" &> /dev/null; then
            echo "  {{GREEN}}[OK]{{RESET}} $cmd"
        else
            echo "  {{YELLOW}}○{{RESET}} $cmd"
        fi
    done

# ============================================================================
# Aliases and Shortcuts
# ============================================================================

# Aliases for common commands
alias b := build
alias c := check
alias d := dev
alias f := format
alias t := test
alias r := run

# Run the application
run *args='':
    @just build release
    @echo "{{BLUE}}Running application...{{RESET}}"
    @just _run-app {{args}}

[private]
_run-app *args:
    #!/usr/bin/env bash
    if [ -f "target/release/{{project_name}}" ]; then
        ./target/release/{{project_name}} {{args}}
    elif [ -f "dist/index.js" ]; then
        node dist/index.js {{args}}
    elif [ -f "main.py" ]; then
        python main.py {{args}}
    else
        echo "{{RED}}No executable found{{RESET}}"
        exit 1
    fi
```

## Mandatory Recipes

Projects that reference this policy must include the baseline recipes shown above. The following targets are required in every `Justfile`:
- `default`: Provides discoverability by listing available tasks.
- `help`: Emits structured task documentation for human review.
- `install`: Delegates standardized environment bootstrap.
- `install-deps`: Installs language and runtime dependencies using safe defaults.
- `install-tools`: Ensures platform tooling (ripgrep, fd, bat, hyperfine) is present.
- `install-hooks`: Installs the pre-commit hook that enforces linting and tests.

Optional recipes remain in the reference section below and can be enabled incrementally. When extending the baseline, keep the `Mandatory Recipes` comment markers inside the snippet to prevent accidental removal of required targets during merges.

## Nix Flake Integration

Projects that combine Nix Flakes with `just` must treat the flake configuration as the canonical declaration of toolchains, shells, and build inputs. `just` serves as an orchestration layer for workflows that either:
- invoke `nix` entry points to guarantee reproducibility (for example, wrapping `nix develop` or `nix build`), or
- provide operational shims for tasks that are impractical to encode declaratively inside the flake (for example, ad-hoc database resets or editor automation).

### Coordination Rules
- Define all language runtimes, compilers, and external tools inside `flake.nix` to preserve deterministic environments.
- Use `just` recipes to call into the flake rather than duplicating environment setup logic.
- Keep any imperative steps inside recipes idempotent and side-effect aware; the flake remains the single source of truth for dependency resolution.

### Example Wrapper Recipes
```just
# Delegate shell provisioning to Nix Flakes
dev-shell:
    nix develop

# Run formatters inside the flake environment
fmt:
    nix develop --command just fmt-internal

# Internal formatter invoked only from flake-provisioned shell
fmt-internal:
    cargo fmt --all
```

Document in `AGENTS.md` that Nix Flakes hold primacy for environment determinism, whereas `just` is recommended for human-friendly task aliases and multi-step workflows.

## Operational Guardrails

### Best Practices

1. **Recipe Naming**:
   - Use kebab-case for recipe names
   - Group related recipes with common prefixes
   - Keep recipe names short but descriptive
   - Use underscores for private recipes

2. **Documentation**:
   - Add comments above recipes explaining their purpose
   - Use `just --list` friendly descriptions
   - Document required arguments and environment variables

3. **Cross-Platform Compatibility**:
   ```just
   # Platform detection
   os := os()
   arch := arch()
   
   # Conditional commands
   build:
       @just build-{{os}}
   
   build-linux:
       cargo build
   
   build-windows:
       cargo build --target x86_64-pc-windows-msvc
   
   build-macos:
       cargo build --target aarch64-apple-darwin
   ```

4. **Error Handling**:
   ```just
   # Use bash strict mode
   set shell := ["bash", "-euo", "pipefail", "-c"]
   
   # Validate inputs
   deploy env:
       #!/usr/bin/env bash
       if [[ ! "{{env}}" =~ ^(dev|staging|prod)$ ]]; then
           echo "Invalid environment: {{env}}"
           exit 1
       fi
       ./deploy.sh {{env}}
   ```

5. **Environment Variables**:
   ```just
   # Load from .env file
   set dotenv-load := true
   
   # Export all variables
   set export := true
   
   # Use environment variables
   database_url := env_var_or_default("DATABASE_URL", "postgresql://localhost/dev")
   ```

6. **Dependencies Between Recipes**:
   ```just
   # Simple dependency
   test: build
       cargo test
   
   # Multiple dependencies
   release: check test build
       echo "Ready for release"
   
   # Parameterized dependencies
   deploy env: (build "release") (test "--release")
       ./deploy.sh {{env}}
   ```

### Security Considerations

1. **Secret Management**:
   ```just
   # Never hardcode secrets
   deploy:
       #!/usr/bin/env bash
       if [ -z "$API_KEY" ]; then
           echo "Error: API_KEY not set"
           exit 1
       fi
       curl -H "Authorization: Bearer $API_KEY" ...
   ```

2. **Input Validation**:
   ```just
   # Validate and sanitize inputs
   run-sql query:
       #!/usr/bin/env bash
       # Escape special characters
       escaped_query=$(printf '%q' "{{query}}")
       psql -c "$escaped_query"
   ```

3. **Safe File Operations**:
   ```just
   # Use quotes for file paths
   backup dir:
       tar -czf "backup-$(date +%Y%m%d).tar.gz" "{{dir}}"
   ```

### Integration Examples

1. **Git Integration**:
   ```just
   # Git workflow helpers
   feature name:
       git checkout -b feature/{{name}}
       git push -u origin feature/{{name}}
   
   pr:
       @gh pr create --fill || echo "Install GitHub CLI for PR creation"
   ```

2. **CI/CD Integration**:
   ```just
   # GitHub Actions local testing
   ci-local:
       act -P ubuntu-latest=ghcr.io/catthehacker/ubuntu:act-latest
   
   # GitLab CI local testing
   ci-gitlab:
       gitlab-runner exec docker test
   ```

3. **Docker Integration**:
   ```just
   # Docker compose helpers
   up services='':
       docker-compose up -d {{services}}
   
   logs service='':
       docker-compose logs -f {{service}}
   
   shell service:
       docker-compose exec {{service}} /bin/bash
   ```

### Editor Support

**VSCode**: Install "just" extension for syntax highlighting
**Vim**: Add to vimrc:
```vim
au BufNewFile,BufRead justfile,Justfile,*.just set ft=just
```

### Common Patterns

1. **Multi-Environment Deployment**:
   ```just
   deploy env:
       @just _validate-env {{env}}
       @just _build-{{env}}
       @just _deploy-{{env}}
   
   _validate-env env:
       #!/usr/bin/env bash
       case "{{env}}" in
           dev|staging|prod) ;;
           *) echo "Invalid env: {{env}}"; exit 1 ;;
       esac
   ```

2. **Incremental Builds**:
   ```just
   # Check if rebuild needed
   build:
       #!/usr/bin/env bash
       if [ "src" -nt "build" ]; then
           echo "Rebuilding..."
           make build
       else
           echo "Build up to date"
       fi
   ```

3. **Task Composition**:
   ```just
   # Compose complex workflows
   daily: update-deps check test
       @echo "Daily tasks complete"
   
   weekly: daily stats security-scan
       @echo "Weekly tasks complete"
   ```

### Anti-Patterns to Avoid

1. ❌ Don't use `make` syntax
2. ❌ Avoid complex bash scripts in recipes (extract to files)
3. ❌ Don't hardcode paths (use variables)
4. ❌ Avoid silent failures (use set -e)
5. ❌ Don't mix tabs and spaces
