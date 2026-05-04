## Linter and Formatter Configuration Policy - Just

### Required Tools

1. **just --fmt** (REQUIRED - Formatter/Linter)
   - Built-in formatter and validator
   - Format: `just --fmt --unstable`
   - Check: `just --fmt --check --unstable`
   - Version requirement: just >= 1.0.0

2. **pre-commit-just** (REQUIRED - Pre-commit Integration)
   - Automated formatting checks
   - Repository: https://github.com/instrumentl/pre-commit-just

### Justfile Naming Convention

```
project-root/
├── justfile              # Main justfile (no extension)
├── .just/               # Directory for modular just files
│   ├── docker.just      # Docker-related recipes
│   ├── test.just        # Test-related recipes
│   └── deploy.just      # Deployment recipes
└── scripts/
    └── mod.just         # Module-specific justfiles
```

### Pre-commit Configuration

Create `.pre-commit-config.yaml`:
```yaml
repos:
  - repo: https://github.com/instrumentl/pre-commit-just
    rev: main
    hooks:
      - id: format-justfile
        pass_filenames: false  # Critical: must be false
        
  # Additional validation
  - repo: local
    hooks:
      - id: just-validation
        name: Validate justfile syntax
        entry: just --evaluate --justfile
        language: system
        files: ^(.*\.just|justfile)$
        pass_filenames: true
```

### Formatting Standards

The built-in formatter enforces these rules automatically:
- **Indentation**: 4 spaces (enforced)
- **Line endings**: Unix-style (LF)
- **Recipe body indentation**: Consistent with recipe declaration
- **Assignment alignment**: Aligned for readability
- **Command prefix spacing**: Consistent `@` prefix handling

### Style Guidelines

1. **Recipe Organization**:
   ```just
   # ============================================================================
   # Project Metadata
   # ============================================================================
   
   project_name := "my-project"
   version := `cat VERSION 2>/dev/null || echo "0.0.0"`
   
   # ============================================================================
   # Default Recipe
   # ============================================================================
   
   # Display available commands
   default:
       @just --list --unsorted
   
   # ============================================================================
   # Development Commands
   # ============================================================================
   
   # Install dependencies
   install:
       npm install
   
   # Start development server
   dev:
       npm run dev
   ```

2. **Documentation Standards**:
   ```just
   # Every recipe MUST have a comment above it
   # This comment appears in `just --list` output
   build:
       cargo build
   
   # Private recipes start with underscore
   # These don't appear in `just --list`
   _internal-helper:
       echo "Internal use only"
   ```

3. **Naming Conventions**:
   ```just
   # Use kebab-case for recipe names
   format-code:
       cargo fmt
   
   # Use snake_case for variables
   database_url := env_var_or_default("DATABASE_URL", "postgresql://localhost/dev")
   
   # Use SCREAMING_SNAKE_CASE for constants
   DEFAULT_PORT := "3000"
   
   # Namespace related commands with prefixes
   db-migrate:
       diesel migration run
       
   db-rollback:
       diesel migration revert
       
   db-reset: db-rollback db-migrate
   ```

4. **Variable Declaration Order**:
   ```just
   # 1. Metadata variables
   project_name := "app"
   version := "1.0.0"
   
   # 2. System detection
   os := os()
   arch := arch()
   
   # 3. Environment variables
   port := env_var_or_default("PORT", "3000")
   database_url := env_var_or_default("DATABASE_URL", "postgresql://localhost/dev")
   
   # 4. Computed variables
   target := if os == "macos" { "x86_64-apple-darwin" } else { "x86_64-unknown-linux-gnu" }
   
   # 5. Settings
   set dotenv-load := true
   set export := true
   set shell := ["bash", "-euo", "pipefail", "-c"]
   ```

5. **Recipe Dependencies**:
   ```just
   # Simple dependency
   test: build
       cargo test
   
   # Multiple dependencies (space-separated)
   deploy: lint test build
       ./deploy.sh
   
   # Parameterized dependencies
   release version: (check) (test) (build "release")
       git tag {{version}}
   ```

### Security Requirements

1. **No Hardcoded Secrets**:
   ```just
   # [BAD] NEVER do this
   deploy:
       curl -H "Authorization: Bearer abc123" https://api.example.com
   
   # ✅ Use environment variables
   deploy:
       #!/usr/bin/env bash
       set -euo pipefail
       if [ -z "${API_KEY:-}" ]; then
           echo "Error: API_KEY not set"
           exit 1
       fi
       curl -H "Authorization: Bearer $API_KEY" https://api.example.com
   ```

2. **Input Validation**:
   ```just
   # Validate recipe parameters
   deploy env:
       #!/usr/bin/env bash
       set -euo pipefail
       case "{{env}}" in
           dev|staging|prod) ;;
           *) echo "Invalid environment: {{env}}"; exit 1 ;;
       esac
       ./deploy-{{env}}.sh
   ```

3. **Safe Shell Options**:
   ```just
   # Always set safe shell options
   set shell := ["bash", "-euo", "pipefail", "-c"]
   
   # Or for individual recipes
   build:
       #!/usr/bin/env bash
       set -euo pipefail
       cargo build --release
   ```

### CI/CD Integration

GitHub Actions:
```yaml
name: Validate Justfile

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Install just
        uses: extractions/setup-just@v2
        
      - name: Check justfile formatting
        run: just --fmt --check --unstable
        
      - name: Validate justfile syntax
        run: just --evaluate
```

GitLab CI:
```yaml
validate:justfile:
  stage: validate
  image: registry.gitlab.com/juhani/go-semrel-gitlab:v0.21.1
  before_script:
    - wget -qO - https://just.systems/install.sh | bash -s -- --to /usr/local/bin
  script:
    - just --fmt --check --unstable
    - just --evaluate
```

### Editor Integration

**VSCode** (Recommended extensions):
```json
{
  "recommendations": [
    "nefrob.vscode-just",
    "mkhl.vscode-just"
  ],
  "[just]": {
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "mkhl.vscode-just"
  }
}
```

**Vim/Neovim**:
```vim
" Add to .vimrc or init.vim
au BufNewFile,BufRead justfile,Justfile,*.just set ft=just

" Format on save
autocmd BufWritePre justfile,*.just silent! execute '!just --fmt --unstable %'
```

### Validation Commands

Add these recipes to your justfile:
```just
# Validate and format this justfile
meta-check:
    @just --fmt --check --unstable || (just --fmt --unstable && exit 1)

# Show formatted diff without changing file
meta-diff:
    @just --fmt --check --unstable || true

# Format this justfile
meta-format:
    @just --fmt --unstable
    
# Dump justfile as JSON for analysis
meta-dump:
    @just --dump --dump-format json

# List all recipes with descriptions
meta-list:
    @just --list --list-heading $'Available recipes:\n'
```

### Common Anti-patterns

1. **Missing Recipe Documentation**:
   ```just
   # [BAD] No documentation
   build:
       cargo build
   
   # ✅ Good - documented
   # Build the project in debug mode
   build:
       cargo build
   ```

2. **Inconsistent Error Handling**:
   ```just
   # [BAD] No error handling
   deploy:
       ssh server "cd app && git pull"
       ssh server "cd app && npm install"
       ssh server "cd app && npm run build"
   
   # ✅ Good - proper error handling
   deploy:
       #!/usr/bin/env bash
       set -euo pipefail
       ssh server "cd app && git pull && npm install && npm run build"
   ```

3. **Platform-Specific Commands Without Abstraction**:
   ```just
   # [BAD] Breaks on Windows
   clean:
       rm -rf build/
   
   # ✅ Good - cross-platform
   clean:
       {{ if os() == "windows" { "rmdir /s /q build" } else { "rm -rf build/" } }}
   ```

### Modular Justfile Pattern

For large projects, use modular justfiles:

**justfile** (root):
```just
# Import modules
mod docker '.just/docker.just'
mod test '.just/test.just'
mod deploy '.just/deploy.just'

# Main recipes that coordinate modules
all: docker::build test::all deploy::staging
```

**.just/docker.just**:
```just
# Build Docker image
build tag='latest':
    docker build -t myapp:{{tag}} .

# Run Docker container
run tag='latest':
    docker run -it myapp:{{tag}}
```

### Compliance Checklist

- [ ] All justfiles use no file extension or `.just` extension
- [ ] Pre-commit hooks configured with `pre-commit-just`
- [ ] CI/CD includes `just --fmt --check --unstable`
- [ ] All recipes have descriptive comments
- [ ] No hardcoded secrets or credentials
- [ ] Shell safety options enabled (`set shell`)
- [ ] Input validation for all parameterized recipes
- [ ] Cross-platform compatibility considered
- [ ] Error handling implemented in multi-command recipes
- [ ] Modular structure for projects with >20 recipes