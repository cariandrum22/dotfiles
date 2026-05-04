## CI Policy - GitHub Actions

> This document extends the requirements defined in `ci-policy-common.md`. Review that baseline first, then apply the GitHub Actions specific guidance below.

### Local Testing Requirements

1. **act** (REQUIRED - Local GitHub Actions Runner)
   - Test workflows locally before pushing
   - Install: `brew install act` or `curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash`
   - Documentation: https://github.com/nektos/act

### Act Configuration

Create `.actrc`:
```bash
# Default Docker image for runners
-P ubuntu-latest=ghcr.io/catthehacker/ubuntu:act-latest
-P ubuntu-22.04=ghcr.io/catthehacker/ubuntu:act-22.04
-P ubuntu-20.04=ghcr.io/catthehacker/ubuntu:act-20.04

# Use Docker instead of Podman
--container-daemon-socket unix:///var/run/docker.sock

# Reuse containers for speed
--reuse

# Default workflow file
--workflows .github/workflows/
```

Create `.secrets` (never commit):
```bash
# GitHub secrets for local testing
GITHUB_TOKEN=your_github_token_here
NPM_TOKEN=your_npm_token_here
AWS_ACCESS_KEY_ID=your_aws_key_here
AWS_SECRET_ACCESS_KEY=your_aws_secret_here
```

### Workflow Structure Best Practices

```yaml
name: CI Pipeline

# Trigger configuration
on:
  push:
    branches: [main, develop]
    paths-ignore:
      - '**.md'
      - 'docs/**'
  pull_request:
    types: [opened, synchronize, reopened]
  workflow_dispatch:
    inputs:
      debug_enabled:
        type: boolean
        description: 'Run with tmate debugging'
        required: false
        default: false

# Global environment variables
env:
  NODE_VERSION: '20'
  PYTHON_VERSION: '3.11'

# Concurrency control
concurrency:
  group: ${{ github.workflow }}-${{ github.event.pull_request.number || github.ref }}
  cancel-in-progress: true

jobs:
  # Job for dependency caching and setup
  setup:
    runs-on: ubuntu-latest
    outputs:
      cache-key: ${{ steps.cache-keys.outputs.key }}
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0  # For proper git history

      - name: Generate cache keys
        id: cache-keys
        run: |
          echo "key=${{ runner.os }}-${{ hashFiles('**/package-lock.json', '**/yarn.lock', '**/pnpm-lock.yaml') }}" >> $GITHUB_OUTPUT

  # Parallel linting jobs
  lint:
    needs: setup
    runs-on: ubuntu-latest
    strategy:
      matrix:
        linter: [eslint, prettier, stylelint]
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: 'npm'
          
      - name: Install dependencies
        run: npm ci --prefer-offline --no-audit
        
      - name: Run ${{ matrix.linter }}
        run: npm run lint:${{ matrix.linter }}

  # Security scanning
  security:
    runs-on: ubuntu-latest
    permissions:
      security-events: write
    steps:
      - uses: actions/checkout@v4
      
      - name: Run CodeQL Analysis
        uses: github/codeql-action/analyze@v3
        
      - name: Run Trivy vulnerability scanner
        uses: aquasecurity/trivy-action@master
        with:
          scan-type: 'fs'
          scan-ref: '.'
          format: 'sarif'
          output: 'trivy-results.sarif'
          
      - name: Upload Trivy results
        uses: github/codeql-action/upload-sarif@v3
        if: always()
        with:
          sarif_file: 'trivy-results.sarif'

  # Test job with coverage
  test:
    needs: lint
    runs-on: ${{ matrix.os }}
    strategy:
      fail-fast: false
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        node: [18, 20]
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup Node.js
        uses: actions/setup-node@v4
        with:
          node-version: ${{ matrix.node }}
          cache: 'npm'
          
      - name: Install dependencies
        run: npm ci
        
      - name: Run tests with coverage
        run: npm run test:coverage
        env:
          CI: true
          
      - name: Upload coverage
        uses: codecov/codecov-action@v3
        if: matrix.os == 'ubuntu-latest' && matrix.node == '20'
        with:
          token: ${{ secrets.CODECOV_TOKEN }}
          fail_ci_if_error: true

  # Build artifacts
  build:
    needs: test
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Setup build environment
        uses: actions/setup-node@v4
        with:
          node-version: ${{ env.NODE_VERSION }}
          cache: 'npm'
          
      - name: Install and build
        run: |
          npm ci
          npm run build
          
      - name: Upload artifacts
        uses: actions/upload-artifact@v4
        with:
          name: build-artifacts
          path: dist/
          retention-days: 7
          
  # Deployment job (protected)
  deploy:
    if: github.ref == 'refs/heads/main' && github.event_name == 'push'
    needs: [lint, security, test, build]
    runs-on: ubuntu-latest
    environment:
      name: production
      url: https://app.example.com
    steps:
      - uses: actions/checkout@v4
      
      - name: Download artifacts
        uses: actions/download-artifact@v4
        with:
          name: build-artifacts
          path: dist/
          
      - name: Deploy to production
        run: |
          echo "Deploying to production..."
          # Actual deployment commands here
```

### Local Testing Commands

```bash
# List available workflows
act -l

# Run default push event
act

# Run specific workflow
act -W .github/workflows/ci.yml

# Run specific job
act -j test

# Run with specific event
act pull_request

# Run with secrets file
act --secret-file .secrets

# Run with debug output
act --verbose

# Run specific job with matrix
act -j test --matrix os:ubuntu-latest

# Dry run (show what would run)
act -n

# Run with custom event payload
act push -e event.json

# Run with environment variables
act --env-file .env.test
```

### Security Best Practices

1. **Secrets Management**:
   ```yaml
   - name: Secure step
     env:
       SECRET_KEY: ${{ secrets.SECRET_KEY }}
     run: |
       # Never echo secrets
       # Use masked values in logs
       echo "::add-mask::$SECRET_KEY"
   ```

2. **Permissions Control**:
   ```yaml
   permissions:
     contents: read
     issues: write
     pull-requests: write
     security-events: write
   ```

3. **Third-party Actions**:
   ```yaml
   # Always pin to specific commit SHA
   - uses: actions/checkout@b4ffde65f46336ab88eb53be808477a3936bae11 # v4.1.1
   ```

### Reusable Workflows

Create `.github/workflows/reusable-tests.yml`:
```yaml
name: Reusable Test Workflow

on:
  workflow_call:
    inputs:
      node-version:
        required: false
        type: string
        default: '20'
    secrets:
      npm-token:
        required: true

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: ${{ inputs.node-version }}
      - run: npm ci
        env:
          NPM_TOKEN: ${{ secrets.npm-token }}
      - run: npm test
```

Use in main workflow:
```yaml
jobs:
  test:
    uses: ./.github/workflows/reusable-tests.yml
    with:
      node-version: '20'
    secrets:
      npm-token: ${{ secrets.NPM_TOKEN }}
```

### Optimization Strategies

1. **Dependency Caching**:
   ```yaml
   - name: Cache dependencies
     uses: actions/cache@v4
     with:
       path: |
         ~/.npm
         ~/.cache
         node_modules
       key: ${{ runner.os }}-deps-${{ hashFiles('**/package-lock.json') }}
       restore-keys: |
         ${{ runner.os }}-deps-
   ```

2. **Docker Layer Caching**:
   ```yaml
   - name: Set up Docker Buildx
     uses: docker/setup-buildx-action@v3
     
   - name: Build and cache
     uses: docker/build-push-action@v5
     with:
       context: .
       push: false
       tags: myapp:latest
       cache-from: type=gha
       cache-to: type=gha,mode=max
   ```

3. **Conditional Steps**:
   ```yaml
   - name: Deploy only on main
     if: github.ref == 'refs/heads/main' && github.event_name == 'push'
     run: ./deploy.sh
   ```

### Debugging Workflows

1. **Enable Debug Logging**:
   ```yaml
   - name: Enable debug logging
     run: |
       echo "ACTIONS_STEP_DEBUG=true" >> $GITHUB_ENV
       echo "ACTIONS_RUNNER_DEBUG=true" >> $GITHUB_ENV
   ```

2. **Interactive Debugging**:
   ```yaml
   - name: Setup tmate session
     if: ${{ github.event_name == 'workflow_dispatch' && inputs.debug_enabled }}
     uses: mxschmitt/action-tmate@v3
     timeout-minutes: 15
   ```

### Act Troubleshooting

```bash
# Common issues and solutions

# Issue: Container not found
act --pull

# Issue: Secrets not working
act --secret-file .secrets --insecure-secrets

# Issue: Path not found
act --bind

# Issue: Network issues
act --network host

# Clean containers
docker rm $(docker ps -a -q --filter="name=act-")

# Use different platform
act --platform ubuntu-latest=ghcr.io/catthehacker/ubuntu:act-latest
```

### Required Files

Create `.github/dependabot.yml`:
```yaml
version: 2
updates:
  - package-ecosystem: "github-actions"
    directory: "/"
    schedule:
      interval: "weekly"
    reviewers:
      - "team-name"
    
  - package-ecosystem: "npm"
    directory: "/"
    schedule:
      interval: "weekly"
    versioning-strategy: increase
```

Create `.github/workflows/security.yml`:
```yaml
name: Security Audit

on:
  schedule:
    - cron: '0 0 * * 1'  # Weekly on Monday
  workflow_dispatch:

jobs:
  audit:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: npm audit --production
      - run: npx snyk test
```
