## Code Coverage Measurement and Tracking Policy

### Core Philosophy

Code coverage serves as a quantitative measure of test completeness. This policy mandates comprehensive coverage measurement with mutation testing to ensure both code execution and test effectiveness.

### Coverage Requirements

#### Standard Requirements

**Production Code**:
- Line Coverage: ≥ 90%
- Branch Coverage: ≥ 85%
- Function Coverage: 100%
- Mutation Score: ≥ 80%

**Critical Code** (Authentication, Payment, Security, Data Integrity):
- Line Coverage: 100%
- Branch Coverage: 100%
- Function Coverage: 100%
- Mutation Score: ≥ 90%

**Infrastructure Code** (CI/CD, Deployment Scripts):
- Line Coverage: ≥ 80%
- Branch Coverage: ≥ 75%
- Function Coverage: ≥ 90%

### Ratchet Principle

Coverage metrics must never decrease. Each commit must maintain or improve existing coverage levels.

```yaml
# Example coverage gate configuration
coverage:
  rules:
    - metric: line
      minimum: 90.0
      enforce_increase: true
    - metric: branch
      minimum: 85.0
      enforce_increase: true
    - metric: mutation
      minimum: 80.0
      enforce_increase: true
```

### Quick Compliance Checklist

Prior to submitting a pull request, confirm:
- Line, branch, function, and mutation thresholds meet or exceed the values mandated for the affected module category.
- Coverage reports (`lcov`, HTML, JSON summary) are generated and attached to CI artifacts.
- Mutation testing executed on all modified modules or a documented waiver is filed.
- Ratchet enforcement is validated locally (for example, via `npm run coverage:check`).
- Coverage deltas are summarized in the pull request description, including justification for any temporary exception requests.

### Language-Specific Tooling

#### JavaScript/TypeScript

**Coverage Tools**:
- Test Runner: Vitest or Jest
- Coverage: c8 (V8 native coverage)
- Mutation Testing: Stryker Mutator

**Configuration** (`vitest.config.ts`):
```typescript
export default defineConfig({
  test: {
    coverage: {
      provider: 'v8',
      reporter: ['lcov', 'html', 'text', 'json-summary'],
      exclude: [
        'node_modules/**',
        'dist/**',
        '**/*.d.ts',
        '**/*.config.*',
        '**/generated/**',
        '**/__mocks__/**'
      ],
      lines: 90,
      branches: 85,
      functions: 100,
      statements: 90
    }
  }
});
```

**Stryker Configuration** (`stryker.config.mjs`):
```javascript
export default {
  packageManager: 'npm',
  reporters: ['html', 'clear-text', 'progress', 'dashboard'],
  testRunner: 'vitest',
  coverageAnalysis: 'perTest',
  thresholds: { high: 90, low: 80, break: 75 },
  mutate: [
    'src/**/*.ts',
    '!src/**/*.spec.ts',
    '!src/**/generated/**'
  ]
};
```

#### Python

**Coverage Tools**:
- Coverage: coverage.py with pytest-cov
- Mutation Testing: mutmut

**Configuration** (`pyproject.toml`):
```toml
[tool.coverage.run]
source = ["src"]
branch = true
parallel = true
omit = [
    "*/tests/*",
    "*/migrations/*",
    "*/__pycache__/*",
    "*/generated/*"
]

[tool.coverage.report]
precision = 2
show_missing = true
skip_covered = false
fail_under = 90

[tool.coverage.html]
directory = "coverage_html"

[tool.mutmut]
paths_to_mutate = "src/"
backup = false
runner = "pytest -x"
tests_dir = "tests/"
dict_synonyms = "Struct,NamedStruct"
```

#### Rust

**Coverage Tools**:
- Coverage: cargo-tarpaulin
- Mutation Testing: cargo-mutants

**Configuration** (`tarpaulin.toml`):
```toml
[default]
workspace = true
out = ["Html", "Lcov", "Json"]
exclude-files = ["*/generated/*", "*/tests/*"]
fail-under = 90
print-summary = true
print-uncovered-lines = true

[report]
coveralls = "coveralls-token"
```

**Mutation Testing**:
```bash
# .cargo/mutants.toml
[mutants]
exclude_globs = ["src/generated/**", "tests/**"]
minimum_test_timeout = 30.0
build_timeout = 300.0
```

#### Go

**Coverage Tools**:
- Coverage: Built-in go test -cover
- Mutation Testing: go-mutesting

**Script** (`.agents/products/scripts/go-coverage.sh`):
```bash
#!/usr/bin/env bash
set -euo pipefail

# Run tests with coverage
go test -race -coverprofile=coverage.out -covermode=atomic ./...

# Generate reports
go tool cover -html=coverage.out -o coverage.html
go tool cover -func=coverage.out | tail -n1

# Run mutation testing
go-mutesting --config=.mutesting.yml ./...
```

**Configuration** (`.mutesting.yml`):
```yaml
threshold: 0.8
exclude:
  - "**/generated/**"
  - "**/vendor/**"
  - "**/*_test.go"
blacklist:
  - "github.com/example/package.Func"
```

#### Java

**Coverage Tools**:
- Coverage: JaCoCo
- Mutation Testing: Pitest

**Maven Configuration** (`pom.xml`):
```xml
<plugin>
    <groupId>org.jacoco</groupId>
    <artifactId>jacoco-maven-plugin</artifactId>
    <configuration>
        <excludes>
            <exclude>**/generated/**/*.class</exclude>
            <exclude>**/dto/**/*.class</exclude>
        </excludes>
    </configuration>
    <executions>
        <execution>
            <goals>
                <goal>prepare-agent</goal>
            </goals>
        </execution>
        <execution>
            <id>report</id>
            <phase>test</phase>
            <goals>
                <goal>report</goal>
            </goals>
        </execution>
        <execution>
            <id>check</id>
            <goals>
                <goal>check</goal>
            </goals>
            <configuration>
                <rules>
                    <rule>
                        <element>BUNDLE</element>
                        <limits>
                            <limit>
                                <counter>LINE</counter>
                                <value>COVEREDRATIO</value>
                                <minimum>0.90</minimum>
                            </limit>
                            <limit>
                                <counter>BRANCH</counter>
                                <value>COVEREDRATIO</value>
                                <minimum>0.85</minimum>
                            </limit>
                        </limits>
                    </rule>
                </rules>
            </configuration>
        </execution>
    </executions>
</plugin>

<plugin>
    <groupId>org.pitest</groupId>
    <artifactId>pitest-maven</artifactId>
    <configuration>
        <targetClasses>
            <param>com.example.*</param>
        </targetClasses>
        <excludedClasses>
            <param>com.example.generated.*</param>
        </excludedClasses>
        <mutationThreshold>80</mutationThreshold>
        <outputFormats>
            <outputFormat>HTML</outputFormat>
            <outputFormat>XML</outputFormat>
        </outputFormats>
    </configuration>
</plugin>
```

#### Haskell

**Coverage Tools**:
- Coverage: HPC (Haskell Program Coverage)
- Mutation Testing: MuCheck

**Stack Configuration** (`stack.yaml`):
```yaml
coverage:
  threshold: 90
  exclude:
    - "Generated"
    - "Paths_*"
```

**Usage**:
```bash
# Generate coverage
stack test --coverage

# Generate report
stack hpc report

# Run mutation testing
mucheck -m MyModule -t MyModuleSpec
```

### Exclusion Patterns

Standard exclusions across all languages:
- Generated code (`*/generated/*`, `*.pb.go`, `*.g.dart`)
- Test files and test utilities
- Configuration files
- Migration files
- Vendor/third-party code
- Build artifacts

### Coverage Data Storage

#### Directory Structure
```
.coverage/
├── daily/           # Raw LCOV + mutation results (6 months retention)
│   ├── 2024-01-15/
│   │   ├── lcov.info
│   │   ├── mutation-report.json
│   │   └── summary.json
├── weekly/          # Aggregated metrics (2 years retention)
│   ├── 2024-W03.json
├── monthly/         # Statistical summaries (indefinite retention)
│   ├── 2024-01.csv
└── reports/         # Latest HTML reports
    ├── coverage/
    └── mutation/
```

#### Data Retention Policy

1. **Daily Raw Data**: 6 months
   - Complete LCOV files
   - Full mutation testing results
   - Per-file coverage data
   - Used for: Short-term trend analysis, PR impact assessment

2. **Weekly Aggregates**: 2 years
   - Coverage percentages by module
   - Mutation scores
   - Test execution times
   - Used for: Sprint retrospectives, quarterly reviews

3. **Monthly Summaries**: Indefinite
   - Overall project metrics
   - Coverage trend indicators
   - Critical code coverage status
   - Used for: Long-term quality tracking, technical debt assessment

#### Storage Format Examples

**Daily Summary** (`.coverage/daily/2024-01-15/summary.json`):
```json
{
  "timestamp": "2024-01-15T14:30:00Z",
  "commit": "abc123def",
  "branch": "main",
  "coverage": {
    "line": 92.5,
    "branch": 87.3,
    "function": 100.0,
    "statement": 91.8
  },
  "mutation": {
    "killed": 245,
    "survived": 31,
    "noCoverage": 5,
    "timeout": 2,
    "score": 88.7
  },
  "modules": {
    "src/auth": {
      "line": 100.0,
      "branch": 100.0,
      "mutation": 95.2
    }
  }
}
```

**Weekly Aggregate** (`.coverage/weekly/2024-W03.json`):
```json
{
  "week": "2024-W03",
  "commits": 47,
  "average_coverage": {
    "line": 91.8,
    "branch": 86.9,
    "mutation": 87.3
  },
  "trend": {
    "line": "+0.3",
    "branch": "+0.1",
    "mutation": "+1.2"
  }
}
```

**Monthly Summary** (`.coverage/monthly/2024-01.csv`):
```csv
month,line_coverage,branch_coverage,function_coverage,mutation_score,critical_coverage,commits
2024-01,91.5,86.7,100.0,86.9,100.0,198
```

### CI/CD Integration

#### GitHub Actions Example
```yaml
name: Coverage

on: [push, pull_request]

jobs:
  coverage:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      
      - name: Run tests with coverage
        run: |
          npm run test:coverage
          npm run test:mutation
          
      - name: Check coverage thresholds
        run: |
          npm run coverage:check
          
      - name: Archive coverage data
        run: |
          mkdir -p .coverage/daily/$(date +%Y-%m-%d)
          cp coverage/lcov.info .coverage/daily/$(date +%Y-%m-%d)/
          cp reports/mutation/mutation.json .coverage/daily/$(date +%Y-%m-%d)/
          
      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/lcov.info
          fail_ci_if_error: true
          
      - name: Comment PR
        uses: actions/github-script@v7
        if: github.event_name == 'pull_request'
        with:
          script: |
            const coverage = require('./coverage/coverage-summary.json');
            // Post coverage summary as PR comment
```

### Reporting and Visualization

#### Standard Reports
1. **HTML Reports**: Generated for local development
2. **LCOV Format**: For IDE integration and CI tools
3. **JSON Summary**: For custom dashboards
4. **Markdown Reports**: For PR comments

#### Dashboard Integration
- SonarQube: Full analysis with history
- Codecov/Coveralls: PR integration
- Custom dashboards: Using stored JSON data

### Enforcement Strategy

1. **Pre-commit Hooks**: Run coverage on changed files
2. **PR Checks**: Block merge if coverage decreases
3. **Nightly Builds**: Full coverage with mutation testing
4. **Weekly Reports**: Team coverage metrics
5. **Monthly Reviews**: Coverage trend analysis

### Best Practices

1. **Write Tests First**: TDD approach ensures coverage
2. **Test Behavior, Not Implementation**: Focus on outcomes
3. **Avoid Coverage Gaming**: No empty tests or assertion-free tests
4. **Regular Mutation Testing**: Ensure test quality
5. **Critical Path Focus**: 100% coverage for security/payment code

### Coverage Improvement Workflow

```bash
# 1. Identify uncovered code
npm run coverage:report

# 2. Generate missing test list
npm run coverage:missing > uncovered-files.txt

# 3. Run mutation testing on changed files
npm run mutation:changed

# 4. Validate improvements
npm run coverage:check
```

### Exceptions and Waivers

Temporary coverage exceptions require:
1. Technical justification document
2. Remediation timeline
3. Risk assessment
4. Approval from technical lead

Exception template:
```yaml
exception:
  file: "src/legacy/parser.ts"
  current_coverage: 65
  target_coverage: 90
  reason: "Legacy code pending refactor"
  risk: "Low - output validated by new wrapper"
  timeline: "2024-Q2"
  approved_by: "tech-lead@example.com"
  review_date: "2024-02-01"
```

### Compliance Monitoring

Monthly coverage review checklist:
- [ ] Overall coverage trend positive
- [ ] No modules below threshold
- [ ] Mutation score maintained
- [ ] Critical code at 100%
- [ ] New code coverage ≥ 95%
- [ ] Coverage data archived properly
- [ ] Exceptions reviewed and updated
