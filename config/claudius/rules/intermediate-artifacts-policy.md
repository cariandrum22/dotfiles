## Intermediate Artifacts Management Policy

### Overview

This policy defines the standard locations for intermediate artifacts and work products that are generated during development but may not be part of the final deliverables.

### Directory Structure

All intermediate artifacts must be stored under `./.agents/products/` with the following organization:

```
.agents/
└── products/
    ├── tmp/          # Temporary artifacts not included in final deliverables
    ├── scripts/      # Reusable scripts not part of final product
    ├── reports/      # User-facing reports and analysis documents
    └── docs/
        └── design/   # Design documents with uncertain final placement
```

### Artifact Categories and Storage Rules

#### 1. Temporary Artifacts (`./.agents/products/tmp/`)

Store artifacts that meet these criteria:
- Generated during analysis or processing
- Not intended for final deliverables
- May be deleted after task completion
- Intermediate calculation results
- Debug outputs and logs
- Draft versions before finalization

Examples:
```
./.agents/products/tmp/
├── analysis-2024-01-15.txt
├── dependency-graph.dot
├── performance-metrics.csv
├── extracted-data.json
└── debug-output.log
```

#### 2. Utility Scripts (`./.agents/products/scripts/`)

Store scripts that:
- May be reused across multiple tasks
- Support development workflow
- Are not part of the final product
- Automate repetitive tasks
- Perform data transformation or analysis

Examples:
```
./.agents/products/scripts/
├── analyze-codebase.py
├── generate-report.sh
├── transform-data.js
├── validate-config.rb
└── cleanup-artifacts.sh
```

Script Header Template:
```python
#!/usr/bin/env python3
"""
Script: analyze-codebase.py
Purpose: Analyze codebase structure and generate metrics
Created: 2024-01-15
Usage: python analyze-codebase.py <directory>

This is an intermediate artifact not intended for final delivery.
"""
```

#### 3. User Reports (`./.agents/products/reports/`)

Store documents that:
- Require user review or approval
- Summarize analysis or findings
- Present recommendations
- Document decisions or rationale
- Provide progress updates

Examples:
```
./.agents/products/reports/
├── security-audit-2024-01-15.md
├── performance-analysis.md
├── dependency-review.md
├── architecture-recommendations.md
└── migration-plan.md
```

Report Structure Template:
```markdown
# [Report Title]

**Date**: 2024-01-15  
**Type**: [Analysis|Audit|Review|Recommendation]  
**Status**: [Draft|Final]

## Executive Summary

[Brief overview of findings and recommendations]

## Detailed Findings

[Comprehensive analysis with supporting data]

## Recommendations

[Actionable items with priority levels]

## Appendices

[Supporting materials and references]
```

#### 4. Design Documents (`./.agents/products/docs/design/`)

Store documents when:
- Uncertain whether to include in main documentation
- Design is exploratory or provisional
- Architecture decisions are pending
- Multiple design alternatives exist
- Documentation structure is undetermined

Examples:
```
./.agents/products/docs/design/
├── api-design-v1.md
├── database-schema-proposal.md
├── component-architecture.md
├── state-management-options.md
└── integration-patterns.md
```

### File Naming Conventions

1. **Include timestamps** for version tracking:
   - Format: `YYYY-MM-DD` or `YYYYMMDD-HHMMSS`
   - Example: `analysis-2024-01-15.txt`

2. **Use descriptive names**:
   - Clear purpose identification
   - Include version numbers where applicable
   - Example: `security-audit-v2-draft.md`

3. **Maintain consistent extensions**:
   - Reports: `.md` (Markdown)
   - Scripts: Appropriate language extension
   - Data files: `.json`, `.csv`, `.yaml`
   - Logs: `.log`, `.txt`

### Lifecycle Management

#### Creation Phase
```bash
# Create directory structure if not exists
mkdir -p .agents/products/{tmp,scripts,reports,docs/design}

# Add to .gitignore
echo ".agents/products/tmp/" >> .gitignore
```

#### During Development
- Place artifacts immediately in correct location
- Update artifact status in reports (Draft → Final)
- Document artifact purpose in file headers
- Maintain index file for complex projects

#### Project Completion
1. Review all artifacts for final placement
2. Move design documents to permanent locations if approved
3. Archive or delete temporary artifacts
4. Document which scripts should be preserved

### Integration with Version Control

**.gitignore Configuration**:
```gitignore
# Intermediate artifacts
.agents/products/tmp/
.agents/products/scripts/*.log
.agents/products/reports/*-draft.*

# Keep directory structure
!.agents/products/tmp/.gitkeep
!.agents/products/scripts/.gitkeep
!.agents/products/reports/.gitkeep
!.agents/products/docs/design/.gitkeep
```

### Decision Criteria

#### When to use `.agents/products/` vs project directories:

Use `.agents/products/`:
- Uncertain about final placement
- Experimental or exploratory work
- Analysis specific to current task
- Personal working notes
- Intermediate processing results

Use project directories directly:
- Clear requirement for deliverable
- Established project structure exists
- Production code or configuration
- User-requested features
- Official documentation

### Examples by Use Case

#### Code Analysis Task
```
.agents/products/
├── tmp/
│   ├── function-calls-graph.dot
│   ├── complexity-metrics.json
│   └── unused-imports.txt
├── scripts/
│   └── analyze-dependencies.py
└── reports/
    └── code-quality-assessment.md
```

#### Architecture Design Task
```
.agents/products/
├── docs/design/
│   ├── microservices-proposal.md
│   ├── monolith-comparison.md
│   └── hybrid-approach.md
├── tmp/
│   └── architecture-diagrams.drawio
└── reports/
    └── architecture-decision-record.md
```

#### Performance Optimization Task
```
.agents/products/
├── tmp/
│   ├── profiling-results/
│   ├── benchmark-data.csv
│   └── flame-graphs/
├── scripts/
│   ├── run-benchmarks.sh
│   └── analyze-profiles.py
└── reports/
    └── performance-optimization-plan.md
```

### Quality Checklist

Before creating an artifact, verify:
- [ ] Correct directory selected based on artifact type
- [ ] Clear naming convention followed
- [ ] Purpose documented in file header
- [ ] Appropriate file format chosen
- [ ] Version control considerations addressed
- [ ] Lifecycle phase identified (temporary/permanent)

### Maintenance Commands

```bash
# List all intermediate artifacts
find .agents/products -type f -name "*" | sort

# Clean old temporary files (older than 7 days)
find .agents/products/tmp -type f -mtime +7 -delete

# Generate artifact inventory
ls -la .agents/products/*/ > .agents/products/inventory.txt

# Archive completed project artifacts
tar -czf agent-artifacts-$(date +%Y%m%d).tar.gz .agents/products/
```

### Error Prevention

Common mistakes to avoid:
- [ERROR] Placing final deliverables in `.agents/products/`
- [ERROR] Using `.agents/` for project source code
- [ERROR] Forgetting to document artifact purpose
- [ERROR] Mixing temporary and permanent artifacts
- [ERROR] Committing sensitive data in reports

### Integration with Other Policies

This policy complements:
- Communication Style Policy (for report formatting)
- Documentation Standards (for design documents)
- Security Policy (for handling sensitive analysis)
- Version Control Policy (for artifact lifecycle)