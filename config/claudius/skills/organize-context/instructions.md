# Context Document Organizer

This command analyzes CLAUDE.md or AGENTS.md files and reorganizes them with complete, deduplicated rule references for optimal context management.

## Purpose

Context documents (CLAUDE.md, AGENTS.md) serve as primary guidance for AI agents. Over time, these documents may:
- Reference rules that have been updated or renamed
- Miss newly created relevant rules
- Contain duplicate or conflicting information
- Lack consistent structure

This command ensures your context documents remain comprehensive and well-organized.

## Analysis Process

I will perform the following steps:

1. **Rule Discovery**
   - Parse the target document for all rule references
   - Scan `/rules/` directory for all available rules
   - Identify missing, outdated, or broken references

2. **Content Analysis**
   - Extract core directives from the main document
   - Analyze each referenced rule for key policies
   - Detect overlapping or contradictory guidance

3. **Reorganization**
   - Structure content hierarchically by topic
   - Merge related rules intelligently
   - Eliminate redundancies while preserving specificity
   - Update cross-references to current paths

4. **Validation**
   - Ensure all critical rules are included
   - Verify no policy conflicts exist
   - Confirm readability and logical flow

## Usage Instructions

1. **Specify the target document**:
   - Full path to CLAUDE.md or AGENTS.md
   - Or simply "CLAUDE.md" for the current directory

2. **Review the analysis report** showing:
   - Current structure assessment
   - Missing rule detections
   - Redundancy findings
   - Proposed reorganization plan

3. **Approve the reorganization** to:
   - Update the document with optimized structure
   - Include all relevant rules
   - Remove duplications
   - Standardize formatting

## Document Structure Template

The reorganized document will follow this structure:

```markdown
# [CLAUDE.md | AGENTS.md]

## Overview
[Project-specific context and purpose]

## Core Directives

### Language and Communication
[Consolidated from communication-style.md and related rules]

### Technical Standards
[Merged from linting, testing, and quality policies]

### Development Workflow
[Combined from CI/CD, version control, and artifact policies]

### Architecture Principles
[Extracted from design patterns and reasoning policies]

## Rule References
[Organized list of all incorporated rules with brief descriptions]

## Quick Reference
[Summary table of key policies and standards]
```

## Example Analysis Output

```
Current Document Analysis:
- Total rules referenced: 12
- Missing rule references: 3
  - /rules/code-coverage-policy.md
  - /rules/intermediate-artifacts-policy.md
  - /rules/technical-reasoning-policy.md
- Duplicate topics found: 2
  - Communication style (3 sections)
  - Testing requirements (2 sections)
- Structure issues:
  - Security section scattered across document
  - Inconsistent heading levels

Proposed Reorganization:
1. Consolidate communication guidelines
2. Add missing coverage and artifact policies
3. Group security-related content
4. Standardize markdown formatting
```

## Options

### Scope Control
- `--minimal`: Only fix broken references and add missing rules
- `--comprehensive`: Full reorganization with content merging
- `--preserve-structure`: Maintain current sections, only update content

### Output Format
- `--diff`: Show changes as unified diff
- `--preview`: Display reorganized content without saving
- `--backup`: Create `.bak` file before modifying

### Rule Selection
- `--include-pattern`: Regex pattern for rules to include
- `--exclude-pattern`: Regex pattern for rules to exclude
- `--priority-rules`: Comma-separated list of must-include rules

## Best Practices

1. **Regular Maintenance**: Run monthly or after significant rule changes
2. **Version Control**: Commit before and after reorganization
3. **Team Review**: Have changes reviewed when structure significantly changes
4. **Custom Sections**: Preserve project-specific sections not found in rules
5. **Rule Evolution**: Update when rules are renamed or split

## Validation Checklist

The reorganized document will ensure:
- [ ] All rules in `/rules/` are considered for inclusion
- [ ] No broken rule references remain
- [ ] Related content is grouped logically
- [ ] Hierarchical structure is consistent
- [ ] Critical policies are prominently placed
- [ ] Cross-references use current paths
- [ ] Markdown formatting is standardized
- [ ] Document remains concise yet complete

## Ready to Organize

Please provide:
1. Path to the context document (CLAUDE.md or AGENTS.md)
2. Preferred reorganization approach (minimal/comprehensive)
3. Any specific rules that must be included or excluded

I'll analyze the document and present a detailed reorganization plan for your approval.
