# Git Commit Message Generator

This command analyzes your staged changes and suggests appropriate commit messages following the Conventional Commits specification.

## Prerequisites

Before using this command:
1. Ensure you have staged changes: `git add <files>`
2. Review staged changes: `git diff --cached`

## Analysis Process

I will analyze:
- **File types and paths** to determine commit type and scope
- **Change patterns** to identify the nature of modifications
- **Project conventions** from `.commitlintrc`, `.gitmessage`, or commit hooks
- **Only staged changes** - unstaged files and work-in-progress are ignored

## Conventional Commits Format

```
<type>(<scope>): <subject>

[optional body]

[optional footer(s)]
```

### Types
- **feat**: New feature
- **fix**: Bug fix
- **docs**: Documentation only changes
- **style**: Code style changes (formatting, missing semicolons, etc)
- **refactor**: Code change that neither fixes a bug nor adds a feature
- **perf**: Performance improvements
- **test**: Adding or updating tests
- **chore**: Changes to build process or auxiliary tools
- **ci**: CI/CD configuration changes

### Scope
Optional context describing what part of the codebase changed (e.g., `auth`, `api`, `ui`)

### Subject
Brief description in imperative mood (e.g., "add OAuth support" not "added OAuth support")

## Usage Instructions

1. **Stage your changes**:
   ```bash
   git add <files>
   ```

2. **Run this command** to get commit message suggestions

3. **Choose or modify** a suggested message

4. **Commit** with:
   ```bash
   git commit -m "<chosen message>"
   ```

## Example Output

For changes to authentication files:
```
Suggested commit messages:

1) feat(auth): add OAuth2 login support
2) fix(auth): resolve token expiration issue
3) refactor(auth): extract validation logic

Based on: 3 files changed (+45, -12)
- Added OAuth2Strategy class
- Updated login controller
- Modified auth middleware
```

## Best Practices

- Keep subject line under 50 characters
- Use imperative mood ("add" not "adds" or "added")
- Don't end subject with period
- Separate subject from body with blank line
- Wrap body at 72 characters
- Explain "what" and "why", not "how"

## Breaking Changes

If your change breaks backward compatibility, add `!` after type:
```
feat!: remove deprecated API endpoints
```

## Ready to Analyze

Please share the output of `git diff --cached --stat` and I'll suggest appropriate commit messages based on your staged changes.
