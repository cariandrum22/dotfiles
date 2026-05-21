# Git Commit Message Generator

Analyze staged changes and suggest appropriate commit messages following the
Conventional Commits specification.

## Prerequisites

Before suggesting messages:

1. Run `git diff --cached --stat`.
2. Run `git diff --cached`.
3. If there are no staged changes, tell the user to stage changes first.

## Analysis Process

Analyze:

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

Generate 2-4 candidate messages from the staged diff. Do not commit unless the
user explicitly asks you to commit.

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
