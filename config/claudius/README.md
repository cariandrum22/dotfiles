## Priority Reading Order
1. `/rules/verified-truth-directive.md` - Truthfulness and uncertainty labeling
2. `/rules/communication-style.md` - Bilingual interaction and style constraints
3. `/rules/technical-reasoning-policy.md` - Evidence tagging and response structure

## Mandatory Automation Recipes
- Refer to `/rules/build-tool-just.md#mandatory-recipes` for baseline `just` targets that every project must ship (`default`, `help`, `install`, `install-deps`, `install-tools`, `install-hooks`).
- For projects using Nix Flakes, follow `/rules/build-tool-just.md#nix-flake-integration` to keep `flake.nix` as the single source of truth and wrap declarative entry points via `just`.

## Checklists for Final Responses and PRs
- Response compliance: see `/rules/technical-reasoning-policy.md#response-compliance-checklist`.
- Coverage gate verification: see `/rules/code-coverage-policy.md#quick-compliance-checklist`.
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Claudius is a multi-agent configuration management tool for Claude Desktop/CLI, Claude Code, Codex CLI, and Gemini CLI. It manages MCP (Model Context Protocol) servers, agent settings, skills, and project context rules across projects and teams.

The configuration directory is located at `$XDG_CONFIG_HOME/claudius` (typically `~/.config/claudius`). Claudius syncs configuration sources from this directory into project-local or global targets.

## Quick Start

- Inspect available commands:
  - `claudius --list-commands`
- Bootstrap configuration (creates default files):
  - `claudius config init`
- Sync project-local configuration:
  - `claudius config sync`
- Sync global configuration for a specific agent:
  - `claudius config sync --global --agent claude-code`
- Validate source files without writing:
  - `claudius config validate`
- Sync skills only:
  - `claudius skills sync`
- Append a context rule:
  - `claudius context append security`

## Configuration Sources

Located under `$XDG_CONFIG_HOME/claudius`:

- `/mcpServers.json` - MCP server definitions
- `/claude.settings.json` - Claude/Claude Code settings (preferred)
- `/settings.json` - Legacy Claude settings (backward compatible)
- `/codex.settings.toml` - Codex settings
- `/codex.requirements.toml` - Codex admin-enforced requirements (optional)
- `/codex.managed_config.toml` - Codex admin-managed defaults (optional)
- `/gemini.settings.json` - Gemini settings
- `/config.toml` - Claudius application configuration (default agent, context file, secret manager)
- `/skills/` - Skills (shared and per-agent overrides)
- `/rules/` - Context rule templates (`*.md`)
- `/commands/` - Legacy slash commands (converted to skills during sync)
- `/bin/mcp-with-credentials` - Runtime loader for MCP credentials (local tooling)
- `/credentials/` - Secret storage for MCP credentials (git-ignored)
- `/justfile` - Task automation (validation, hooks)

## Skills Management

The `skills/` directory is the source of truth for managed skills.
Treat deployed skill directories (for example `~/.claude/skills`) as generated outputs.
Use `claudius skills sync` to publish updates into agent targets.

Managed skills (current):
- `setup-biome`
- `setup-c-lint`
- `setup-capybara-lint`
- `setup-commitlint`
- `setup-erb-lint`
- `setup-file-hygiene`
- `setup-git-hooks`
- `setup-i18n-lint`
- `setup-iac-security`
- `setup-just`
- `setup-just-lint`
- `setup-nix-ci-lint`
- `setup-nix-flake`
- `setup-nix-lint`
- `setup-python-lint`
- `setup-rails-api`
- `setup-rails-ci`
- `setup-rails-db-safety`
- `setup-rails-security`
- `setup-rails-testing`
- `setup-rubocop`
- `setup-secrets-scan`
- `setup-shell-lint`
- `setup-sorbet`
- `setup-sql-lint`
- `setup-stylelint`
- `setup-tofu-lint`
- `setup-tofu-project`
- `setup-tftpl-lint`
- `setup-ts-typecheck`

Note: The legacy `commands/` directory is retained in this phase. Do not migrate commands into `skills/` unless explicitly instructed.

## Target Locations and Scopes

### Project-local mode (default)

- Claude Desktop / Claude Code (project scope):
  - MCP servers -> `./.mcp.json`
  - Settings -> `./.claude/settings.json`
- Claude Code (local scope):
  - MCP servers -> `~/.claude.json` (per-project)
  - Settings -> `./.claude/settings.local.json`
- Codex:
  - Settings + MCP servers -> `./.codex/config.toml`
- Gemini:
  - Settings + MCP servers -> `./.gemini/settings.json`

### Global mode (`--global`)

- Claude Desktop:
  - `$XDG_CONFIG_HOME/Claude/claude_desktop_config.json`
  - macOS: `~/Library/Application Support/Claude/claude_desktop_config.json`
  - Windows: `%APPDATA%\Claude\claude_desktop_config.json`
- Claude Code (user scope):
  - MCP servers -> `~/.claude.json`
  - Settings -> `~/.claude/settings.json`
- Claude Code (managed scope):
  - MCP servers -> `managed-mcp.json`
  - Settings -> `managed-settings.json`
  - Managed directory defaults:
    - Linux: `/etc/claude-code`
    - macOS: `/Library/Application Support/ClaudeCode`
    - Windows: `C:\Program Files\ClaudeCode`
  - Override with `CLAUDIUS_CLAUDE_CODE_MANAGED_DIR`
- Codex:
  - User config -> `~/.codex/config.toml`
  - Optional admin-enforced requirements -> `/etc/codex/requirements.toml`
  - Optional managed defaults -> `/etc/codex/managed_config.toml`
  - Overrides: `CLAUDIUS_CODEX_REQUIREMENTS_PATH`, `CLAUDIUS_CODEX_MANAGED_CONFIG_PATH`
- Gemini:
  - User settings -> `~/.gemini/settings.json`
  - System settings -> `/etc/gemini-cli/settings.json` (OS-specific default)
  - Override: `GEMINI_CLI_SYSTEM_SETTINGS_PATH`

### Context files

- Default context file for Claude/Claude Code: `CLAUDE.md`
- Default context file for Codex/Gemini: `AGENTS.md`
- Optional override: `config.toml` -> `[default] context-file = "..."`

### Skills targets

- Project-local:
  - Claude -> `./.claude/skills/`
  - Gemini -> `./.gemini/skills/`
  - Codex -> `./.codex/skills/` (experimental, opt-in)
- Global:
  - Claude -> `~/.claude/skills/`
  - Gemini -> `~/.gemini/skills/`
  - Codex -> `~/.codex/skills/` (experimental, opt-in)

## Command Reference

### `claudius config init`

Bootstrap the configuration directory and create default files.

Creates (if missing):
- `mcpServers.json`
- `claude.settings.json` (preferred)
- `settings.json` (legacy, for backward compatibility)
- `codex.settings.toml`
- `codex.requirements.toml`
- `codex.managed_config.toml`
- `gemini.settings.json`
- `config.toml`
- `skills/<skill-name>/SKILL.md`
- `rules/example.md`

Additional behavior:
- Migrates legacy `settings.json` to `claude.settings.json` when needed.
- Initializes context files in the current directory.
  - Creates the primary context file and a symlink to the secondary file on Unix-like systems.
  - Respects `config.toml` default context file when provided.

### `claudius config sync`

Synchronize source configuration files into project-local or global targets.

Key options:
- `--global` - write to global targets instead of project-local
- `--agent` - `claude`, `claude-code`, `codex`, or `gemini`
- `--scope` - `managed`, `user`, `project`, `local` (Claude Code only)
- `--dry-run` - preview output without writing
- `--backup` - create timestamped backups before writing
- `--config` - override `mcpServers.json` source path (`CLAUDIUS_CONFIG`)
- `--target-config` - override target config path (`TARGET_CONFIG_PATH`)
- `--codex-requirements` - sync `/etc/codex/requirements.toml` (global Codex only)
- `--codex-managed-config` - sync `/etc/codex/managed_config.toml` (global Codex only)
- `--gemini-system` - sync Gemini CLI system settings (global Gemini only)

Notes:
- If `--global` is used with no agent, no custom paths, and no special flags, Claudius syncs all available agents detected from source files.
- Skills are synced automatically after configuration sync, except for Codex (requires explicit opt-in).

### `claudius config validate`

Validate source configuration files without writing anything.

Options:
- `--agent` - validate a specific agent
- `--strict` - treat warnings as errors

### `claudius skills sync`

Synchronize skills from `skills/` (or legacy `commands/`) into agent skill directories.

Options:
- `--global` - target global skill directory
- `--agent` - `claude`, `claude-code`, `codex`, or `gemini`
- `--enable-codex-skills` - required for Codex (experimental)

### `claudius context append`

Append rule templates or custom templates to context files.

Options:
- `--path` (`CLAUDIUS_PROJECT_PATH`) - target directory
- `--template-path` (`CLAUDIUS_TEMPLATE_PATH`) - custom template file
- `--global` - target `$HOME` instead of project directory
- `--agent` - selects default context file for the agent

### `claudius context install`

Copy rules into a project-local rules directory and add a reference directive to the context file.

Options:
- `--all` - install all rules (recursively)
- `--path` - target project directory
- `--install-dir` (`CLAUDIUS_INSTALL_DIR`) - override install directory (default: `.agents/rules`)
- `--agent` - selects default context file for the agent

Reference directive format (added or updated):
```
<!-- CLAUDIUS_RULES_START -->
# External Rule References

The following rules from `.agents/rules` are installed:

- `.agents/rules/security.md`: security
- `.agents/rules/testing.md`: testing

Read these files to understand the project conventions and guidelines.
<!-- CLAUDIUS_RULES_END -->
```

### `claudius context list`

List available rules in the rules directory.

Options:
- `--tree` - display a directory tree with nested rules

### `claudius secrets run`

Run a command with automatic secret resolution from environment variables.

Behavior:
- Resolves `CLAUDIUS_SECRET_*` environment variables using the configured secret manager
- Supports `op://...` and `{{op://...}}` 1Password references
- Expands nested variables using a DAG-based resolver
- Removes the `CLAUDIUS_SECRET_` prefix before execution
- Inherits stdio for interactive commands

Note:
- `type = "vault"` can be configured, but secret resolution is not implemented and values remain unchanged (a warning is logged).

## Configuration Formats

### `mcpServers.json`
```json
{
  "mcpServers": {
    "server-name": {
      "command": "executable-command",
      "args": ["arg1", "arg2"],
      "env": {
        "API_KEY": "value"
      }
    }
  }
}
```

### `claude.settings.json`
```json
{
  "apiKeyHelper": "/path/to/key-generator.sh",
  "cleanupPeriodDays": 20,
  "env": {"CUSTOM_VAR": "value"},
  "includeCoAuthoredBy": false,
  "permissions": {
    "allow": ["Bash(npm run lint)"],
    "deny": ["Write(/etc/*)"],
    "defaultMode": "allow"
  },
  "preferredNotifChannel": "chat"
}
```

### `codex.settings.toml`
```toml
# model = "gpt-5-codex"
# approval_policy = "on-request"
```

### `codex.requirements.toml`
```toml
# allowed_approval_policies = ["untrusted", "on-request", "on-failure"]
# allowed_sandbox_modes = ["read-only", "workspace-write"]
```

### `codex.managed_config.toml`
```toml
# approval_policy = "on-request"
# sandbox_mode = "workspace-write"
```

### `gemini.settings.json`
```json
{
  "$schema": "https://raw.githubusercontent.com/google-gemini/gemini-cli/main/schemas/settings.schema.json",
  "general": {},
  "ui": {},
  "tools": {},
  "context": {},
  "privacy": {},
  "telemetry": {}
}
```

### `config.toml`
```toml
[default]
agent = "claude"  # or "claude-code" or "codex" or "gemini"
context-file = "CLAUDE.md"  # optional custom filename

[secret-manager]
type = "1password"  # or "vault" (not implemented)
```

## Secret Resolution and Variable Expansion

- Secret manager types:
  - `1password` is supported via the `op` CLI.
  - `vault` is accepted in configuration but does not resolve secrets.
- Reference formats:
  - Bare: `op://vault/item/field`
  - Delimited: `{{op://vault/item/field}}` (recommended for URL contexts)
- Resolution phases:
  1. Collect `CLAUDIUS_SECRET_*` variables
  2. Resolve `op://` references in parallel
  3. Expand nested variables using a DAG (supports `$VAR` and `${VAR}`)
  4. Strip `CLAUDIUS_SECRET_` prefix

## Merge Behavior

- MCP servers:
  - New servers are added
  - Existing servers with the same name are replaced
  - Other servers remain unchanged
- Settings:
  - Only specified fields are updated
  - Unspecified fields remain unchanged
  - `null` values do not remove existing values
- Skills:
  - Skill directories are synced
  - Existing skills are overwritten
  - Removed source skills do not delete deployed skills

## Environment Variables

- `CLAUDIUS_CONFIG` - default MCP server config source path
- `TARGET_CONFIG_PATH` - override target config path for sync
- `CLAUDIUS_CLAUDE_CODE_MANAGED_DIR` - override Claude Code managed directory
- `CLAUDIUS_CODEX_REQUIREMENTS_PATH` - override Codex requirements target path
- `CLAUDIUS_CODEX_MANAGED_CONFIG_PATH` - override Codex managed config target path
- `GEMINI_CLI_SYSTEM_SETTINGS_PATH` - override Gemini CLI system settings path
- `CLAUDIUS_PROJECT_PATH` - default project path for `context append`
- `CLAUDIUS_TEMPLATE_PATH` - default template path for `context append`
- `CLAUDIUS_INSTALL_DIR` - default install directory for `context install`
- `XDG_CONFIG_HOME` - base config directory
- `CLAUDIUS_SECRET_*` - secrets injected into command environment
- `CLAUDIUS_TEST_MOCK_OP` - enable mock 1Password CLI for tests

## MCP Server Integrations (local configuration)

Defined in `mcpServers.json` in this directory:
- `awslabs_aws-documentation-mcp-server`
- `chrome-devtools`
- `figma`
- `filesystem`
- `github`
- `google-calendar`
- `perplexity-ask`
- `playwright`
- `todoist`

## Security Considerations

- Avoid committing secrets into tracked configuration files.
- Use `credentials/` and `bin/mcp-with-credentials` for runtime secret injection.
- Prefer `CLAUDIUS_SECRET_*` environment variables for secret resolution during execution.

## Development Notes

- Configuration files (`*.settings.*`) are synchronized across Claude Code, Gemini CLI, and Codex CLI.
- `mcpServers.json` is the single source of truth for MCP server definitions.
- Rule templates in `/rules/` can be merged into context files via `context append` or `context install`.
- Legacy `settings.json` and `commands/` are still supported for backward compatibility.
