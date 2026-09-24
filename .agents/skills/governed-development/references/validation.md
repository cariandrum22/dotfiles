# Authoring validation — 2026-09-25

This records validation of the reusable skill, not deployment of the workflow to dotfiles. The
initial authoring results below are historical; see the review-fix checks for subsequent changes.
Re-run capability discovery and validation when applying the skill elsewhere.

## Target and evidence

- Installed binary: `codex-cli 0.156.1`.
- Official live configuration schema SHA-256:
  `b373250e2565043b270d22df0e8cf32cacb36dd32fa2dc1e1e871115e73caab8`.
- Model evidence: installed bundled catalog advertises `gpt-6-astra` and all three required efforts;
  live provider entitlement remains unverified. No model name is fixed in reusable templates.
- Native project skill path: `.agents/skills/governed-development/SKILL.md`; actual CLI prompt
  discovery included its name. Required frontmatter passes the installed `quick_validate.py`.
- Templates target `.codex/config.toml` and standalone `.codex/agents/*.toml`. Root and all roles
  set matching normal/Plan effort. Existing compatible repository model pins take precedence.

## Checks performed

| Check                    | Result and scope                                                                                                                                             |
| ------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Skill structure          | Passed installed skill-creator `quick_validate.py`                                                                                                           |
| Native skill discovery   | Passed `codex debug prompt-input`; raw private prompt was not published                                                                                      |
| Template syntax/schema   | All four rendered TOML templates passed Python TOML parsing and full JSON Schema validation; role metadata checked separately                                |
| Model effort support     | Required `medium`, `high`, `xhigh` advertised by bundled catalog                                                                                             |
| Native project parsing   | Passed strict app-server startup from an isolated Git fixture                                                                                                |
| Effective root           | `thread/start` returned `gpt-6-astra` / `xhigh` with no model/effort overrides                                                                               |
| Default implementation   | Native `config/read` returned subagent model `gpt-6-astra`, effort `high`, cap 3; an actual default child remains untested                                   |
| AGENTS loading           | Native `instructionSources` included fixture AGENTS; prompt inspection found the complete governed block                                                     |
| Plan effort              | Template parsing/schema verified; an actual Plan model turn was not run                                                                                      |
| Native negative control  | Injected unsupported root key caused strict startup failure; unknown key in an unused role did not fail root startup                                         |
| Named-role execution     | Not verified: CLI no-op model turn failed with `Missing environment variable: OPENAI_API_KEY` before spawning                                                |
| Context isolation        | Actual authoring review used `fork_turns="none"`, explicit `xhigh`; no global isolation setting claimed                                                      |
| Preservation/idempotence | Independent fixture retained project rules, Sol pin, read-only sandbox, on-request approvals, cap 2, and unrelated comment; second reconciliation diff empty |
| Conflict fixtures        | Existing conflicting reviewer, shadowing override, and `.codex` regular file preserved and reported blocked                                                  |
| Security                 | No real user/project permissions, trust, credentials, network, hooks, or tool authorizations changed                                                         |
| Formatting               | Repository Prettier and whitespace checks passed                                                                                                             |

The independent preservation fixture has a possible effective-layer conflict: its retained
`sandbox_mode` coexists with user-level `default_permissions`. The skill reports the conflict
instead of changing either setting. That fixture is a static reconciliation test, not a runtime
deployment.

## Adversarial review and repairs

An independent fresh-context `xhigh` agent reviewed the seven core skill files. Initial review
identified ignored/untracked policy artifacts as a durability risk. The final skill checks tracking
and proposes narrow repository-only exceptions, preserving global excludes and intentional
local-only choices. Authoring inspection also identified Plan mode's independent effort preset;
templates now set its supported override. That initial follow-up review found no remaining
actionable findings; a later review identified the three issues documented below.

Reviewed seven-file manifest SHA-256:
`8c207477bfbefc2a577afe9213922aeb20b3ce903641e76f8b484e69dc07454b`. This evidence note is not part
of that manifest. Independent final reconciliation fixture manifest:
`ae5bcc16711ca380d0ebb004d249afe69916b73a31e83adfe489d0ec78803310`. These hashes identify the
initial revision, not the later repaired files.

## Review-fix checks

The later review identified unsafe role-less fallback, accidental discovery of the policy template
as live instructions, and missing validation of implementer-to-mechanical delegation.

- Generated AGENTS now requires equivalent or stricter native enforcement of the intended role's
  restrictions before fallback. Unknown or broader child permissions stop delegation; a supported
  fresh session must demonstrably load the restrictions before it can be used instead.
- Renamed the source to `assets/AGENTS.md.template` and updated its reference. Native
  `codex debug prompt-input` from `assets/` confirmed the governed block is no longer loaded as live
  instructions, while the skill remains discoverable.
- Added effective-depth checks, a separate two-edge no-op procedure, and negative cases for V1
  `max_depth = 1` and unverified V2 limits. Existing restrictions remain unchanged. If nested
  delegation is unavailable, the high implementer retains the mechanical work or returns it to root
  triage. Direct role success cannot count as a successful nested test.
- The installed skill validator, TOML parsing of all four configuration templates, and Prettier
  checks passed after these changes.

An independent fresh-context `xhigh` review found no remaining actionable findings. It simulated
restricted-reviewer fallback, V1 depth 1, unknown V2 limits, and proven equivalent permissions with
supported nesting. The first case stops delegation; the next two retain work with the high
implementer; only the last permits bounded delegation, without claiming native role execution on a
role-less harness. These are instruction-level scenario checks, not runtime permission tests.

Repository-wide `nix develop --impure --command pre-commit run --all-files` also passed. Its initial
EditorConfig failure was resolved by using Markdown bullet indentation compatible with both the
repository's checker and Prettier, without changing validation semantics or disabling a check.
`nix flake check` passed on x86_64-linux; incompatible aarch64-darwin checks were not run.

Actual named-role and nested CLI model execution remain unverified: the earlier no-op stopped for
missing `OPENAI_API_KEY`, and no credentials, permissions, or backend settings were changed to retry
it. Instruction review and simulated conflict cases cannot replace that runtime evidence.

## Dotfiles preview

Real `AGENTS.md` and `.codex` were left unchanged. Proposed AGENTS content preserves existing rules
and adds the governed section (approximately 13 KB total). Its global Markdown ignore requires a
narrow repository exception for durable delivery. The existing `.codex` is an empty, read-only
**regular file**: creating the configuration directory is blocked until that collision is explicitly
resolved. A hypothetical candidate would produce:

| Target                         | Proposed behavior                                                                     |
| ------------------------------ | ------------------------------------------------------------------------------------- |
| AGENTS.md                      | Add RFC/ADR/TODO/feature-PR, escalation, delegation, verification, and context policy |
| .gitignore                     | Narrow exception for intended versioned AGENTS.md                                     |
| .codex/config.toml             | Catalog-resolved Astra, root normal/Plan xhigh, default children high, cap 3          |
| .codex/agents/implementer.toml | high, bounded implementation and architectural escalation                             |
| .codex/agents/mechanical.toml  | medium, fully specified work only, no grandchildren                                   |
| .codex/agents/reviewer.toml    | xhigh, independent immutable evidence review, no grandchildren                        |

This is not an applicable configuration patch while the `.codex` collision exists. No trust or
permissions changes were used to circumvent it. The real repository changes are the reusable skill
and narrowly scoped `.gitignore` entries that make the skill itself versionable.

## Enforcement and remaining gaps

Configuration expresses root/default/role efforts and the per-session child cap. It does not make
them immutable against higher-priority overrides. Native role execution requires a harness that
actually selects the role files; the authoring collaboration tool has no role selector. It does
support explicit effort and no-fork controls. The repaired fallback first requires verified security
equivalence; it still cannot be labeled native custom-role execution. If role-specific restrictions
cannot actually be enforced, delegation stops instead of replacing restrictions with instructions.

RFC/ADR governance, bounded delegation, escalation, file ownership, verification integrity, durable
context, and metrics collection remain AGENTS policy. There is no verified universal global
non-inheritance or cross-session depth/concurrency enforcement. Git worktrees isolate mutable
workspaces, not security boundaries. Authentication-blocked native child tests remain outstanding
for each future deployment; the skill requires them or an explicit unverified result.
