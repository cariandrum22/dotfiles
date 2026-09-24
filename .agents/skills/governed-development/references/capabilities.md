# Capability discovery and validation

## Baseline and authoritative sources

Inspected 2026-09-25: locally installed `codex-cli 0.156.1`. These links are discovery starting
points, not authority for a different installed version:

- [Skills](https://developers.openai.com/codex/skills): repository `.agents/skills/<name>/SKILL.md`,
  YAML frontmatter `name` and `description`; optional `assets`, `references`, `scripts`, UI
  metadata.
- [Configuration reference](https://learn.chatgpt.com/docs/config-file/config-reference): trusted
  repository `.codex/config.toml`; inspect configuration precedence and managed requirements.
- [Configuration schema](https://developers.openai.com/codex/config-schema.json): fetch the current
  schema and record its hash/date. It is a moving schema, not necessarily the installed version.
- [Subagents](https://developers.openai.com/codex/multi-agent): standalone `.codex/agents/*.toml`
  with `name`, `description`, `developer_instructions`, and normal config-layer keys. Role-specific
  model/effort wins over explicit spawn values; otherwise spawn > agents defaults > parent.
- [Instruction discovery](https://developers.openai.com/codex/guides/agents-md): root-to-CWD
  discovery, `AGENTS.override.md` precedence, nested rules, and the aggregate instruction byte
  limit.

Inspect `codex --version`, `codex --help`, and the help of available subcommands before using them.
0.156.1 exposes `--strict-config` for `exec` and `app-server`, **not** `debug`. It exposes
`debug models --bundled`, `debug prompt-input`, and
`app-server generate-json-schema --experimental --out <scratch-dir>`. The last command generates
**app-server protocol** schemas, not the TOML configuration schema. Do not confuse the two.

The bundled catalog advertises `gpt-6-astra` with `low`, `medium`, `high`, `xhigh`, `max`, `ultra`.
The TOML schema's `ReasoningEffort` is a nonempty string; schema validity alone cannot establish
model effort support. Re-query the current configured provider/model catalog (e.g. `debug models` or
app-server `model/list` after checking their help/schema); use bundled output only as an explicit
offline fallback, with account availability unverified. Never infer "best" by lexicographic sorting.

For 0.156.1, these configuration controls are documented and schema-supported:

| Requirement      | Candidate mechanism                                 | Qualification                                                   |
| ---------------- | --------------------------------------------------- | --------------------------------------------------------------- |
| Root effort      | `model_reasoning_effort = "xhigh"`                  | New session, trusted layer, compatible model; overrides can win |
| Worker default   | `agents.default_subagent_reasoning_effort = "high"` | Explicit spawn or role overrides can win                        |
| Worker model     | `agents.default_subagent_model`                     | Preserve compatible pins and provider identity                  |
| Named roles      | Standalone `.codex/agents/*.toml`                   | Must verify actual backend exposes/loads/selects roles          |
| Concurrency      | `agents.max_concurrent_threads_per_session`         | Child threads only; check backend-specific precedence           |
| Managed worktree | CLI `--worktree`                                    | Workspace separation, not sandboxing; inspect inherited config  |

The current schema also contains `agents.max_depth`, explicitly **V1-only, ignored by V2**. The
shared templates omit this backend-specific setting. On V1, inspect its effective value, including
the installed version's default: root → implementer → mechanical requires two child edges, so a
limit of 1 blocks the mechanical grandchild. On V2, inspect the actual backend limits; setting the
ignored V1 key proves nothing. Record nested delegation separately from direct role availability.
Preserve an existing restrictive limit. Do not raise limits, enable an experimental backend, or
expose new tool permissions to obtain a passing result. When nesting is unavailable or unverified,
retain mechanical work in the high implementer or return it to root triage and disclose the
limitation. The hierarchy and repository-wide concurrency coordination are policy, not a global
runtime guarantee.

`plan_mode_reasoning_effort` is a separate supported override. When absent, Plan mode uses a
built-in preset, not necessarily the normal effort. Templates set both default and Plan effort to
the assigned value. Validate both relevant modes and report explicit session/turn overrides that
supersede them. Native `--strict-config` rejection of an unknown root key does not prove unused
standalone role files were parsed: an unknown key in an unselected role was not rejected during the
authoring root-start probe. Validate role files statically and through actual selection separately.

## Inspect the actual harness, not just the CLI version

CLI, app, remote server, and hosted collaboration tools can expose different capabilities even with
the same installed binary. Record both surfaces. On the authoring harness, `spawn_agent` exposes
`task_name`, `message`, `model`, `reasoning_effort`, and `fork_turns`; it has **no role selector**.
`fork_turns="none"` starts without root conversation; a positive integer includes recent turns;
`"all"` (including omission) copies history and forbids model/effort overrides. Its four slots
include the root. Never pass an invented `agent_type`/`role` parameter to this tool.

Before a role-less fallback, resolve the intended role's configuration layers and restrictions.
Compare them with the actual inherited child sandbox, filesystem/network permissions, approvals, and
tool authorization. Check effective runtime overrides too. A read-only or tool-restricted role must
not become a writable or more broadly authorized child just because its file is not loaded. Require
native evidence of equivalent or stricter enforcement; a role contract, preserved TOML, or a no-op
acknowledgment is not such evidence. Unknown enforcement blocks the fallback. Use a supported fresh
session that demonstrably loads the restrictions, or pause that delegation.

After this gate passes, this tool shape can use `fork_turns="none"`, supported effort/model
controls, and the minimal role contract. Report restriction equivalence separately from role-file
selection: effort/context selection is native per-call, but role selection is policy fallback and
the `.toml` files are not proven executed. A named `task_name` does not load a custom role. On
another tool, inspect its schema for a first-class role selector and use it. Do not assume its fork
control has the same spelling, type, default, or semantics. No global full-context non-inheritance
setting was established here. If unsupported, follow the fresh-session alternative in AGENTS; do not
quietly use full inheritance. Keep the restriction-equivalence gate in generated AGENTS for later
tasks that do not load this setup skill.

## Reconciliation checks

Before changes record content hashes privately for managed paths and a parsed configuration snapshot
with secret-bearing values excluded from reports. Compare all non-workflow settings after edits, not
just a short list of security key names. Review role layers too: developer instructions must not
contradict or weaken permissions. Preserve existing comments, provider/auth settings, network and
filesystem constraints, approvals, hooks, tool authorization, and any unknown settings. Unknown
existing keys that fail strict validation require a report/decision, not automatic deletion.

Allow changes only to agreed model/effort defaults, the three roles' reconciled
metadata/instructions, and a new conservative concurrency cap. Existing role collisions require
reconciliation, not a second registration. Existing disabled multi-agent controls or lower caps are
restrictions to retain. Check protected/generated paths, symlink targets, file/directory collisions,
and concurrent edits.

For AGENTS, compare every template semantic requirement, not exact heading matches. Check conflicts
with ancestor/nested instructions and that the whole managed block fits the actual loaded
instruction budget. Do not enlarge context windows/limits merely to hide duplication. Preserve local
commands and artifact references. Apply formatting, then repeat the second-pass no-diff check.

Check Git tracking and ignore rules for every generated policy/configuration/document. Project and
global ignores can hide AGENTS.md or the entire .codex directory from normal status/PRs. Preserve
existing exclusions and add only necessary repository-local exceptions for intended versioned files;
never change global ignores or force-add secrets. A clean local parse is not proof of durable
delivery.

## Validation procedure after application

Record each check separately as passed, failed, not run (reason), or unsupported. A candidate is
**not runtime-validated** if any required loading/effort/role check lacks native evidence.

Use the repository's normal authorized launch path, including configured secret injection. For
example, a Claudius-managed installation may require `claudius secrets run -- codex ...` with its
configured secret-reference variables rather than a direct `codex` process. Inspect the documented
wrapper and use the existing secret manager; verify only that required variables are present, never
print their values or write resolved credentials into files. A missing variable in an unwrapped
process is a launch-path failure, not evidence that credentials are unavailable. Once that cause is
understood and the authorized launcher resolves it, resume the bounded test without changing
provider, trust, permissions, or authentication settings. Keep examples optional; Claudius is not a
dependency of this reusable skill.

- **Static structure:** Parse all TOML with `tomllib` (Python 3.11+) or an existing parser. Validate
  new/changed config keys against a schema compatible with the installed version. For standalone
  roles, validate required `name`, `description`, `developer_instructions` metadata separately from
  the config-layer keys (the root config schema does not necessarily contain role metadata). Reject
  unresolved `__MODEL__`, duplicate keys/roles, and invalid paths. Catalog-check each effort.
- **Native parsing:** On 0.156.1 start `codex app-server --strict-config --stdio` **from the target
  working directory**, using the same configuration environment as the target. Use protocol shapes
  generated by that binary. Initialize the client, then request `thread/start` with target `cwd`,
  `ephemeral=true`, and no permission, model, effort, trust, or policy overrides. No model turn is
  needed for this step. Record the returned `model`, `reasoningEffort`, and `instructionSources`.
  Shutdown the process afterward. Never use another CODEX_HOME/project-as-user-config to claim
  project trust/loading succeeded. `config/read` with `cwd` and `includeLayers` can help audit
  origins, but compare it to the actual thread result; a global config read alone does not prove
  project settings were used.
- **Guidance discovery:** Inspect `debug prompt-input` locally (it may contain private instructions;
  do not publish raw output) or native thread instruction-source evidence. Verify the intended root
  AGENTS and the end of its governed block are present. Repeat for affected nested working dirs and
  worktrees. If an override shadows AGENTS or content is truncated, reconcile it before claiming
  applicability. Do not rename overrides or change trust without authorization. When maintaining
  this skill, inspect from its `assets/` directory too: the policy template must not appear as an
  instruction source or inject its governed block. Keep its `.template` suffix.
- **Role loading and efforts:** Inspect actual spawn tools/role registry. In a safely bounded no-op,
  select each named role, request no file changes, shell commands, network tools, or grandchildren,
  and return a fixed acknowledgment. Run roles sequentially to fit the cap. Use fresh/minimal
  context. Check native child session/turn metadata for model, effort, and loaded role; do not trust
  the child's statement about its own settings. Where safe, a temporary fixture-only instruction
  sentinel can corroborate loading, but never suffices alone to establish effective model/effort. If
  native metadata is unavailable, mark that check unverified. A root `-c` effort override or
  launching the role file as user config is not proof that a custom-role spawn loaded it. On 0.156.1
  app-server, collect child IDs from `subAgentActivity` items and use `thread/read` with
  `includeTurns=false` while the server is alive. Its `agentRole`, `model`, `reasoningEffort`, and
  parent/source fields provide native evidence. A state-DB-only `thread/list` can omit ephemeral
  children; an empty list does not establish that no delegation occurred. If the harness cannot
  close individual workers, finish the bounded probe and end its server after collecting metadata;
  use a fresh session for remaining tests rather than assuming interruption frees a slot.
- **Defaults and fork behavior:** Separately test one ordinary unnamed/default child without an
  explicit effort to verify the configured `high` default. Keep named-role and fallback explicit
  effort tests distinct. Use native spawn arguments/events to establish no-fork behavior; a sentinel
  acknowledgment does not prove absence of history. Inspect restrictions and effective concurrency;
  do not stress-test resource limits by flooding agents.
- **Nested delegation:** Inspect effective backend depth and concurrency limits before any spawn. If
  a limit blocks the two-edge chain, report the exact conflict without changing the limit and skip
  the blocked run. Otherwise run a separate bounded no-op: root starts one `implementer` at high; it
  starts exactly one `mechanical` at medium, receives `NOOP_OK`, and returns it to root. Allocate
  capacity for both descendants, use fresh/minimal handoffs, prohibit file changes, shell/network
  tools and further descendants, and stop/close the probes afterward. Verify actual parent-child
  lineage, role loading, and resolved efforts from native metadata; record evidence gaps explicitly.
  The direct role smoke tests' no-grandchildren rule applies only to those tests. If only a
  role-less harness is available, apply the restriction-equivalence gate first and label the result
  an explicit-control fallback test, not proof of named-role execution. Direct tests passing or a V2
  backend ignoring `max_depth` cannot establish nested support. Unknown limits or missing safe
  execution evidence leave the hierarchy unverified; do not claim full validation.
- **Security and identity:** Compare snapshots/diffs of all untouched settings and security layers.
  Hash final configuration, policy, and reviewed/tested artifacts; record CLI/schema/catalog
  versions, test commands, environment and assumptions. Review after any changes that invalidate the
  evidence. Separately compare native effective child restrictions with the intended role; unchanged
  files alone do not prove fallback security equivalence.
- **Idempotence and conflicts:** Reconcile again with the same evidence and expect zero changes.
  Exercise fixtures with an existing unrelated AGENTS section, pinned model, unrelated TOML/security
  settings, a conflicting role, a shadowing override, and a `.codex` regular file. Expect
  preservation or an explicit blocked result, never silent overwriting or permission relaxation.
  Include a role-less harness with a writable parent and a read-only/tool-restricted reviewer:
  fallback must stop unless equivalent native restrictions can be applied and verified. Include V1
  `agents.max_depth = 1`: direct-role success must not produce a nested-validation pass, and the
  existing limit must remain unchanged.

Runtime smoke tests can invoke models and incur cost; keep them no-op and bounded. Inspect existing
startup hooks/MCP services before starting a new session, because no-op prompts do not suppress
startup side effects. Use an already authorized environment; if those side effects or required
permissions are not authorized, report the check not run. Do not disable hooks, ignore user config,
ignore rules, change sandbox mode, or bypass approvals to make a test pass. Stop after one failing
smoke attempt until the cause is understood; never weaken security as a retry strategy.

## Capability report contract

Classify **each** semantic requirement, using these baseline categories and recording runtime
status:

- Native configuration, conditional on loaded settings: root/model effort, subagent default,
  role-specific effort and instructions when actually selectable, per-session concurrency cap.
- Native per-call where available: effort override and fresh/no-fork context; managed worktree
  creation.
- AGENTS policy: RFC/ADR/TODO/PR process, architecture escalation, bounded handoff/returns,
  selective reading, immutable independent review, non-overlapping ownership, verification
  integrity, durable context boundaries, depth/retry/repository-wide concurrency discipline, and
  metrics without routing self-modification.
- Not currently enforceable universally: immutable effort settings, semantic correctness, hard
  architectural escalation, global non-inheritance, repository-wide process/depth limits across
  independent sessions, sandboxing by worktrees, and custom-role selection on role-less harnesses.

A fallback must identify which native guarantee was lost. Preserve the intended policy, use the
closest supported mechanism, and state any security/architecture-relevant gap explicitly. If no safe
mechanism satisfies a necessary boundary, pause that execution rather than approximate it silently.
