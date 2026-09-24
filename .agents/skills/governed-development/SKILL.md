---
name: governed-development
description:
  Establish or reconcile repository AGENTS.md and Codex root/subagent configuration for governed
  RFC, ADR, bounded TODO, feature PR, and evidence-based review workflows. Use for
  development-workflow setup or maintenance, not ordinary feature implementation.
---

# Governed development

Configure the repository so deep architectural reasoning becomes durable specifications before
bounded implementation is delegated. This skill contains the complete reusable policy; no
Trismegistus conversation is required. Preserve domain-specific terms when relevant; map them to
local equivalents only without weakening their meaning.

## Invocation and scope

Invoke `$governed-development` to establish/update the workflow, or ask for a **preview** to obtain
proposed diffs without changing target files. It manages `AGENTS.md`, `.codex/config.toml`, and
`.codex/agents/{implementer,mechanical,reviewer}.toml` where supported. Optionally maintain
`docs/development/codex-workflow.md` for capability evidence, artifact paths, and validation
results; do not duplicate the policy there. It does not merge PRs, change account configuration, or
install new permissions. Copy this whole skill directory into a new repository's `.agents/skills/`,
or the currently supported user skill directory, and invoke it there.

## 1. Inspect before designing changes

Read [capabilities and validation](references/capabilities.md). Recheck the installed CLI, official
docs/schema, model catalog, and actual harness tools on **every application**. The recorded 0.156.1
baseline is evidence, not a permanent version assumption. Do not guess configuration keys, role
selection arguments, effort values, or model identifiers. Model catalogs establish supported values,
not account entitlement. If evidence is missing, retain the intended policy and report the execution
requirement as unverified or unsupported; do not claim full setup.

Inspect the Git root/worktree, dirty files, all applicable ancestor and nested `AGENTS.md` and
`AGENTS.override.md`, configuration layers/profiles, managed requirements, custom roles, instruction
size limits, existing RFC/ADR/task locations and verification commands. Inspect only needed
settings; do not dump credentials, environment variables, transcripts, or entire user configuration.
Identify the normal authorized launch command and any secret-injection wrapper before runtime tests.
A direct CLI process missing an environment variable does not prove the configured credentials are
unavailable; use the existing launcher without exposing secret values or changing authentication.

Use `lstat` on intended destinations and parents. A `.codex` regular file, unexpected symlink,
read-only/generated config, duplicate role name, conflicting override, or malformed file is a
reconciliation problem, not permission to remove or replace it. Resolve a repository's generator
source if authorized; otherwise prepare the concrete safe portion and report the blocked portion.

Check `git ls-files` and `git check-ignore -v` for every intended artifact. A locally discoverable
but ignored file is not durable repository state. If the policy is meant to ship with the
repository, prepare narrowly scoped repository `.gitignore` exceptions as part of the diff,
preserving unrelated exclusions; never modify global ignores or stage/commit automatically. If files
are intentionally local-only, disclose that other clones will not inherit the workflow and do not
claim team-wide setup.

## 2. Reconcile policy and configuration

Use [the policy template](assets/AGENTS.md.template) as the complete semantic checklist. Preserve
existing valid project rules, verification commands, accepted decisions, and document naming
conventions. Map document references to actual paths; do not fabricate accepted RFCs or decisions.
Keep templates under non-discoverable names; only the reconciled destination is named `AGENTS.md`.

On first application, merge existing equivalent workflow sections in place or add the single marked
section. On later applications, compare its current text to the desired policy and preserve local
adaptations. Markers indicate scope, not ownership or permission to overwrite. Never append
duplicate policy sections. If existing rules conflict semantically, present the exact conflicting
text and proposed reconciliation for human decision; do independent, nonconflicting work meanwhile.
Do not silently pick a winner, delete a rule, or leave contradictory instructions as a successful
result.

Resolve the root model: preserve an existing repository model pin unless proven incompatible. If
none exists, select the best configured, available Astra identifier from current model evidence; use
the same model for workers unless an existing compatible role pin should be preserved. A non-Astra
repository pin takes precedence over the Astra preference: disclose this. Do not silently substitute
another family when Astra or required efforts are unavailable. Retain specifications and report the
gap; obtain a decision for any model migration.

For compatible installations, adapt [config.toml](assets/config.toml.template) and the three
[role templates](assets/agents/). Set supported default-mode and Plan-mode effort overrides to each
assigned effort; Plan presets may otherwise bypass the normal default. Inspect explicit mode/session
overrides during validation. Resolve `__MODEL__` as a TOML-escaped string, never by unsafe shell
interpolation. On 0.156.1 use standalone custom role files with mandatory metadata; do not also
register duplicate `[agents.<role>]` declarations. An existing supported config-file registration
may instead be maintained in place after validating precedence and role loading.

Edit TOML with targeted patches or a comment-preserving parser. Preserve every unrelated key,
comment, role, security restriction, and tool authorization. Never serialize an entire existing
config through a lossy dict writer. Preserve an existing concurrency limit; for a new configuration
use at most three concurrent child threads and the actual host limit if lower. Inspect backend
precedence: a V2-specific cap can override the generic cap. Do not increase an existing cap or
enable a disabled tool/backend merely to pass a smoke test. Report a blocking feature restriction.
Inspect effective nesting limits as well: V1 `agents.max_depth` can block the mechanical grandchild.
Verify the two-edge delegation separately; never infer it from direct role tests or raise an
existing depth limit automatically. If unavailable, retain mechanical work in the high implementer
and disclose the hierarchy limitation.

Do not change sandbox, network, filesystem, approvals, trust, provider/auth, hooks, telemetry, or
machine-level configuration. Inherit security settings in every role. Read-only review is a policy
unless the existing environment already enforces it. Worktrees are not security sandboxes. Preserve
role-specific restrictions as well as parent restrictions. Before substituting explicit effort/model
controls for native role selection, establish equivalent or stricter actual enforcement of the
resolved role's sandbox, filesystem/network, approvals, and tool restrictions. If that cannot be
established, use a supported fresh session that loads them or pause that delegation. Carry this
condition into generated AGENTS so it still applies when this setup skill is not loaded.

## 3. Preview, apply, and verify

Prepare complete diffs and a capability matrix before editing the target. Distinguish an actual
applicable preview from a hypothetical candidate blocked by a path or policy conflict. In preview
mode, use an isolated fixture under an allowed scratch location; do not change target files, trust,
or credentials. Do not copy secret-bearing configuration into reports or fixtures.

For an authorized application, re-read affected files before patching to detect intervening edits.
Apply only the reconciled changes. No new approval is needed for routine authorized setup; seek a
decision only on a concrete unresolved conflict. Run the procedure in
[capabilities and validation](references/capabilities.md), including native loading and role runtime
checks where possible. TOML parsing or an agent's self-reported effort is not runtime proof.

Reapply the reconciliation in preview mode: the second diff must be empty. Check that unrelated
rules/settings and all security values are unchanged. Review the final artifact at `xhigh`; when
available, delegate an independent **read-only** review with fresh context and explicit `xhigh`. Ask
it to examine stale assumptions, unsupported keys, policy/config contradictions, security, unbounded
delegation, context explosion, architectural invention, duplicated rules, non-idempotent edits, and
silent overwrite. If that effort or delegation is unavailable, disclose the limitation and perform
the strongest available review without claiming an `xhigh` run.

## 4. Report accurately

Report changed/proposed paths, CLI version and schema provenance, resolved models/efforts,
conflicts, verification commands and results, exact immutable test target, and a per-requirement
matrix: **native configuration**, **AGENTS policy**, or **not currently enforceable**. Include
evidence state (passed, failed, not run, or unsupported) separately from capability. Configuration
defaults remain overridable; do not call them an immutable security boundary. Describe context-fork
controls and worktree handling actually available, missing capabilities, security settings left
unchanged, and how to invoke the skill. Never present a partial or blocked deployment as fully
configured.
