# Polyglot OSS Quality Profile

Use this profile for a repository that mixes one or more general-purpose
language surfaces such as Rust, Haskell, Lean, Python, Ruby, and TypeScript
without clearly fitting the existing Rails-monorepo or OpenTofu-infra
archetypes.

## Commit Scope Taxonomy

Keep the scope taxonomy small and durable.

Shared baseline scopes:

- `ci`
- `config`
- `deps`
- `docs`
- `nix`
- `release`
- `scripts`
- `security`
- `tests`
- `tooling`

Add durable repository domains only when they represent clear ownership
surfaces, for example:

- `api`
- `cli`
- `compiler`
- `core`
- `crates`
- `packages`
- `parser`
- `proofs`
- `runtime`
- `sdk`
- `web`

Do NOT default to language names as commit scopes unless the repository really
uses language-segmented top-level ownership such as `rust/`, `python/`, or
`lean/`.

## Workflow Naming

Preferred workflow display names:

- `CI - Lint` for repo-wide policy and low-cost validation
- `CI - Rust`
- `CI - Haskell`
- `CI - Lean`
- `CI - Python`
- `CI - Ruby`
- `CI - TypeScript`
- `Deploy - <Target>`
- `Release - <Target>`

Avoid generic names such as `CI`, `Checks`, `Build`, or `Deploy`.

## Job Naming

Prefer stable human-readable job `name:` fields for checks that may become
required:

- `Lint`
- `Rust Lint`
- `Haskell Lint`
- `Lean Lint`
- `Python Lint`
- `Ruby Lint`
- `TypeScript Lint`

Keep the display name stable even if the underlying command changes.

## Step Naming

Use `Verb + target` naming. Add path qualifiers when there are multiple app or
package surfaces.

Good examples:

- `Install Rust dependencies`
- `Run Haskell format checks`
- `Run Lean builtin lint`
- `Run Python type checks`
- `Install web dependencies (packages/web)`
- `Lint PR title`

Avoid generic steps such as `Setup`, `Run script`, or `Checks`.

## Local Hook Naming

Preferred hook names:

- `rust-lint`
- `haskell-lint`
- `lean-lint`
- `python-lint`
- `ruby-lint`
- `ts-lint`
- `ts-typecheck`
- `commitlint`
- `commitlint-pre-push`

Use stronger domain-oriented hook names only when the repository already has a
clear surface contract that is better than language-level naming.

## Linter Policy

### Rust

The Rust baseline should include:

- `cargo fmt --all -- --check`
- `cargo clippy --workspace --all-targets --all-features`
- `-D warnings`
- `clippy::pedantic`
- `clippy::cargo`
- explicit denials for `unwrap`, `expect`, `panic`, `todo`, and `unimplemented`

### Haskell

The Haskell baseline should include:

- `fourmolu --mode check .`
- `hlint .`
- a strict build with `-Wall`, `-Wcompat`, and warnings treated as errors

### Lean

The Lean baseline should include:

- `lake build`
- `lake lint --clippy` by default
- preservation of project-specific lint drivers or custom Lake lint executables

Formatting stays project-specific unless the repository already exposes a
checked formatter command.

### Python

Use `/setup-python-lint`.

### Ruby

Use `/setup-ruby-lint` for general Ruby surfaces.

If the repository also uses typed Ruby, layer `/setup-sorbet` on top.

### TypeScript

For plain TS and JS packages, prefer `/setup-ts-typecheck` and `/setup-biome`.

For framework-heavy, browser-extension, GraphQL, security-sensitive, or
functional-rule-heavy surfaces, prefer `/setup-typed-eslint-monorepo`.

## CI Policy

- Use `fetch-depth: 0` in lint workflows.
- Install language dependencies before running `pre-commit`.
- Keep commitlint and PR-title lint in the repo-wide `CI - Lint` workflow.
- Keep hooks and CI check-only.
- Split language-specific workflows only when their dependency boot cost or run
  time justifies a dedicated surface.

## Required Checks

Typical minimum:

- `CI - Lint / Lint`

Add language-specific checks to branch protection only when they are stable,
always-on, and clearly owned by the repository.
