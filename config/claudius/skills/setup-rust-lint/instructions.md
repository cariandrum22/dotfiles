# Setup Rust Lint

Add `cargo fmt` and strict `cargo clippy` checks for a Cargo project. This
skill standardizes on the community formatter and a strict Clippy gate with
check-only hooks.

## Prerequisites

- The project must have a root `Cargo.toml`.
- The active Rust toolchain must provide `cargo`, `rustfmt`, and `clippy`.
- For Nix Flake projects: ensure the chosen Rust toolchain in
  `devShells.default.packages` exposes the `rustfmt` and `clippy` components.
- For non-Nix projects: if the components are missing, tell the user to run
  `rustup component add rustfmt clippy`. Do NOT run it automatically.

## Steps

### 1. Determine hook integration method

Check whether `flake.nix` exists in the project root and already contains a
`git-hooks` or `pre-commit-hooks` input.

- If yes: follow Path A (Nix `git-hooks.nix`).
- If no: follow Path B (pre-commit local hooks).

Do NOT apply both paths.

---

### Path A: Nix git-hooks.nix

#### A-1. Add hooks to `flake.nix`

Add the hooks from [hooks.nix.template](assets/hooks.nix.template) into the
`hooks = { ... }` block inside `git-hooks.lib.${system}.run`. Do NOT duplicate
hooks that already exist.

Hooks added:

- `cargo-fmt-check` - runs `cargo fmt --all -- --check`
- `cargo-clippy-strict` - runs strict `cargo clippy` checks across the whole
  workspace

#### A-2. Ensure the Rust toolchain is in the dev shell

Verify the project's dev shell provides `cargo`, `rustfmt`, and `clippy`. If
the toolchain is already managed via overlays or `fenix`, preserve the existing
pattern and only fill gaps.

---

### Path B: Pre-commit local hooks

#### B-1. Add hooks to `.pre-commit-config.yaml`

If `.pre-commit-config.yaml` does not exist, create it.

Add the hooks from
[pre-commit-hooks.yaml.template](assets/pre-commit-hooks.yaml.template) to the
`repos` list. Do NOT duplicate hooks that already exist.

Hooks added:

- `cargo-fmt-check` - runs `cargo fmt --all -- --check`
- `cargo-clippy-strict` - runs the strict Clippy gate for the workspace

### 2. Preserve existing Clippy configuration

If `clippy.toml` or `.clippy.toml` already exists, preserve it.

Do NOT create a Clippy config file by default. Keep the baseline strictness
explicit in hooks and CI so the policy remains visible and portable across
toolchain managers.

### 3. Validate

Run the following commands, or tell the user to run them:

1. `cargo fmt --all -- --check`
2. `cargo clippy --workspace --all-targets --all-features -- -D warnings -W clippy::pedantic -W clippy::cargo -D clippy::unwrap_used -D clippy::expect_used -D clippy::panic -D clippy::todo -D clippy::unimplemented`

If `cargo fmt` reports formatting drift, tell the user to run `cargo fmt --all`
manually and review the diff.

If Clippy reports violations, keep the lint levels intact and fix the code. Do
NOT add `#[allow]` attributes broadly just to make the hook pass.

## Important Notes

- Keep hooks and CI check-only. Do NOT add `cargo fmt` without `--check` or
  `cargo clippy --fix` to hooks.
- Prefer hook- or CI-level lint flags over crate-level `#![deny(...)]` changes
  unless the repository explicitly wants source-local lint policy.
- If a workspace is too large for one Clippy job, split the CI surface by
  package or domain, but keep the same lint-level set everywhere.
