# Setup git-hooks.nix

Integrate [cachix/git-hooks.nix](https://github.com/cachix/git-hooks.nix) into
the project's `flake.nix`.

## Existing Configuration Policy

Before changing an existing repository file, inspect the current content and ask the user to confirm the proposed change. This applies even when the change is additive, such as merging config keys, appending CI steps, adding package scripts, normalizing workflow names, or updating tool versions.

Do not ask when creating a missing file from this skill's template or when the user explicitly requested applying all changes without confirmation. Preserve project-specific settings and avoid replacing entire files unless the user approves that replacement.

When an existing file is involved, present a concise change plan before editing:

- `file`: target path
- `current_state`: what exists and whether this skill owns it
- `operation`: `skip`, `merge`, `update`, `replace`, or `create-adjacent`
- `proposed_delta`: exact setting, block, command, or path change to add or modify
- `risk`: compatibility, policy, or behavior risk
- `question`: the approval needed from the user

Default to `skip` or `merge`. Use `replace` only when the user explicitly
approves replacing that file.

If the user explicitly requested applying all changes without confirmation, do
not wait for approval after presenting the plan. Still follow the plan, preserve
project-specific settings, and do not use `replace` unless the user explicitly
allowed replacement.

Otherwise, if multiple existing files are affected, batch them in one plan and
wait for approval before editing any of them.

## Steps

1. Read the existing `flake.nix` in the project root.

2. **Add the `git-hooks` input** (if not already present):

   ```nix
   git-hooks.url = "github:cachix/git-hooks.nix";
   git-hooks.inputs.nixpkgs.follows = "nixpkgs";
   ```

   If the project uses the old name `pre-commit-hooks` (i.e.
   `github:cachix/pre-commit-hooks.nix`), inspect all references first, report
   the proposed rename from `pre-commit-hooks` to `git-hooks`, and ask the user
   before changing it. When approved, update all references consistently in the
   same change.

3. **Add `git-hooks` to the outputs function parameters** (if not already
   present).

4. **Add hook helper functions** in the `let` block (if not already present):

   ```nix
   mkHook = name: entry: {
     enable = true;
     inherit name entry;
     language = "system";
     pass_filenames = false;
   };
   mkHookWithStages =
     name: entry: stages:
     (mkHook name entry) // { inherit stages; };
   ```

5. **Add the `pre-commit-check` binding** (if not already present):

   ```nix
   pre-commit-check = git-hooks.lib.${system}.run {
     src = ./.;
     hooks = {
       # hooks go here
     };
   };
   ```

6. **Wire it into `devShells.default`**:
   - Add `pre-commit-check.enabledPackages` to `packages`.
   - Add `${pre-commit-check.shellHook}` (or `inherit (pre-commit-check)
     shellHook;`) to `shellHook`.

7. **Optionally expose as a check**:

   ```nix
   checks.pre-commit-check = pre-commit-check;
   ```

## Important Notes

- Do NOT remove any existing hooks already defined in `flake.nix`.
- Preserve the existing `devShells.default` structure; only add the necessary
  wiring.
- If `flake-utils` is used, keep the existing `eachSystem` /
  `eachDefaultSystem` pattern.
- Run `nix flake check` mentally to verify the structure is valid, but do not
  actually run it unless asked.
