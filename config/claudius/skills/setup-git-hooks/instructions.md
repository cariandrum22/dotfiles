# Setup git-hooks.nix

Integrate [cachix/git-hooks.nix](https://github.com/cachix/git-hooks.nix) into
the project's `flake.nix`.

## Steps

1. Read the existing `flake.nix` in the project root.

2. **Add the `git-hooks` input** (if not already present):

   ```nix
   git-hooks.url = "github:cachix/git-hooks.nix";
   git-hooks.inputs.nixpkgs.follows = "nixpkgs";
   ```

   If the project uses the old name `pre-commit-hooks` (i.e.
   `github:cachix/pre-commit-hooks.nix`), rename it to `git-hooks` and update
   all references throughout the file.

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
