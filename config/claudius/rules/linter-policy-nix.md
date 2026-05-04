## Linter and Formatter Configuration Policy - Nix

### Required Tools

1. **nixfmt-rfc-style** (REQUIRED - Formatter)
   - The modern formatter following RFC 166 style guide
   - Run: `nixfmt-rfc-style .`
   - Install: `nix-env -iA nixpkgs.nixfmt-rfc-style` or add to shell.nix
   - Note: This is the community-agreed standard format for Nix code
   - **Important**: Do NOT use the deprecated `nixfmt` or `nixfmt-classic`

2. **statix** (REQUIRED - Linter)
   - Modern linter for Nix code
   - Run: `statix check`
   - Fix: `statix fix`
   - Install: `nix-env -iA nixpkgs.statix` or add to shell.nix

3. **deadnix** (REQUIRED - Dead Code Detection)
   - Finds unused code in Nix files
   - Run: `deadnix`
   - Fix: `deadnix -e`
   - Install: `nix-env -iA nixpkgs.deadnix` or add to shell.nix

4. **nil** (RECOMMENDED - Language Server)
   - Language server for IDE integration
   - Provides real-time diagnostics
   - Install: `nix-env -iA nixpkgs.nil` or add to shell.nix

### Configuration

Create `.conform.yaml` for pre-commit integration:
```yaml
policies:
  - type: commit_msg
    spec:
      - length: 80
        
  - type: file
    spec:
      - ".*.nix"
    pipeline:
      fail_fast: false
      stages:
        - type: fmt
          command: |
            nixfmt-rfc-style "$FILENAME"
        - type: lint
          command: |
            statix check "$FILENAME" && deadnix "$FILENAME"
```

### Pre-commit Hooks Configuration

For `pre-commit-hooks.nix`:
```nix
{
  pre-commit.settings = {
    hooks = {
      nixfmt-rfc-style.enable = true;  # Use this instead of deprecated nixfmt
      statix.enable = true;
      deadnix.enable = true;
    };
  };
}
```

### Shell Environment

Always include in `shell.nix` or `flake.nix`:
```nix
{
  buildInputs = with pkgs; [
    nixfmt-rfc-style
    statix
    deadnix
    nil
  ];
}
```

### Editor Integration

**VSCode**:
- Install "Nix IDE" extension
- Configure to use `nil` as language server
- Enable format on save with `nixfmt-rfc-style`

**Neovim**:
- Use `nil` with nvim-lspconfig
- Configure null-ls or conform.nvim for `nixfmt-rfc-style`

### CI/CD Integration

GitHub Actions example:
```yaml
- name: Install Nix tools
  run: |
    nix-env -iA nixpkgs.nixfmt-rfc-style nixpkgs.statix nixpkgs.deadnix
    
- name: Format check
  run: nixfmt-rfc-style --check .
  
- name: Lint
  run: |
    statix check
    deadnix
```

### Functional Programming Best Practices

Nix is a **purely functional language**, making functional patterns mandatory:

1. **Pure Functions Only**:
   - All Nix functions are pure by design
   - No side effects or mutation possible
   - Leverage this for confident refactoring

2. **Function Composition**:
   ```nix
   # Good: Compose functions
   lib.pipe value [
     (map toString)
     (filter (x: x != ""))
     (concatStringsSep ",")
   ];
   
   # Avoid: Nested function calls
   concatStringsSep "," (filter (x: x != "") (map toString value));
   ```

3. **Attribute Set Patterns**:
   ```nix
   # Good: Use update operator
   config // { newAttr = value; }
   
   # Good: Use recursion carefully
   let
     fibonacci = n:
       if n <= 1 then n
       else fibonacci (n - 1) + fibonacci (n - 2);
   in fibonacci
   ```

4. **Avoid Anti-patterns**:
   - Never use `rec` unnecessarily (breaks referential transparency)
   - Prefer `let...in` over `with` for clarity
   - Use attribute paths instead of nested `with` statements

5. **Functional Utilities**:
   - Master `lib` functions: `map`, `filter`, `fold`, `pipe`, `compose`
   - Use `mapAttrs`, `filterAttrs` for attribute sets
   - Leverage `optional`, `optionals` for conditional inclusion

### Common Issues to Check

- Unused `let` bindings
- Redundant parentheses
- Unused function arguments
- Anti-patterns like `rec { ... }`
- Missing or inconsistent indentation
- Legacy syntax patterns
- Overuse of `with` statements
- Unnecessary recursion in attribute sets