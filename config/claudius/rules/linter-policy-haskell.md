## Linter and Formatter Configuration Policy - Haskell

### Required Tools

1. **ormolu** (REQUIRED - Formatter)
   - Most opinionated and strict formatter
   - Zero configuration by design
   - Run: `ormolu --mode inplace .`
   - Check: `ormolu --mode check .`
   - Install: `stack install ormolu` or `cabal install ormolu`

2. **hlint** (REQUIRED - Linter)
   - Suggests improvements and detects anti-patterns
   - Run: `hlint .`
   - Apply suggestions: `hlint . --refactor --refactor-options="--inplace"`
   - Install: `stack install hlint` or `cabal install hlint`

3. **stan** (REQUIRED - Static Analysis)
   - Advanced static analyzer for Haskell
   - Run: `stan`
   - Install: `stack install stan` or `cabal install stan`

4. **haskell-language-server** (REQUIRED - LSP)
   - Provides IDE features and real-time diagnostics
   - Install: `ghcup install hls`

### Build Tool Configuration

**For Stack projects** - `stack.yaml`:
```yaml
resolver: lts-21.22  # Use latest LTS
packages:
- .

ghc-options:
  "$everything": -Wall -Wcompat -Widentities -Wincomplete-record-updates
                  -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints
                  -Werror
```

**For Cabal projects** - `cabal.project`:
```cabal
packages: .

package *
  ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates
               -Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints
               -Werror
```

### Package Configuration

In your `.cabal` file or `package.yaml`:

**Cabal format**:
```cabal
common warnings
  ghc-options:
    -Wall
    -Wcompat
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wpartial-fields
    -Wredundant-constraints
    -Wmissing-export-lists
    -Wmissing-home-modules
    -Werror
    -O2

library
  import: warnings
  -- other settings
```

**Stack/hpack format** (`package.yaml`):
```yaml
ghc-options:
- -Wall
- -Wcompat
- -Widentities
- -Wincomplete-record-updates
- -Wincomplete-uni-patterns
- -Wpartial-fields
- -Wredundant-constraints
- -Wmissing-export-lists
- -Wmissing-home-modules
- -Werror
- -O2

default-extensions:
- NoImplicitPrelude  # Force explicit imports
- OverloadedStrings
- TypeApplications
- ScopedTypeVariables
```

### HLint Configuration

Create `.hlint.yaml`:
```yaml
# Strict HLint configuration
- arguments: [--color=always]

# Warnings to enforce
- error: {lhs: "map f (map g x)", rhs: "map (f . g) x"}
- error: {lhs: "concat (map f x)", rhs: "concatMap f x"}
- error: {lhs: "if x then True else False", rhs: "x"}
- error: {lhs: "x == True", rhs: "x"}

# Require explicit imports
- error: {name: Use explicit module imports}

# Ban dangerous functions
- error: {name: [unsafePerformIO, unsafeCoerce, undefined]}

# Custom rules for the project
- group:
    name: generalise
    enabled: true
```

### Stan Configuration

Create `.stan.toml`:
```toml
# Observations to exclude
[[check]]
type = "Exclude"
id = "OBS-STAN-0201"  # Exclude specific observations if needed

# Custom inspections
[[check]]
type = "Include"
id = "STAN-0101"  # Infinite recursion
severity = "Error"

[[check]]
type = "Include" 
id = "STAN-0203"  # Pattern match on `_`
severity = "Error"
```

### Pre-commit Integration

Create `.pre-commit-config.yaml`:
```yaml
repos:
  - repo: local
    hooks:
      - id: ormolu
        name: ormolu
        entry: ormolu --mode check
        language: system
        types: [haskell]
        
      - id: hlint
        name: hlint
        entry: hlint
        language: system
        types: [haskell]
```

### Editor Integration

**VSCode**:
- Install "Haskell" extension (uses HLS)
- Settings:
```json
{
  "haskell.formattingProvider": "ormolu",
  "editor.formatOnSave": true,
  "[haskell]": {
    "editor.defaultFormatter": "haskell.haskell"
  }
}
```

**Neovim**:
- Use `haskell-tools.nvim` with HLS
- Configure formatting with ormolu

### CI/CD Integration

GitHub Actions:
```yaml
- name: Setup Haskell
  uses: haskell/actions/setup@v2
  with:
    ghc-version: '9.4.8'
    enable-stack: true
    stack-version: 'latest'

- name: Install tools
  run: |
    stack install ormolu hlint stan
    
- name: Format check
  run: ormolu --mode check $(find . -name '*.hs')

- name: Lint
  run: hlint .

- name: Static analysis
  run: stan

- name: Build with warnings as errors
  run: stack build --pedantic
```

### Functional Programming Excellence

Haskell is a **purely functional language**. Enforce these practices:

1. **Purity and Referential Transparency**:
   ```haskell
   -- Good: Pure functions only
   add :: Int -> Int -> Int
   add x y = x + y
   
   -- Good: Explicit IO type for effects
   readConfig :: FilePath -> IO Config
   readConfig path = do
     content <- readFile path
     pure $ parseConfig content
   ```

2. **Total Functions**:
   ```haskell
   -- Good: Use NonEmpty for safe head
   import Data.List.NonEmpty (NonEmpty(..))
   
   safeHead :: NonEmpty a -> a
   safeHead (x :| _) = x
   
   -- Good: Return Maybe for partial operations
   lookup :: Eq a => a -> [(a, b)] -> Maybe b
   ```

3. **Advanced Type System Usage**:
   ```haskell
   -- Good: Phantom types for safety
   newtype UserId = UserId Int
   newtype ProductId = ProductId Int
   
   -- Good: GADTs for type-safe DSLs
   data Expr a where
     Lit :: a -> Expr a
     Add :: Num a => Expr a -> Expr a -> Expr a
   
   -- Good: Type families
   type family Elem (a :: Type) :: Type
   ```

4. **Algebraic Effects with Monad Transformers**:
   ```haskell
   -- Good: Explicit effect stacks
   type App = ReaderT Config (ExceptT AppError IO)
   
   -- Good: MTL style for abstraction
   doWork :: (MonadReader Config m, MonadError AppError m, MonadIO m) => m Result
   ```

5. **Property-Based Testing**:
   ```haskell
   -- Good: QuickCheck properties
   prop_reverse :: [Int] -> Bool
   prop_reverse xs = reverse (reverse xs) == xs
   
   -- Good: Hedgehog for stateful testing
   ```

6. **Avoid Common Anti-patterns**:
   - Never use `String` for text (use `Text`)
   - Never use `head`, `tail`, `!!` (use safe variants)
   - Never use partial record accessors
   - Always handle all pattern matches
   - Prefer `newtype` over type aliases

### Required Language Extensions

Always enable in `default-extensions`:
```yaml
- BangPatterns          # Strict evaluation control
- ConstraintKinds       # Type constraint aliases
- DataKinds            # Type-level data
- DeriveFunctor        # Automatic functor instances
- DerivingStrategies   # Explicit deriving
- FlexibleContexts     # Flexible type contexts
- FlexibleInstances    # Flexible instance heads
- GADTs                # Generalized ADTs
- GeneralizedNewtypeDeriving
- LambdaCase          # Pattern matching lambdas
- MultiParamTypeClasses
- OverloadedStrings   # String literals as Text
- RankNTypes          # Higher-rank polymorphism
- ScopedTypeVariables # Scoped type variables
- StandaloneDeriving  # Standalone deriving
- TypeApplications    # Visible type application
- TypeFamilies       # Type families
- TypeOperators      # Type operators
- ViewPatterns       # View patterns
```

### Performance and Strictness

```haskell
-- Good: Use strict data types
data User = User
  { userId   :: !Int
  , userName :: !Text
  } deriving (Eq, Show)

-- Good: Use BangPatterns for strict evaluation
sum' :: [Int] -> Int
sum' = go 0
  where
    go !acc []     = acc
    go !acc (x:xs) = go (acc + x) xs
```