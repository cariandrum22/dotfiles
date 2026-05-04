## Linter and Formatter Configuration Policy - PureScript

### Required Tools

1. **purs-tidy** (REQUIRED - Formatter)
   - Official PureScript formatter
   - Run: `purs-tidy format-in-place 'src/**/*.purs' 'test/**/*.purs'`
   - Check: `purs-tidy check 'src/**/*.purs'`
   - Install: `npm install -g purs-tidy`

2. **purs** (REQUIRED - Compiler/Linter)
   - PureScript compiler with comprehensive warnings
   - Run: `purs compile 'src/**/*.purs'`
   - Install: `npm install -g purescript`

3. **spago** (REQUIRED - Build Tool)
   - Package manager and build tool
   - Run: `spago build`
   - Install: `npm install -g spago`

4. **purescript-language-server** (REQUIRED - LSP)
   - Language server for IDE support
   - Install: `npm install -g purescript-language-server`

5. **psa** (RECOMMENDED - Pretty Errors)
   - Pretty, flexible error/warning reporting
   - Install: `npm install -g purescript-psa`
   - Use: `spago build --purs-args "--json-errors" | psa`

### Purs-tidy Configuration

Create `.tidyrc.json`:
```json
{
  "importSort": "source",
  "importWrap": "source",
  "indent": 2,
  "operatorsFile": null,
  "ribbon": 1,
  "typeArrowPlacement": "first",
  "unicode": "never",
  "width": 100
}
```

### Compiler Warnings Configuration

In `spago.dhall`:
```dhall
{ name = "my-project"
, dependencies = [ "console", "effect", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purs"
, build =
  { pedantic_packages = true
  , strict = true
  , censor_warnings =
    [ "ShadowedName"
    , "MissingTypeDeclaration"
    , "WildcardInferredType"
    , "ImplicitImport"
    , "UnusedImport"
    , "UnusedExplicitImport"
    , "DeprecatedOperatorDecl"
    ]
  }
}
```

### Project Structure

```
purescript-project/
├── .tidyrc.json
├── .gitignore
├── packages.dhall
├── spago.dhall
├── src/
│   ├── Main.purs
│   ├── Data/
│   │   └── Types.purs
│   ├── Effect/
│   │   └── App.purs
│   └── Control/
│       └── Logic.purs
├── test/
│   ├── Main.purs
│   └── Spec/
│       └── AppSpec.purs
├── output/           # Compiled output
└── .spago/          # Dependencies
```

### Module Structure Best Practices

```purescript
-- | Module documentation using Haddock-style comments
-- | This module provides core application types
module App.Data.Types
  ( User(..)           -- Export data constructor
  , UserId             -- Export type only
  , createUser         -- Export smart constructor
  , updateUser
  -- Grouped exports
  , module Exports
  ) where

import Prelude

-- Re-exports
import Data.Maybe (Maybe(..)) as Exports
import Data.Either (Either(..)) as Exports

-- Type definitions with documentation
-- | Unique identifier for users
newtype UserId = UserId Int

derive instance newtypeUserId :: Newtype UserId _
derive instance eqUserId :: Eq UserId
derive instance ordUserId :: Ord UserId

-- | User data type
-- | Invariant: age must be non-negative
data User = User
  { id :: UserId
  , name :: String
  , age :: Int
  }

derive instance eqUser :: Eq User
derive instance genericUser :: Generic User _

instance showUser :: Show User where
  show = genericShow

-- Smart constructors for validation
-- | Create a user with validation
createUser :: UserId -> String -> Int -> Maybe User
createUser userId name age
  | age >= 0 && name /= "" = Just (User { id: userId, name, age })
  | otherwise = Nothing
```

### Type Safety Patterns

```purescript
module App.Control.Validation where

import Prelude
import Data.Either (Either(..))
import Data.Validation.Semigroup (V, invalid)

-- Use phantom types for type safety
data Validated
data Unvalidated

newtype Email :: forall k. k -> Type
newtype Email validation = Email String

-- Smart constructors with validation
validateEmail :: String -> Either String (Email Validated)
validateEmail str
  | contains "@" str = Right (Email str)
  | otherwise = Left "Invalid email format"

-- Validation composition
type ValidationError = Array String

validateUser 
  :: String 
  -> Int 
  -> V ValidationError User
validateUser name age = ado
  validName <- validateName name
  validAge <- validateAge age
  in User { name: validName, age: validAge }
  where
  validateName n
    | n == "" = invalid ["Name cannot be empty"]
    | otherwise = pure n
    
  validateAge a
    | a < 0 = invalid ["Age must be non-negative"]
    | a > 150 = invalid ["Age seems unrealistic"]
    | otherwise = pure a
```

### Effect Management

```purescript
module App.Effect.Logger where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console

-- Define custom effects
data LogLevel = Debug | Info | Warning | Error

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

-- Type class for logging capability
class MonadEffect m <= MonadLogger m where
  log :: LogLevel -> String -> m Unit

-- Concrete implementation
newtype LoggerT m a = LoggerT (m a)

derive newtype instance functorLoggerT :: Functor m => Functor (LoggerT m)
derive newtype instance applyLoggerT :: Apply m => Apply (LoggerT m)
derive newtype instance applicativeLoggerT :: Applicative m => Applicative (LoggerT m)
derive newtype instance bindLoggerT :: Bind m => Bind (LoggerT m)
derive newtype instance monadLoggerT :: Monad m => Monad (LoggerT m)
derive newtype instance monadEffectLoggerT :: MonadEffect m => MonadEffect (LoggerT m)

instance monadLoggerLoggerT :: MonadEffect m => MonadLogger (LoggerT m) where
  log level msg = liftEffect $ Console.log $ "[" <> show level <> "] " <> msg

-- Helper functions
debug :: forall m. MonadLogger m => String -> m Unit
debug = log Debug

info :: forall m. MonadLogger m => String -> m Unit
info = log Info
```

### Functional Patterns

```purescript
module App.Data.Patterns where

import Prelude
import Data.Array as Array
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Traversable (traverse)

-- Algebraic Data Types
data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

derive instance functorTree :: Functor Tree

-- Recursion schemes
foldTree :: forall a b. (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree f _ (Leaf x) = f x
foldTree f g (Branch l r) = g (foldTree f g l) (foldTree f g r)

-- Higher-order functions
-- | Apply a function n times
applyN :: forall a. Int -> (a -> a) -> a -> a
applyN n f = go n
  where
  go 0 x = x
  go m x = go (m - 1) (f x)

-- Optics (using profunctor-lenses)
-- | Example lens definition
_name :: forall r a. Lens' { name :: a | r } a
_name = lens _.name (_ { name = _ })

-- Type-safe builders
type UserBuilder =
  { name :: Maybe String
  , age :: Maybe Int
  , email :: Maybe String
  }

emptyBuilder :: UserBuilder
emptyBuilder = { name: Nothing, age: Nothing, email: Nothing }

buildUser :: UserBuilder -> Maybe User
buildUser builder = do
  name <- builder.name
  age <- builder.age
  email <- builder.email
  createUser name age email
```

### Testing Configuration

Create `test/Main.purs`:
```purescript
module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "User validation" do
    it "creates valid users" do
      let result = createUser "Alice" 30 "alice@example.com"
      isJust result `shouldEqual` true
      
    it "rejects invalid age" do
      let result = createUser "Bob" (-1) "bob@example.com"
      isJust result `shouldEqual` false
```

### Pre-commit Configuration

Create `.pre-commit-config.yaml`:
```yaml
repos:
  - repo: local
    hooks:
      - id: purs-tidy
        name: PureScript Formatter
        entry: purs-tidy check
        language: system
        files: \.purs$
        
      - id: spago-build
        name: PureScript Build
        entry: spago build
        language: system
        pass_filenames: false
        files: \.purs$
```

### CI/CD Integration

GitHub Actions example:
```yaml
name: PureScript CI

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v4
    
    - name: Setup Node.js
      uses: actions/setup-node@v4
      with:
        node-version: '20'
        
    - name: Install PureScript toolchain
      run: |
        npm install -g purescript@0.15.13
        npm install -g spago@0.21.0
        npm install -g purs-tidy@0.10.0
        npm install -g purescript-psa
        
    - name: Install dependencies
      run: spago install
      
    - name: Check formatting
      run: purs-tidy check 'src/**/*.purs' 'test/**/*.purs'
      
    - name: Build with warnings
      run: |
        spago build --purs-args "--json-errors" 2>&1 | psa --strict --censor-lib --censor-codes=ImplicitImport,UnusedExplicitImport
        
    - name: Run tests
      run: spago test
```

### Editor Integration

**VSCode**:
```json
{
  "[purescript]": {
    "editor.defaultFormatter": "nwolverson.ide-purescript",
    "editor.formatOnSave": true,
    "editor.tabSize": 2
  },
  "purescript.addNpmPath": true,
  "purescript.buildCommand": "spago build --purs-args --json-errors",
  "purescript.censorWarnings": [
    "ShadowedName",
    "MissingTypeDeclaration"
  ]
}
```

### Common Anti-patterns to Avoid

1. **Partial Functions**: Never use `unsafePartial` without justification
2. **String Types**: Use newtype wrappers for domain concepts
3. **Boolean Blindness**: Use custom ADTs instead of booleans
4. **Orphan Instances**: Define instances in same module as type
5. **Large Modules**: Keep modules focused and cohesive
6. **Missing Type Signatures**: Always annotate top-level definitions
7. **Implicit Imports**: Use explicit imports for clarity

### Performance Considerations

```purescript
-- Use strict evaluation when needed
import Data.Function (on)
import Data.Array.ST as STArray
import Control.Monad.ST as ST

-- Efficient array operations
fastSum :: Array Int -> Int
fastSum arr = ST.run do
  total <- STRef.new 0
  STArray.foreach arr \x -> do
    current <- STRef.read total
    STRef.write (current + x) total
  STRef.read total

-- Prefer tail recursion
factorial :: Int -> Int
factorial = go 1
  where
  go acc 0 = acc
  go acc n = go (acc * n) (n - 1)
```