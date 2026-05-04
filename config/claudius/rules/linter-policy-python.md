## Linter and Formatter Configuration Policy - Python

### Required Tools

1. **ruff** (REQUIRED - All-in-One Linter/Formatter)
   - Modern, fast replacement for: flake8, black, isort, pyupgrade, autoflake
   - Run lint: `ruff check .`
   - Run format: `ruff format .`
   - Auto-fix: `ruff check --fix .`
   - Install: `pip install ruff` or `pipx install ruff`

2. **mypy** (REQUIRED - Type Checker)
   - Static type checking
   - Run: `mypy .`
   - Install: `pip install mypy`

### Do NOT Use (Replaced by ruff)
- [DEPRECATED] black (formatting)
- [DEPRECATED] isort (import sorting)
- [DEPRECATED] flake8 (linting)
- [DEPRECATED] pylint (linting)
- [DEPRECATED] autopep8 (formatting)
- [DEPRECATED] pycodestyle (style checking)

### Configuration

Create `pyproject.toml`:
```toml
[tool.ruff]
target-version = "py38"  # Adjust to your minimum Python version
line-length = 88
select = [
    "E",   # pycodestyle errors
    "W",   # pycodestyle warnings
    "F",   # pyflakes
    "I",   # isort
    "N",   # pep8-naming
    "UP",  # pyupgrade
    "B",   # flake8-bugbear
    "C4",  # flake8-comprehensions
    "DTZ", # flake8-datetimez
    "RUF", # Ruff-specific rules
]

[tool.ruff.isort]
known-first-party = ["your_package_name"]

[tool.mypy]
python_version = "3.8"
strict = true
warn_return_any = true
warn_unused_configs = true
disallow_untyped_defs = true
disallow_incomplete_defs = true
check_untyped_defs = true
no_implicit_optional = true
warn_redundant_casts = true
warn_unused_ignores = true
```

### Pre-commit Configuration

Create `.pre-commit-config.yaml`:
```yaml
repos:
  - repo: https://github.com/astral-sh/ruff-pre-commit
    rev: v0.1.6  # Check for latest version
    hooks:
      - id: ruff
        args: [ --fix ]
      - id: ruff-format
        
  - repo: https://github.com/pre-commit/mirrors-mypy
    rev: v1.7.1  # Check for latest version
    hooks:
      - id: mypy
        additional_dependencies: [types-all]
```

### Shell Environment

For `requirements-dev.txt`:
```txt
ruff>=0.1.6
mypy>=1.7.1
pytest>=7.4.3  # for testing
pytest-cov>=4.1.0  # for coverage
```

### Editor Integration

**VSCode**:
```json
{
  "python.linting.enabled": false,
  "[python]": {
    "editor.formatOnSave": true,
    "editor.defaultFormatter": "charliermarsh.ruff",
    "editor.codeActionsOnSave": {
      "source.fixAll": true,
      "source.organizeImports": true
    }
  }
}
```

**Neovim**:
- Use `ruff-lsp` for LSP integration
- Configure null-ls or conform.nvim for formatting

### CI/CD Integration

GitHub Actions example:
```yaml
- name: Install dependencies
  run: |
    pip install -r requirements-dev.txt
    
- name: Lint with ruff
  run: |
    ruff check .
    ruff format --check .
    
- name: Type check with mypy
  run: mypy .
```

### Package Structure Requirements

- Always use `src/` layout for packages
- Include `py.typed` marker for typed packages
- Use `__init__.py` files even in Python 3.3+
- Keep test files in `tests/` directory separate from source

### Common Ruff Rules to Enable

- `UP`: Automatically upgrade syntax for newer Python versions
- `B`: Catch common bugs (mutable defaults, etc.)
- `C4`: Simplify comprehensions
- `SIM`: Simplify code patterns
- `TCH`: Move type-checking imports into TYPE_CHECKING blocks

### Functional Programming Approaches

Python has **moderate functional support**. Apply these patterns where appropriate:

1. **Immutable Data Patterns**:
   ```python
   from typing import NamedTuple, FrozenSet
   from dataclasses import dataclass, replace
   
   # Good: Immutable data structures
   @dataclass(frozen=True)
   class User:
       name: str
       age: int
   
   # Good: Create new instances instead of mutation
   user = User("Alice", 30)
   updated_user = replace(user, age=31)
   
   # Good: Tuple for immutable sequences
   Point = NamedTuple("Point", [("x", float), ("y", float)])
   
   # Avoid: Mutable class attributes
   class BadUser:
       def __init__(self, name: str):
           self.name = name  # Mutable
   ```

2. **Pure Functions and Comprehensions**:
   ```python
   # Good: List comprehensions (functional style)
   squared = [x**2 for x in numbers if x > 0]
   
   # Good: Generator expressions for lazy evaluation
   sum_of_squares = sum(x**2 for x in numbers if x > 0)
   
   # Good: Pure function with type hints
   def calculate_total(items: list[float]) -> float:
       """Pure function that calculates sum without side effects."""
       return sum(items)
   
   # Avoid: Side effects in functions
   total = 0
   def bad_calculate(items: list[float]) -> None:
       global total  # Side effect
       for item in items:
           total += item
   ```

3. **Higher-Order Functions**:
   ```python
   from functools import reduce, partial
   from operator import add, mul
   
   # Good: Using map, filter, reduce
   doubled = list(map(lambda x: x * 2, numbers))
   evens = list(filter(lambda x: x % 2 == 0, numbers))
   product = reduce(mul, numbers, 1)
   
   # Good: Partial application
   add_five = partial(add, 5)
   result = add_five(10)  # Returns 15
   
   # Good: Functions returning functions
   def multiplier(factor: float) -> Callable[[float], float]:
       return lambda x: x * factor
   ```

4. **Functional Utilities**:
   ```python
   from itertools import chain, groupby, takewhile
   from functools import lru_cache
   import toolz  # Or cytoolz for performance
   
   # Good: Lazy evaluation with itertools
   chained = chain.from_iterable(nested_lists)
   grouped = groupby(sorted_data, key=lambda x: x.category)
   
   # Good: Memoization for pure functions
   @lru_cache(maxsize=None)
   def fibonacci(n: int) -> int:
       if n < 2:
           return n
       return fibonacci(n-1) + fibonacci(n-2)
   
   # Good: Function composition (with toolz)
   from toolz import compose, pipe
   process = compose(str.upper, str.strip)
   ```

5. **Type-Safe Optional Values**:
   ```python
   from typing import Optional, Union
   
   # Good: Explicit optional handling
   def find_user(user_id: int) -> Optional[User]:
       """Returns None if user not found."""
       return users.get(user_id)
   
   # Good: Result type pattern
   from typing import TypeVar, Generic
   T = TypeVar('T')
   E = TypeVar('E')
   
   @dataclass(frozen=True)
   class Result(Generic[T, E]):
       value: Optional[T] = None
       error: Optional[E] = None
       
       @property
       def is_ok(self) -> bool:
           return self.error is None
   ```

6. **Avoid Mutation**:
   ```python
   # Good: Create new collections
   original = [1, 2, 3]
   extended = [*original, 4]  # New list
   
   # Good: Dictionary merging
   base = {"a": 1, "b": 2}
   updated = {**base, "c": 3}  # New dict
   
   # Avoid: In-place mutations
   original.append(4)  # Mutates original
   base["c"] = 3  # Mutates base
   ```

7. **Ruff Configuration for Functional Style**:
   ```toml
   [tool.ruff]
   select = [
       "F",    # Pyflakes
       "E",    # pycodestyle errors
       "W",    # pycodestyle warnings
       "I",    # isort
       "N",    # pep8-naming
       "UP",   # pyupgrade
       "B",    # flake8-bugbear (catches mutable defaults)
       "C4",   # flake8-comprehensions
       "SIM",  # flake8-simplify
       "RUF",  # Ruff-specific rules
       "FA",   # flake8-future-annotations
       "ISC",  # flake8-implicit-str-concat
       "PIE",  # flake8-pie (misc lints)
   ]
   
   [tool.ruff.flake8-bugbear]
   extend-immutable-calls = ["functools.partial", "toolz.curry"]
   ```