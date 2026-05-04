---
name: setup-python-lint
description: Set up Ruff (linter + formatter) and mypy (type checker) with strict rules encouraging functional style. Use when adding code quality checks to a Python project.
disable-model-invocation: true
argument-hint: [package name and Python target version, e.g. "myapp 3.12"]
---

# Setup Python Lint

Add Ruff as the unified linter and formatter, and mypy as the strict type checker. The configuration enforces a **functional-first style**: immutability, pure functions, comprehensions over loops, explicit types, and minimal side effects.

**The argument `$ARGUMENTS` is the package name and Python target version** (e.g. `myapp 3.12`). The package name is used for isort `known-first-party`. The Python version sets `target-version` for Ruff and `python_version` for mypy. If no argument is given, infer the package name from the project directory and detect the Python version from `pyproject.toml` `[project] requires-python`, `.python-version`, or `runtime.txt`. If none can be detected, ask the user.

## Prerequisites

- The project must have a `pyproject.toml` in the root. If not, tell the user to create one first (`uv init`, `poetry init`, or manually).
- For Nix Flake projects: the project must have `git-hooks.nix` integrated in `flake.nix`. If not, tell the user to run `/setup-git-hooks` first.
- For non-Nix projects: `pre-commit` must be installed.

## Steps

### 1. Install Ruff and mypy

#### Nix Flake projects

Verify that `pkgs.ruff` and `pkgs.mypy` are listed in `devShells.default.packages` (or `buildInputs`). If absent, add them.

#### Non-Nix projects

Check `pyproject.toml` for `ruff` and `mypy` in dev dependencies (under `[project.optional-dependencies]`, `[tool.poetry.group.dev.dependencies]`, or `[dependency-groups]`). If absent, tell the user to install them:

```bash
uv add --dev ruff mypy
```

Or if not using uv:

```bash
pip install --upgrade ruff mypy
```

Do NOT run these commands automatically — the user must choose their package manager.

### 2. Add Ruff and mypy configuration to `pyproject.toml`

If `[tool.ruff]` already exists in `pyproject.toml`, go to Step 3.

Add the configuration sections from [pyproject.ruff.toml.template](pyproject.ruff.toml.template) to `pyproject.toml`. Replace the following placeholders:

| Placeholder | Value | Source |
|---|---|---|
| `__TARGET_VERSION__` | e.g. `"py312"` | Python version from `$ARGUMENTS`, formatted as `pyNN` |
| `__MYPY_PYTHON_VERSION__` | e.g. `"3.12"` | Python version from `$ARGUMENTS` |
| `__PACKAGE_NAME__` | e.g. `"myapp"` | Package name from `$ARGUMENTS` |

### 3. Verify strict configuration

If `[tool.ruff]` already existed, verify the following critical settings. If any are missing or weaker, inform the user of the discrepancies. Do NOT overwrite — let the user decide.

Required lint settings:

| Setting | Required | Rationale |
|---|---|---|
| `"ANN"` in select | present | Type annotations must be enforced |
| `"B"` in select | present | Bugbear catches mutable defaults and common bugs |
| `"C4"` in select | present | Comprehensions over loops (functional style) |
| `"SIM"` in select | present | Code simplification |
| `"RET"` in select | present | Return simplification |
| `"FBT"` in select | present | Boolean trap detection (explicit arguments) |
| `"T20"` in select | present | Print statements forbidden in production code |
| `"ERA"` in select | present | Commented-out code forbidden |
| `"S"` in select | present | Security checks (bandit) |

Required mypy settings:

| Setting | Required | Rationale |
|---|---|---|
| `strict` | `true` | Enables all strict flags as a group |
| `warn_unreachable` | `true` | Dead code detection |
| `disallow_any_generics` | `true` | No bare `list` or `dict` — require `list[int]` |
| `no_implicit_optional` | `true` | No implicit `Optional` |

### 4. Add `from __future__ import annotations`

The Ruff configuration requires `from __future__ import annotations` in every file via `required-imports`. This enables:

- PEP 563 postponed evaluation (all annotations become strings)
- Forward references without quotes
- Cleaner type syntax (`list[int]` instead of `List[int]`)

Verify this is configured in `[tool.ruff.lint.isort]`. If not, add it.

### 5. Add pre-commit hooks

Determine the hook integration method.

#### Path A: Nix git-hooks.nix

Add the hooks from [hooks.nix.template](hooks.nix.template) into the `hooks = { ... }` block. Do NOT duplicate hooks that already exist.

Hooks added:

- **ruff-check** — `ruff check --no-fix`. Lint violations fail the commit. No auto-fix in hooks.
- **ruff-format-check** — `ruff format --check`. Formatting violations fail the commit. No auto-fix in hooks.
- **mypy-check** — `mypy .`. Type errors fail the commit.

#### Path B: Pre-commit (astral-sh/ruff-pre-commit)

Add the hooks from [pre-commit-hooks.yaml.template](pre-commit-hooks.yaml.template) to `.pre-commit-config.yaml`. Replace `__RUFF_VERSION__` with the installed Ruff version prefixed with `v` (e.g. `v0.15.0`).

Do NOT duplicate if ruff or mypy hooks already exist.

**Hook ordering**: `ruff` (lint) MUST come before `ruff-format` (format check). Both MUST come before `mypy` (type check).

### 6. Remove conflicting tools (conditional)

If the project has any of the following alongside Ruff, warn the user about the conflict. Do NOT remove anything automatically.

| Conflicting tool | Detection | Recommendation |
|---|---|---|
| flake8 | `.flake8`, `setup.cfg [flake8]`, `flake8` in deps | Ruff replaces flake8 entirely |
| Black | `[tool.black]` in pyproject.toml, `black` in deps | `ruff format` replaces Black |
| isort | `[tool.isort]` in pyproject.toml, `isort` in deps | Ruff's `I` rules replace isort |
| autopep8 | `autopep8` in deps | `ruff format` replaces autopep8 |
| pylint | `.pylintrc`, `[tool.pylint]`, `pylint` in deps | Ruff's `PL` rules cover most of pylint |
| bandit | `.bandit`, `bandit` in deps | Ruff's `S` rules replace bandit |
| pyflakes | `pyflakes` in deps | Ruff's `F` rules replace pyflakes |
| pycodestyle | `pycodestyle` in deps | Ruff's `E`/`W` rules replace pycodestyle |

List all detected conflicts and tell the user to resolve them.

### 7. Validate

Run the following commands (or tell the user to run them):

1. `ruff check .` — lint check
2. `ruff format --check .` — format check
3. `mypy .` — type check

If there are violations:

- Formatting: tell the user to run `ruff format .` to auto-fix.
- Lint (fixable): tell the user to run `ruff check --fix .` to auto-fix safe violations.
- Lint (unfixable): list them. The configuration marks `ERA001`, `F841`, `T20` as unfixable — these require manual removal.
- Type errors: list them for the user. Do NOT add `# type: ignore` comments.

## Functional Style Enforcement

The configuration promotes functional programming through the following rule groups:

| Rule Group | Enforcement |
|---|---|
| **B006** | Mutable default arguments forbidden (prevents shared state) |
| **C4** | Comprehensions required over `map()`/`filter()` with lambdas and manual loops |
| **SIM** | Ternary expressions, collapsed `if`/`else`, simplified boolean logic |
| **RET** | Unnecessary `else` after `return`, single-expression returns |
| **FBT** | Boolean positional arguments forbidden (forces keyword arguments for clarity) |
| **PIE** | Unnecessary `pass`, redundant `dict` calls, unnecessary spread |
| **FURB** | Modern idioms: `itertools` over manual loops, `isinstance` tuples |
| **PERF** | `list()` over `[]` in comprehensions, unnecessary list copies |
| **PTH** | `pathlib.Path` required over `os.path` (immutable path objects) |
| **ANN** | All function signatures must have type annotations |
| **C90** | Cyclomatic complexity capped at 10 (forces function decomposition) |
| **PL** | Max 5 args, 3 returns, 8 branches, 30 statements per function |

Additional mypy enforcement:

| Setting | Effect |
|---|---|
| `strict = true` | All type-safety flags enabled |
| `disallow_any_generics` | No bare `list`/`dict` — requires `list[int]`, `dict[str, Any]` |
| `no_implicit_optional` | `def f(x: int = None)` forbidden — must use `Optional[int]` |
| `warn_unreachable` | Dead code after `return`/`raise` is an error |

## Important Notes

- Do NOT use `ruff check --fix` or `ruff format` in pre-commit hooks. Hooks MUST be check-only. Auto-fixing in hooks can produce changes that bypass review.
- Do NOT disable `ANN` rules globally. Type annotations are mandatory for functional-style enforcement. The test file override relaxes this only for tests.
- Do NOT add `# noqa` comments without an explicit rule code. The `PGH` rules enforce `# noqa: XXXX` format.
- Do NOT add `# type: ignore` comments without an explicit error code. The mypy `ignore-without-code` error code enforces `# type: ignore[code]` format.
- The `from __future__ import annotations` requirement is non-negotiable for Python < 3.14. It enables modern annotation syntax and prevents runtime evaluation of type hints.
- The `ban-relative-imports = "all"` setting forbids relative imports. All imports must be absolute. This improves readability and prevents circular import ambiguity.
- If the project uses Django, FastAPI, or other frameworks with specific patterns, some rules may need per-file overrides. Add them to `[tool.ruff.lint.per-file-ignores]` rather than disabling rules globally.
- The `extend-immutable-calls` setting in `[tool.ruff.lint.flake8-bugbear]` includes FastAPI defaults. Remove or adjust this list based on the project's actual framework.
