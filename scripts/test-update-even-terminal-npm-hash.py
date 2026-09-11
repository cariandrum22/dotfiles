"""Regression tests for the Even Terminal npmDepsHash updater."""

from __future__ import annotations

import importlib.util
from pathlib import Path
from tempfile import TemporaryDirectory
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from types import ModuleType


def _require_equal(actual: object, expected: object, message: str) -> None:
    if actual != expected:
        raise AssertionError(message)


def _require(*, condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def _load_updater() -> ModuleType:
    module_path = Path(__file__).with_name("update-even-terminal-npm-hash.py")
    spec = importlib.util.spec_from_file_location("update_even_terminal", module_path)
    if spec is None or spec.loader is None:
        msg = f"Could not load {module_path}"
        raise RuntimeError(msg)

    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_synchronizes_an_external_checkout() -> None:
    updater = _load_updater()
    expected_hash = "sha256-74hjtyTiBdkuzMmELjUYFe+METSKEZ1DSZXNRnS17lQ="

    with TemporaryDirectory() as temporary_directory:
        repository_root = Path(temporary_directory)
        package_directory = repository_root / updater.PACKAGE_DIRECTORY
        package_directory.mkdir(parents=True)
        lock_file = package_directory / "package-lock.json"
        nix_file = package_directory / "default.nix"
        lock_file.write_text('{}\n', encoding="utf-8")
        nix_file.write_text(
            'npmDepsHash = "sha256-old";\n',
            encoding="utf-8",
        )

        observed_lock_files: list[Path] = []

        def calculate_hash(path: Path) -> str:
            observed_lock_files.append(path)
            return expected_hash

        updater._calculate_npm_deps_hash = calculate_hash

        npm_deps_hash, changed = updater._synchronize_npm_deps_hash(repository_root)
        current_hash, changed_again = updater._synchronize_npm_deps_hash(
            repository_root,
        )

        _require_equal(npm_deps_hash, expected_hash, "unexpected updated hash")
        _require_equal(current_hash, expected_hash, "unexpected current hash")
        _require(
            condition=changed,
            message="first synchronization reported no change",
        )
        _require(
            condition=not changed_again,
            message="idempotent synchronization changed file",
        )
        _require_equal(
            observed_lock_files,
            [lock_file, lock_file],
            "updater used a lockfile outside the selected checkout",
        )
        _require_equal(
            nix_file.read_text(encoding="utf-8"),
            f'npmDepsHash = "{expected_hash}";\n',
            "Nix hash was not updated",
        )


def test_rejects_ambiguous_hash_assignments() -> None:
    updater = _load_updater()
    content = 'npmDepsHash = "sha256-one";\nnpmDepsHash = "sha256-two";\n'

    rejected = False
    try:
        updater._replace_npm_deps_hash(content, "sha256-new")
    except ValueError:
        rejected = True

    _require(
        condition=rejected,
        message="multiple npmDepsHash assignments were accepted",
    )


def main() -> None:
    test_synchronizes_an_external_checkout()
    test_rejects_ambiguous_hash_assignments()


if __name__ == "__main__":
    main()
