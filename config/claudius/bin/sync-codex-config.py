"""Refresh declarative Codex settings while retaining locally written state."""

from __future__ import annotations

import argparse
import copy
import json
import os
import tempfile
import tomllib
from pathlib import Path

import tomli_w


def remove_managed(local: dict, managed: dict, path: tuple[str, ...] = ()) -> None:
    """Remove owned leaves; server/provider definitions are owned as a whole."""
    for key, value in managed.items():
        if key not in local:
            continue
        if (
            isinstance(value, dict)
            and isinstance(local[key], dict)
            and path not in {("mcp_servers",), ("model_providers",)}
        ):
            remove_managed(local[key], value, (*path, key))
            if not local[key]:
                del local[key]
        else:
            del local[key]


def merge_settings(local: dict, managed: dict, path: tuple[str, ...] = ()) -> None:
    """Apply declared values while retaining unowned table entries."""
    for key, value in managed.items():
        if (
            isinstance(value, dict)
            and isinstance(local.get(key), dict)
            and path not in {("mcp_servers",), ("model_providers",)}
        ):
            merge_settings(local[key], value, (*path, key))
        else:
            local[key] = copy.deepcopy(value)


def atomic_write(path: Path, content: str) -> None:
    """Replace a file through a private temporary sibling."""
    descriptor, name = tempfile.mkstemp(prefix=".claudius-codex.", dir=path.parent)
    staged = Path(name)
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            stream.write(content)
            stream.flush()
            os.fsync(stream.fileno())
        staged.replace(path)
    finally:
        staged.unlink(missing_ok=True)


def sorted_tables(value: object) -> object:
    """Keep serialization stable across repeated ownership reconciliation."""
    if isinstance(value, dict):
        return {key: sorted_tables(value[key]) for key in sorted(value)}
    if isinstance(value, list):
        return [sorted_tables(item) for item in value]
    return value


def sync(source: Path, target: Path) -> None:
    """Reconcile managed settings and persist recoverable ownership records."""
    managed = tomllib.loads(source.read_text(encoding="utf-8"))
    local = tomllib.loads(target.read_text(encoding="utf-8")) if target.exists() else {}
    state_path = target.with_name(target.name + ".managed.json")
    state = (
        json.loads(state_path.read_text(encoding="utf-8"))
        if state_path.exists()
        else {"version": 1}
    )
    if not isinstance(state, dict) or state.get("version") != 1:
        message = "Unsupported Codex managed-state version"
        raise ValueError(message)
    for name in ("managed", "pending"):
        previous = state.get(name, {})
        if not isinstance(previous, dict):
            message = "Invalid Codex managed-state record"
            raise TypeError(message)
        remove_managed(local, previous)
    merge_settings(local, managed)
    rendered = tomli_w.dumps(sorted_tables(local))

    # Record both generations before replacing the config. If interrupted, the
    # next sync can remove either generation's owned settings and retry safely.
    pending = {"version": 1, "managed": state.get("managed", {}), "pending": managed}
    if "pending" in state:
        merge_settings(pending["managed"], state["pending"])
    target.parent.mkdir(parents=True, exist_ok=True)
    atomic_write(state_path, json.dumps(pending))
    atomic_write(target, rendered)
    atomic_write(state_path, json.dumps({"version": 1, "managed": managed}))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument("target", type=Path)
    arguments = parser.parse_args()
    sync(arguments.source, arguments.target)
