#!/usr/bin/env python3
"""Regression checks for Claudius skill guardrail invariants."""

from __future__ import annotations

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def read(relative_path: str) -> str:
    return (ROOT / relative_path).read_text(encoding="utf-8")


def check_contains(errors: list[str], relative_path: str, needle: str) -> None:
    if needle not in read(relative_path):
        errors.append(f"{relative_path}: missing expected text: {needle!r}")


def check_not_contains(errors: list[str], relative_path: str, needle: str) -> None:
    if needle in read(relative_path):
        errors.append(f"{relative_path}: forbidden text is present: {needle!r}")


def check_not_regex(errors: list[str], relative_path: str, pattern: str) -> None:
    if re.search(pattern, read(relative_path), flags=re.MULTILINE):
        errors.append(f"{relative_path}: forbidden pattern matched: {pattern}")


def check_uses_are_sha_pinned(errors: list[str], relative_path: str) -> None:
    action_refs = re.findall(
        r"^\s*uses:\s*([^\s#]+)",
        read(relative_path),
        flags=re.MULTILINE,
    )
    for uses in action_refs:
        if uses.startswith("./"):
            continue
        if "@" not in uses:
            errors.append(f"{relative_path}: action is not versioned: {uses}")
            continue
        _action, ref = uses.rsplit("@", 1)
        if not re.fullmatch(r"[0-9a-f]{40}", ref):
            errors.append(f"{relative_path}: action is not pinned by full SHA: {uses}")


def check_ci_skill(errors: list[str]) -> None:
    ci_template = "config/claudius/skills/setup-nix-ci-lint/assets/lint.yml.template"
    check_uses_are_sha_pinned(errors, ci_template)
    check_contains(errors, ci_template, "contents: read")
    check_contains(errors, ci_template, "--show-diff-on-failure")
    check_not_contains(errors, ci_template, "id-token: write")
    check_not_contains(errors, ci_template, "SKIP:")
    check_not_regex(errors, ci_template, r"uses:\s+[^@\s]+@v\d+")

    ci_instructions = "config/claudius/skills/setup-nix-ci-lint/instructions.md"
    check_contains(errors, ci_instructions, "`tflint` | `nix develop -c tflint --init`")
    check_contains(errors, ci_instructions, "Do not skip hooks in CI by default.")
    check_contains(errors, ci_instructions, "`pre-commit-check.enabledPackages`")
    check_not_contains(errors, ci_instructions, "nix run nixpkgs#tflint -- --init")
    check_not_contains(
        errors,
        ci_instructions,
        "Requires `tofu init` which may need provider credentials",
    )


def check_tofu_skill(errors: list[str]) -> None:
    tofu_hook = "config/claudius/skills/setup-tofu-lint/assets/hooks.nix.template"
    check_contains(errors, tofu_hook, 'temp_data_root="$(mktemp -d)"')
    check_contains(errors, tofu_hook, "TF_DATA_DIR=\"$data_dir\"")
    check_contains(errors, tofu_hook, "TF_IN_AUTOMATION=true")
    check_contains(errors, tofu_hook, "CHECKPOINT_DISABLE=true")
    check_contains(errors, tofu_hook, ".terraform.lock.hcl")
    check_contains(errors, tofu_hook, ".tofu-root-modules")
    check_contains(errors, tofu_hook, "(^|/)\\\\.tofu-root-modules$")
    check_contains(errors, tofu_hook, "init -backend=false -lockfile=readonly")
    check_contains(errors, tofu_hook, "pass_filenames = false")
    check_contains(errors, tofu_hook, "TOFU_VALIDATE_ROOTS_FILE")
    check_contains(errors, tofu_hook, "TOFU_VALIDATE_ALLOW_NO_LOCKED_ROOTS")
    check_contains(errors, tofu_hook, "missing .terraform.lock.hcl")
    check_not_contains(errors, tofu_hook, 'dirname "$arg"')
    check_not_contains(
        errors,
        tofu_hook,
        "find . \\\n            -name '.terraform.lock.hcl'",
    )
    check_not_regex(errors, tofu_hook, r"init -backend=false(?! -lockfile=readonly)")

    formatter = "config/claudius/skills/setup-nix-lint/assets/formatter.nix.template"
    check_contains(errors, formatter, 'git ls-files -- "$@"')
    check_not_contains(errors, formatter, 'exec ${pkgs.nixfmt}/bin/nixfmt "$@"')


def check_profile_policy(errors: list[str]) -> None:
    tofu_profile = (
        "config/claudius/skills/setup-quality-profile/references/tofu-infra.md"
    )
    check_contains(errors, tofu_profile, "Use bounded provider constraints")
    check_contains(errors, tofu_profile, "Do not skip")
    check_contains(errors, tofu_profile, "Pin GitHub Actions by full commit SHA")
    check_contains(errors, tofu_profile, "Separate \"update deployment manifest\"")

    guardrails = "config/claudius/skills/setup-github-guardrails/instructions.md"
    check_contains(errors, guardrails, "full commit SHA")
    check_contains(
        errors,
        guardrails,
        "git ls-remote https://github.com/<owner>/<repo>.git 'refs/tags/<tag>^{}'",
    )
    check_contains(errors, guardrails, "If the peeled ref is absent")

    local_loop = "config/claudius/skills/setup-local-quality-loop/instructions.md"
    check_contains(errors, local_loop, "CI does not skip local hooks by default")

    tofu_project = "config/claudius/skills/setup-tofu-project/instructions.md"
    check_contains(errors, tofu_project, "`/setup-github-guardrails tofu-infra`")
    check_contains(
        errors,
        tofu_project,
        "GitHub Actions left on mutable tag references",
    )


def check_pandacss_skill(errors: list[str]) -> None:
    skill = "config/claudius/skills/setup-pandacss/skill.yaml"
    instructions = "config/claudius/skills/setup-pandacss/instructions.md"
    checker = (
        "config/claudius/skills/setup-pandacss/assets/"
        "panda-practices-check.mjs.template"
    )
    nix_hook = "config/claudius/skills/setup-pandacss/assets/hooks.nix.template"
    precommit_hook = (
        "config/claudius/skills/setup-pandacss/assets/"
        "pre-commit-hooks.yaml.template"
    )

    check_contains(errors, skill, "name: setup-pandacss")
    check_contains(errors, skill, "React Router 7")
    check_contains(errors, instructions, "strictTokens: true")
    check_contains(errors, instructions, "jsxFramework: 'react'")
    check_contains(errors, instructions, "styled-system/")
    check_contains(errors, instructions, "React Router Framework mode")
    check_contains(errors, instructions, "links")
    check_contains(errors, instructions, "staticCss")
    check_contains(errors, instructions, "`pkgs.nodejs`")
    check_contains(errors, instructions, "node scripts/check-pandacss-practices.mjs")
    check_contains(errors, instructions, "Do not rely on Panda extraction")
    check_contains(errors, checker, "strictTokens")
    check_contains(errors, checker, "inline style=")
    check_contains(errors, checker, "raw color")
    check_contains(errors, checker, "global.css")
    check_contains(errors, instructions, "PANDACSS_CHECK_TOKEN_PATHS")
    check_contains(errors, instructions, "PANDACSS_CHECK_RUNTIME_VAR_PATHS")
    check_contains(errors, checker, "pandacss-allow-runtime-var")
    check_contains(errors, nix_hook, "pandacss-practices")
    check_contains(errors, precommit_hook, "pandacss-practices")
    check_contains(errors, precommit_hook, "package\\.json")


def main() -> int:
    errors: list[str] = []

    check_ci_skill(errors)
    check_tofu_skill(errors)
    check_profile_policy(errors)
    check_pandacss_skill(errors)

    if errors:
        print("Claudius skill guardrail checks failed:", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1
    print("Claudius skill guardrail checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
