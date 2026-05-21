#!/usr/bin/env python3
"""Regression checks for Claudius skill guardrail invariants."""
# ruff: noqa: PLR0915, S404

from __future__ import annotations

import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
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


def check_contains_normalized(
    errors: list[str],
    relative_path: str,
    needle: str,
) -> None:
    haystack = " ".join(read(relative_path).split())
    normalized_needle = " ".join(needle.split())
    if normalized_needle not in haystack:
        errors.append(f"{relative_path}: missing expected text: {needle!r}")


def check_not_regex(errors: list[str], relative_path: str, pattern: str) -> None:
    if re.search(pattern, read(relative_path), flags=re.MULTILINE):
        errors.append(f"{relative_path}: forbidden pattern matched: {pattern}")


def check_max_line_length(
    errors: list[str],
    relative_path: str,
    max_length: int,
) -> None:
    for line_number, line in enumerate(read(relative_path).splitlines(), start=1):
        if len(line) > max_length:
            errors.append(
                f"{relative_path}:{line_number}: line exceeds {max_length} chars",
            )


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
    check_contains(errors, ci_instructions, "Do not replace the")
    check_contains(errors, ci_instructions, "workflow wholesale")
    check_contains(errors, ci_instructions, "merge/update it in place")
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
    check_contains(errors, tofu_hook, "tofu fmt -check -diff")
    check_contains(errors, tofu_hook, "hclfmt-check")
    check_contains(errors, tofu_hook, 'cmp -s "$file" "$formatted"')
    check_contains(errors, tofu_hook, 'diff -u "$file" "$formatted"')
    check_contains(errors, tofu_hook, "TOFU_VALIDATE_ROOTS_FILE")
    check_contains(errors, tofu_hook, "TOFU_VALIDATE_ALLOW_NO_LOCKED_ROOTS")
    check_contains(errors, tofu_hook, "missing .terraform.lock.hcl")
    check_not_contains(errors, tofu_hook, 'entry = "${pkgs.opentofu}/bin/tofu fmt";')
    check_not_contains(errors, tofu_hook, "${pkgs.hclfmt}/bin/hclfmt -w")
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


def check_commitlint_skill(errors: list[str]) -> None:
    template = "config/claudius/skills/setup-commitlint/assets/ci-steps.yml.template"
    instructions = "config/claudius/skills/setup-commitlint/instructions.md"
    pre_push = (
        "config/claudius/skills/setup-commitlint/assets/"
        "commitlint-pre-push.sh.template"
    )

    check_contains(errors, template, "BEFORE_SHA: ${{ github.event.before }}")
    check_contains(
        errors,
        template,
        "DEFAULT_BRANCH: ${{ github.event.repository.default_branch }}",
    )
    check_contains(errors, template, "BASE_REF: ${{ github.base_ref }}")
    check_contains(errors, template, "PR_TITLE: ${{ github.event.pull_request.title }}")
    check_contains(errors, template, 'printf \'%s\\n\' "$PR_TITLE"')
    check_contains(errors, template, "git rev-parse --is-shallow-repository")
    check_contains(errors, template, "git fetch --no-tags --prune --unshallow origin")
    check_contains(errors, template, 'git cat-file -e "$BEFORE_SHA^{commit}"')
    check_contains(
        errors,
        template,
        'git log -1 --format=%B "$PUSH_SHA" > "$tmp_msg"',
    )
    check_contains(
        errors,
        template,
        '"+refs/heads/${DEFAULT_BRANCH}:refs/remotes/origin/${DEFAULT_BRANCH}"',
    )
    check_contains(
        errors,
        template,
        '"+refs/heads/${BASE_REF}:refs/remotes/origin/${BASE_REF}"',
    )
    check_not_contains(errors, template, '--depth=1')
    check_not_contains(
        errors,
        template,
        'printf \'%s\\n\' "${{ github.event.pull_request.title }}"',
    )
    check_contains(errors, instructions, "ask the user before")
    check_contains(errors, instructions, "replacing or rewriting")
    check_contains(errors, instructions, "report the proposed scope/rule delta")
    check_contains(errors, instructions, "ask the user before changing it")
    check_contains(errors, instructions, "approved, clearly missing baseline rules")
    check_contains(errors, pre_push, 'remote_name="${1:-origin}"')
    check_contains(errors, pre_push, '"refs/remotes/${remote_name}/HEAD"')
    check_contains(errors, pre_push, '"refs/remotes/${remote_name}/main"')
    check_contains(errors, pre_push, '"refs/remotes/${remote_name}/master"')
    check_not_contains(errors, pre_push, "refs/remotes/origin/HEAD")


def check_tftpl_lint_skill(errors: list[str]) -> None:
    script = "config/claudius/skills/setup-tftpl-lint/assets/tftpl-validate.sh.template"
    instructions = "config/claudius/skills/setup-tftpl-lint/instructions.md"

    check_contains(errors, script, "hcl_string()")
    check_contains(errors, script, 'file_expr="$(hcl_string "${file}")"')
    check_contains(errors, script, 'vars_expr="$(hcl_string "${vars_file}")"')
    check_contains(errors, script, '"${engine}" -chdir="${console_dir}" console')
    check_contains(errors, script, 'console >/dev/null <<TFTPL_EOF')
    check_contains(errors, script, "tftpl-validate: failed to render")
    check_contains(
        errors,
        script,
        "templatefile(${file_expr}, jsondecode(file(${vars_expr})))",
    )
    check_not_contains(errors, script, '"${engine}" console -chdir=')
    check_not_contains(
        errors,
        script,
        'templatefile("${file}", jsondecode(file("${vars_file}")))',
    )
    check_contains(errors, instructions, "discard rendered template stdout")
    check_contains(errors, instructions, "Do NOT print")


def check_biome_skill(errors: list[str]) -> None:
    template = "config/claudius/skills/setup-biome/assets/biome.json.template"
    instructions = "config/claudius/skills/setup-biome/instructions.md"

    try:
        config = json.loads(read(template))
    except json.JSONDecodeError as error:
        errors.append(f"{template}: invalid JSON template: {error}")
        return
    check_max_line_length(errors, template, 100)

    assist_actions = (
        config.get("assist", {})
        .get("actions", {})
        .get("source", {})
    )
    if assist_actions.get("organizeImports") != "on":
        errors.append(
            f"{template}: assist.actions.source.organizeImports must be \"on\"",
        )
    if "organizeImports" in config:
        errors.append(f"{template}: top-level organizeImports is invalid for Biome 2")

    rules = config.get("linter", {}).get("rules", {})
    correctness = rules.get("correctness", {})
    style = rules.get("style", {})
    if "useArrayLiterals" in correctness:
        errors.append(f"{template}: useArrayLiterals belongs under style rules")
    if style.get("useArrayLiterals") != "error":
        errors.append(f"{template}: style.useArrayLiterals must be \"error\"")

    overrides = config.get("overrides", [])
    if not any("includes" in override for override in overrides):
        errors.append(f"{template}: Biome overrides must use includes")
    errors.extend(
        f"{template}: Biome overrides must not use include"
        for override in overrides
        if "include" in override
    )

    check_contains(
        errors,
        instructions,
        "`assist.actions.source.organizeImports`",
    )
    check_contains(errors, instructions, "The template enables `vcs.useIgnoreFile`")
    check_contains(errors, instructions, "If `.gitignore` is missing")
    check_contains(errors, instructions, "create an empty `.gitignore`")
    check_contains(errors, instructions, "requires the file to exist")
    check_not_contains(errors, instructions, "organizeImports.enabled")


def check_lean_lint_skill(errors: list[str]) -> None:
    script = "config/claudius/skills/setup-lean-lint/assets/lean-lint-check.sh.template"
    instructions = "config/claudius/skills/setup-lean-lint/instructions.md"
    skill = "config/claudius/skills/setup-lean-lint/skill.yaml"
    profile = "config/claudius/skills/setup-quality-profile/references/polyglot-oss.md"

    check_contains(errors, script, "lake build")
    check_contains(errors, script, "lake lint")
    check_contains(errors, script, "set +e")
    check_contains(errors, script, "lint_status=$?")
    check_contains(errors, script, "no lint driver configured")
    check_contains(errors, script, "LEAN_LINT_REQUIRE_DRIVER")
    check_not_contains(errors, script, "lake lint --clippy")
    check_not_contains(errors, script, "lake lint --lint-all")
    check_not_contains(errors, script, "LEAN_LINT_SCOPE")

    check_contains(errors, instructions, "Lake does not provide a universal")
    check_contains(errors, instructions, "`--clippy` or `--lint-all`")
    check_contains(errors, instructions, "`LEAN_LINT_REQUIRE_DRIVER=1`")
    check_contains(errors, skill, "Lake lint-driver checks")
    check_contains(
        errors,
        profile,
        "`lake lint` when a project lint driver is configured",
    )
    check_not_contains(errors, profile, "lake lint --clippy")


def check_just_skill(errors: list[str]) -> None:
    template = "config/claudius/skills/setup-just/assets/justfile.template"
    instructions = "config/claudius/skills/setup-just/instructions.md"

    check_contains(errors, template, "set positional-arguments")
    check_contains(errors, template, "set export")
    check_contains(errors, template, "{{ project_name }}")
    check_not_contains(errors, template, "set positional-arguments := true")
    check_not_contains(errors, template, "set export := true")
    check_not_contains(errors, template, "{{project_name}}")
    check_contains(errors, instructions, "just --fmt --check --unstable")


def check_profile_policy(errors: list[str]) -> None:
    tofu_profile = (
        "config/claudius/skills/setup-quality-profile/references/tofu-infra.md"
    )
    check_contains(errors, tofu_profile, "Use bounded provider constraints")
    check_contains(errors, tofu_profile, "Do not skip")
    check_contains(errors, tofu_profile, "Pin GitHub Actions by full commit SHA")
    check_contains(errors, tofu_profile, "Separate \"update deployment manifest\"")

    polyglot_profile = (
        "config/claudius/skills/setup-quality-profile/references/polyglot-oss.md"
    )
    check_contains(errors, polyglot_profile, "`ts-typecheck`")
    check_contains(errors, polyglot_profile, "`ruff-check`")
    check_contains(errors, polyglot_profile, "`mypy-check`")
    check_contains(errors, polyglot_profile, "`tsc-noEmit`")
    check_contains(
        errors,
        polyglot_profile,
        "do not rename existing component hook IDs",
    )

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

    quality_profile = "config/claudius/skills/setup-quality-profile/instructions.md"
    check_contains(errors, quality_profile, "profile=rails-monorepo")
    check_contains(errors, quality_profile, "profile=polyglot-oss")


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
    check_contains_normalized(errors, instructions, "report the proposed config delta")
    for needle in [
        "If `panda.config.ts` already exists",
        "Merge only missing compatible",
        "If the root CSS file already exists",
        "report the proposed Panda",
        "ask the user before adding or changing imports",
    ]:
        check_contains(errors, instructions, needle)
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


def check_file_hygiene_skill(errors: list[str]) -> None:
    hooks = "config/claudius/skills/setup-file-hygiene/assets/hooks.nix.template"
    instructions = "config/claudius/skills/setup-file-hygiene/instructions.md"

    check_contains(errors, hooks, "final-newline-check")
    check_contains(errors, hooks, "trailing-whitespace-check")
    check_contains(errors, hooks, "git ls-files -z")
    check_contains(errors, hooks, "grep -Iq")
    check_contains(errors, hooks, "last_byte=")
    check_contains(errors, hooks, "[[:blank:]]$")
    check_contains(errors, hooks, "builtins.pathExists ./.editorconfig")
    check_contains(errors, hooks, 'args = [ "--fix=no" ];')
    check_not_contains(errors, hooks, "editorconfig-checker.enable = true")
    check_not_contains(errors, hooks, "end-of-file-fixer.enable = true")
    check_not_contains(errors, hooks, "trim-trailing-whitespace.enable = true")
    check_not_contains(errors, hooks, "mixed-line-endings.enable = true")
    check_contains(errors, hooks, "builtins.pathExists ./.markdownlint.json")
    check_contains(
        errors,
        hooks,
        "builtins.fromJSON (builtins.readFile ./.markdownlint.json)",
    )
    check_not_contains(
        errors,
        hooks,
        (
            "settings.configuration = builtins.fromJSON "
            "(builtins.readFile ./.markdownlint.json);"
        ),
    )
    check_contains(errors, hooks, "builtins.pathExists ./.typos.toml")
    check_contains(errors, hooks, '{ configPath = ".typos.toml"; }')
    check_contains(errors, hooks, "{ config = { }; }")
    check_not_contains(errors, hooks, 'settings.configPath = ".typos.toml";')
    check_contains(errors, instructions, "defaults are used when missing")
    check_contains(
        errors,
        instructions,
        "editorconfig-checker is disabled when missing",
    )
    check_contains(errors, instructions, "configPath` only when the file exists")
    check_contains(errors, instructions, "tracked non-empty text files")
    check_contains(errors, instructions, "Keep file hygiene hooks check-only")
    check_contains(errors, instructions, "explicitly accepts auto-fix behavior")
    check_contains(errors, instructions, "configured with `--fix=no`")


def check_rubocop_skill(errors: list[str]) -> None:
    template = "config/claudius/skills/setup-rubocop/assets/rubocop.yml.template"
    instructions = "config/claudius/skills/setup-rubocop/instructions.md"

    if "__RUBY_VERSION__" in read(template):
        check_contains(errors, instructions, "__RUBY_VERSION__")
        check_contains(errors, instructions, ".ruby-version")
        check_contains(errors, instructions, ".tool-versions")
        check_contains(errors, instructions, "mise.toml")
        check_contains(errors, instructions, "Gemfile")
        check_contains(errors, instructions, "Do not leave the `__RUBY_VERSION__`")
    check_contains(errors, instructions, "If `.rubocop.yml` already exists")
    check_contains(errors, instructions, "ask the user before changing")
    check_contains(errors, instructions, "Do NOT duplicate plugin")
    check_contains(errors, instructions, "existing plugin order")


def check_rails_ci_skill(errors: list[str]) -> None:
    template = "config/claudius/skills/setup-rails-ci/assets/ci.yml.template"
    instructions = "config/claudius/skills/setup-rails-ci/instructions.md"

    check_uses_are_sha_pinned(errors, template)
    check_contains(errors, template, "contents: read")
    check_contains(errors, template, '      - "**"')
    check_contains(errors, template, "  test:")
    check_contains(errors, template, "name: Test")
    check_contains(errors, template, "ruby-version: __RUBY_VERSION__")
    check_not_contains(errors, template, "branches: [ main ]")
    check_not_contains(errors, template, "ruby-version: .ruby-version")
    check_not_regex(errors, template, r"uses:\s+[^@\s]+@v\d+")
    check_contains(errors, instructions, "push CI for all branches")
    check_contains(errors, instructions, "three jobs")
    check_not_contains(errors, instructions, "two jobs")
    check_contains(errors, instructions, "__RUBY_VERSION__")
    check_contains(errors, instructions, ".tool-versions")
    check_contains(errors, instructions, "mise.toml")
    check_contains(errors, instructions, "bundle exec rspec")
    check_contains(errors, instructions, "bin/rails test")
    check_contains(errors, instructions, "bundle exec brakeman --no-pager")
    check_contains(errors, instructions, "bundle exec rubocop -f github")
    check_contains(errors, instructions, "If `simplecov` is present")
    check_contains(errors, instructions, "permissions: contents: read")
    check_contains(errors, instructions, "full commit SHA")
    check_contains(errors, instructions, "Do not switch these actions back")
    check_contains(errors, instructions, "Do not replace the workflow")
    check_contains(errors, instructions, "wholesale")
    check_contains(errors, instructions, "merge/update it in place")


def check_rails_testing_skill(errors: list[str]) -> None:
    instructions = "config/claudius/skills/setup-rails-testing/instructions.md"

    check_contains(errors, instructions, "rspec-rails")
    check_contains(errors, instructions, "spec/spec_helper.rb")
    check_contains(errors, instructions, "spec/rails_helper.rb")
    check_contains(errors, instructions, "before any other requires")
    check_contains(errors, instructions, "before application boot")
    check_contains(errors, instructions, "SimpleCov is already configured")
    check_contains(errors, instructions, "do not add a duplicate block")
    check_contains(errors, instructions, "If `.gitignore` exists")
    check_contains(errors, instructions, "ask the user before appending")
    check_not_contains(
        errors,
        instructions,
        "If not already present, add `coverage/` to `.gitignore`.",
    )


def check_repository_baseline_skill(errors: list[str]) -> None:
    instructions = (
        "config/claudius/skills/setup-repository-baseline/instructions.md"
    )

    check_contains(errors, instructions, "__REPOSITORY_NAME__")
    check_contains(errors, instructions, "__DEFAULT_BRANCH__")
    check_contains(errors, instructions, "__PRIMARY_OWNER__")
    check_contains(errors, instructions, "__SECURITY_CONTACT__")
    check_contains(errors, instructions, "Do not leave any")
    for needle in [
        "report missing baseline policy items",
        "report missing baseline security policy items",
        "report whether `Unreleased`",
        "report missing release-process items",
        "report missing PR-description fields",
        "inspect it and ask the user before changing it",
    ]:
        check_contains(errors, instructions, needle)


def check_i18n_lint_skill(errors: list[str]) -> None:
    template = "config/claudius/skills/setup-i18n-lint/assets/i18n-tasks.yml.template"
    instructions = "config/claudius/skills/setup-i18n-lint/instructions.md"

    check_contains(errors, template, "base_locale: __BASE_LOCALE__")
    check_contains(errors, template, "locales: __LOCALES__")
    check_not_contains(errors, template, "base_locale: ja")
    check_not_contains(errors, template, "locales: [ja, en]")
    check_contains(errors, instructions, "__BASE_LOCALE__")
    check_contains(errors, instructions, "__LOCALES__")
    check_contains(errors, instructions, "Do not assume `ja`")
    check_contains_normalized(
        errors,
        instructions,
        "Do not append duplicate rule blocks",
    )
    check_contains(errors, instructions, "If `config/i18n-tasks.yml` already exists")
    check_contains(errors, instructions, "If `.github/workflows/ci.yml` exists")
    check_contains(errors, instructions, "ask the user before")
    check_contains(errors, instructions, "Do NOT duplicate")
    check_contains(errors, instructions, "i18n-tasks missing")
    check_contains_normalized(errors, instructions, "Preserve the existing workflow")


def check_stylelint_skill(errors: list[str]) -> None:
    instructions = "config/claudius/skills/setup-stylelint/instructions.md"
    template = "config/claudius/skills/setup-stylelint/assets/stylelintrc.yml.template"

    check_contains(errors, instructions, "Do not switch package managers")
    check_contains(errors, instructions, "npm run lint:css")
    check_contains(errors, instructions, "pnpm lint:css")
    check_contains(errors, instructions, "yarn lint:css")
    check_contains(errors, instructions, "bun run lint:css")
    check_contains(errors, instructions, "If a Stylelint config already exists")
    check_contains(errors, instructions, "Do not overwrite existing scripts")
    check_contains(errors, instructions, "If `.github/workflows/ci.yml` exists")
    check_contains(errors, instructions, "ask the user before")
    check_contains(errors, instructions, "Do NOT duplicate")
    check_contains_normalized(errors, instructions, "Preserve the existing workflow")
    check_not_contains(errors, instructions, "run: yarn lint:css")
    check_not_contains(errors, instructions, "Run the following to install")
    check_contains(errors, template, 'ignoreValues: ["currentColor"')
    check_contains(errors, template, "expandShorthand: true")
    check_contains(errors, template, "scss/load-no-partial-leading-underscore")
    check_not_contains(errors, template, "expandShorthandProperties")
    check_not_contains(errors, template, "scss/at-import-no-partial-leading-underscore")


def check_rails_db_safety_skill(errors: list[str]) -> None:
    template = (
        "config/claudius/skills/setup-rails-db-safety/assets/"
        "strong_migrations.rb.template"
    )
    instructions = "config/claudius/skills/setup-rails-db-safety/instructions.md"

    if "__PG_VERSION__" in read(template):
        check_contains(errors, instructions, "__PG_VERSION__")
        check_contains(errors, instructions, "PostgreSQL major version")
        check_contains(errors, instructions, "config/database.yml")
        check_contains(errors, instructions, "Docker Compose")
        check_contains(errors, instructions, "Do not leave the `__PG_VERSION__`")
        check_contains(errors, instructions, "If the initializer already exists")
        check_contains(
            errors,
            instructions,
            "If `.database_consistency.yml` already exists",
        )


def check_tofu_project_skill(errors: list[str]) -> None:
    instructions = "config/claudius/skills/setup-tofu-project/instructions.md"
    content = read(instructions)
    after_completion = content.split("## After Completion", 1)[1]
    required = after_completion.split("Also mention these optional", 1)[0]

    check_contains(errors, instructions, "required config files")
    check_contains(errors, instructions, "optional override files")
    check_contains(errors, instructions, "markdownlint uses defaults")
    check_contains(errors, instructions, "typos uses defaults")
    check_contains(errors, instructions, "without explicit scopes")
    check_not_contains(errors, instructions, "ask the user for the list of scopes")
    if ".markdownlint.json" in required:
        errors.append(
            f"{instructions}: .markdownlint.json is listed as a required config",
        )
    if ".typos.toml" in required:
        errors.append(f"{instructions}: .typos.toml is listed as a required config")


def check_c_lint_skill(errors: list[str]) -> None:
    instructions = "config/claudius/skills/setup-c-lint/instructions.md"
    hooks = "config/claudius/skills/setup-c-lint/assets/hooks.nix.template"
    precommit = (
        "config/claudius/skills/setup-c-lint/assets/"
        "pre-commit-hooks.yaml.template"
    )
    script = (
        "config/claudius/skills/setup-c-lint/assets/"
        "clang-tidy-check.sh.template"
    )

    check_contains(errors, hooks, 'files = "\\\\.(c|cc|cpp|cxx|h|hh|hpp|hxx)$";')
    check_contains(errors, hooks, "pass_filenames = true")
    check_contains(errors, precommit, "\\.(c|cc|cpp|cxx|h|hh|hpp|hxx)$")
    check_contains(errors, script, "git ls-files '*.c' '*.cc' '*.cpp' '*.cxx'")
    check_contains(errors, script, 'exec clang-tidy -p "${build_dir}" "${files[@]}"')
    check_not_contains(errors, script, "No input files provided to clang-tidy.")
    check_contains(errors, instructions, "If `.clang-format` already exists")
    check_contains(errors, instructions, "If `.clang-tidy` already exists")
    check_contains(errors, instructions, "If the script already exists")


def check_just_lint_skill(errors: list[str]) -> None:
    instructions = "config/claudius/skills/setup-just-lint/instructions.md"
    hooks = "config/claudius/skills/setup-just-lint/assets/hooks.nix.template"

    check_contains(errors, hooks, 'mkHook "just-evaluate" "just --evaluate"')
    check_not_contains(errors, hooks, "just --evaluate --justfile")
    check_contains(errors, instructions, "ask the user before changing")
    check_contains(
        errors,
        instructions,
        "Do NOT remove or modify any existing hooks without explicit user approval.",
    )
    check_not_contains(
        errors,
        instructions,
        "remove it. This repository has been abandoned since January 2024",
    )


def check_google_workspace_iac_skill(errors: list[str]) -> None:
    skill = "config/claudius/skills/setup-google-workspace-iac/skill.yaml"
    instructions = "config/claudius/skills/setup-google-workspace-iac/instructions.md"

    check_contains(errors, skill, "name: setup-google-workspace-iac")
    check_contains(errors, skill, "Google Workspace")
    check_contains(errors, skill, "Cloud Identity")
    check_contains(errors, skill, "allow-implicit-invocation: false")

    for needle in [
        "Separate identity governance from Google Cloud project infrastructure.",
        "Never manage passwords, recovery channels, backup codes, or one-time secrets",
        "authoritative",
        "additive",
        "catalog-only",
        "Do not convert a group to authoritative management unless the user approves",
        "domain-wide delegation scopes minimal",
        "Treat Google Workspace exports",
        "Do not commit raw admin exports",
        "`data_sensitivity`",
        "`commit_allowed`",
        "Store only sanitized reproducible command output in `results/`",
        (
            "identity/access artifacts committed, sanitized, or kept outside "
            "the repository"
        ),
        "AWS IAM Identity Center",
        "Do not let a Google Workspace state mutate AWS account assignments directly.",
        "Do not run an apply until the user has reviewed",
    ]:
        check_contains(errors, instructions, needle)


def check_multicloud_iac_structure_skill(errors: list[str]) -> None:
    instructions = (
        "config/claudius/skills/setup-multicloud-iac-structure/instructions.md"
    )

    check_contains(
        errors,
        instructions,
        "Determine the target clouds from `$ARGUMENTS`",
    )
    check_contains(errors, instructions, "clouds outside the confirmed scope")
    check_contains(errors, instructions, "Create only the selected `stacks/<cloud>`")
    check_contains(errors, instructions, "only for selected clouds")
    check_contains(errors, instructions, "only for governed regions")
    check_not_contains(
        errors,
        instructions,
        "Create `stacks/aws`, `stacks/azure`, `stacks/gcp`",
    )


def check_typed_eslint_profile_selection(errors: list[str]) -> None:
    instructions = (
        "config/claudius/skills/setup-typed-eslint-monorepo/instructions.md"
    )

    check_contains(errors, instructions, "profile=<rails-monorepo|polyglot-oss>")
    check_contains(errors, instructions, "profile=polyglot-oss")
    check_contains(errors, instructions, "Read the quality-profile reference")
    check_contains(
        errors,
        instructions,
        "Do not read or apply the `rails-monorepo` profile",
    )
    check_contains(errors, instructions, "Use the selected profile reference")


def check_shell_template_formatting(errors: list[str]) -> None:
    shfmt = shutil.which("shfmt")
    if shfmt is None:
        errors.append("shfmt is required to validate shell skill templates")
        return

    shell_templates = sorted(
        str(path.relative_to(ROOT))
        for path in (ROOT / "config/claudius/skills").glob("*/assets/*.sh.template")
    )
    if not shell_templates:
        return

    result = subprocess.run(
        [shfmt, "-i", "2", "-d", *shell_templates],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        details = (result.stdout + result.stderr).strip()
        errors.append(f"shell templates are not shfmt-formatted:\n{details}")


def check_skill_catalog_inventory(errors: list[str]) -> None:
    skills_dir = ROOT / "config/claudius/skills"
    skill_names = {path.name for path in skills_dir.iterdir() if path.is_dir()}
    docs = [
        "config/claudius/docs/skills/catalog.yaml",
        "config/claudius/docs/skills/inventory.yaml",
    ]

    for relative_path in docs:
        documented: set[str] = set()
        in_skills = False
        for line in read(relative_path).splitlines():
            if line == "skills:":
                in_skills = True
                continue
            if in_skills and line and not line.startswith(" "):
                break
            match = re.match(r"^  ([A-Za-z0-9_-]+):\s*$", line)
            if in_skills and match:
                documented.add(match.group(1))
        missing = sorted(skill_names - documented)
        extra = sorted(documented - skill_names)
        if missing:
            errors.append(f"{relative_path}: missing skill entries: {missing}")
        if extra:
            errors.append(f"{relative_path}: stale skill entries: {extra}")


def check_setup_skill_safety_policy(errors: list[str]) -> None:
    for skill_dir in sorted((ROOT / "config/claudius/skills").glob("setup-*")):
        instructions = skill_dir / "instructions.md"
        skill_yaml = skill_dir / "skill.yaml"
        instructions_path = str(instructions.relative_to(ROOT))
        skill_yaml_path = str(skill_yaml.relative_to(ROOT))

        check_contains(errors, instructions_path, "## Existing Configuration Policy")
        check_contains_normalized(
            errors,
            instructions_path,
            "Before changing an existing repository file",
        )
        check_contains_normalized(errors, instructions_path, "ask the user to confirm")
        check_contains_normalized(
            errors,
            instructions_path,
            "even when the change is additive",
        )
        check_contains_normalized(
            errors,
            instructions_path,
            (
                "When an existing file is involved, present a concise change "
                "plan before editing"
            ),
        )
        check_contains(errors, instructions_path, "`file`: target path")
        check_contains(errors, instructions_path, "`current_state`:")
        check_contains(
            errors,
            instructions_path,
            "`operation`: `skip`, `merge`, `update`, `replace`, or `create-adjacent`",
        )
        check_contains(errors, instructions_path, "`proposed_delta`:")
        check_contains(errors, instructions_path, "`risk`:")
        check_contains(errors, instructions_path, "`question`:")
        check_contains(errors, instructions_path, "Default to `skip` or `merge`")
        check_contains(
            errors,
            instructions_path,
            "Use `replace` only when the user explicitly",
        )
        check_contains_normalized(
            errors,
            instructions_path,
            (
                "If the user explicitly requested applying all changes without "
                "confirmation"
            ),
        )
        check_contains_normalized(
            errors,
            instructions_path,
            "do not wait for approval after presenting the plan",
        )
        check_contains_normalized(
            errors,
            instructions_path,
            "Still follow the plan, preserve",
        )
        check_contains_normalized(
            errors,
            instructions_path,
            "do not use `replace` unless the user explicitly",
        )
        check_contains_normalized(
            errors,
            instructions_path,
            "Otherwise, if multiple existing files are affected",
        )
        check_contains(
            errors,
            instructions_path,
            "batch",
        )
        check_contains_normalized(
            errors,
            instructions_path,
            "wait for approval before editing any of them",
        )
        check_contains(errors, skill_yaml_path, "allow-implicit-invocation: false")
        check_not_contains(errors, skill_yaml_path, "  codex:\n    invocation: manual")
        check_not_contains(errors, instructions_path, "\\x27")
        check_not_contains(
            errors,
            instructions_path,
            "there is no valid reason to deviate",
        )
        check_not_contains(
            errors,
            instructions_path,
            "replace** them with the values below",
        )


def check_codex_rendered_setup_skill_policy(errors: list[str]) -> None:
    claudius = os.environ.get("CLAUDIUS_BIN", "claudius")
    with tempfile.TemporaryDirectory(prefix="claudius-codex-render-") as temp_dir:
        env = os.environ.copy()
        env["XDG_CONFIG_HOME"] = str(ROOT / "config")
        result = subprocess.run(
            [
                claudius,
                "skills",
                "render",
                "--agent",
                "codex",
                "--output",
                temp_dir,
                "--prune",
            ],
            cwd=ROOT,
            env=env,
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode != 0:
            errors.append(
                "codex skill render failed: "
                f"{result.stderr.strip() or result.stdout.strip()}",
            )
            return

        for skill_dir in sorted((ROOT / "config/claudius/skills").glob("setup-*")):
            relative_policy_path = Path(skill_dir.name) / "agents/openai.yaml"
            policy_path = Path(temp_dir) / relative_policy_path
            if not policy_path.exists():
                errors.append(
                    (
                        "codex render: missing "
                        f"{relative_policy_path} for manual setup skill"
                    ),
                )
                continue
            policy = policy_path.read_text(encoding="utf-8")
            if "allow_implicit_invocation: false" not in policy:
                errors.append(
                    (
                        "codex render: "
                        f"{relative_policy_path} must disable implicit invocation"
                    ),
                )


def check_git_hooks_skill(errors: list[str]) -> None:
    instructions = "config/claudius/skills/setup-git-hooks/instructions.md"

    check_contains_normalized(errors, instructions, "report the proposed rename")
    check_contains_normalized(errors, instructions, "ask the user before changing it")
    check_contains(errors, instructions, "update all references consistently")


def check_ruby_lint_skill(errors: list[str]) -> None:
    template = "config/claudius/skills/setup-ruby-lint/assets/rubocop.yml.template"
    instructions = "config/claudius/skills/setup-ruby-lint/instructions.md"

    check_contains(errors, template, "  # __OPTIONAL_PLUGINS__")
    check_not_contains(errors, template, "\n__OPTIONAL_PLUGINS__")
    check_contains(errors, instructions, "the full `  # __OPTIONAL_PLUGINS__` line")
    check_contains(errors, instructions, "If no optional surface is detected")
    check_contains(errors, instructions, "remove the placeholder line entirely")
    check_contains(errors, instructions, "Do not leave `__OPTIONAL_PLUGINS__`")


def check_command_style_skills(errors: list[str]) -> None:
    gemini_skill = "config/claudius/skills/gemini-web-search/skill.yaml"
    gemini_instructions = "config/claudius/skills/gemini-web-search/instructions.md"
    check_contains(errors, gemini_skill, "explicitly asks")
    check_contains(errors, gemini_instructions, "only when the user explicitly asks")
    check_contains(errors, gemini_instructions, "Do not claim it is more accurate")
    check_not_contains(errors, gemini_skill, "instead of built-in web search")
    check_not_contains(errors, gemini_instructions, "Ready to Search")
    check_not_contains(errors, gemini_instructions, "Please provide your search query")

    suggest = "config/claudius/skills/suggest-commit/instructions.md"
    check_contains(errors, suggest, "git diff --cached --stat")
    check_contains(errors, suggest, "git diff --cached")
    check_contains(errors, suggest, "Do not commit unless")
    check_not_contains(errors, suggest, "Please share the output")
    check_not_contains(errors, suggest, "Ready to Analyze")

    organize = "config/claudius/skills/organize-context/instructions.md"
    check_contains(errors, organize, "Target Discovery")
    check_contains(errors, organize, "config/claudius/rules")
    check_contains(errors, organize, "selected rules directory")
    check_contains(errors, organize, "Before changing an existing context document")
    check_contains(errors, organize, "ask the user to confirm")
    check_not_contains(errors, organize, "Scan `/rules/` directory")
    check_not_contains(errors, organize, "All rules in `/rules/`")
    check_not_contains(errors, organize, "Ready to Organize")
    check_not_contains(errors, organize, "Please provide:")


def check_python_lint_placeholders(errors: list[str]) -> None:
    instructions = "config/claudius/skills/setup-python-lint/instructions.md"

    check_contains(errors, instructions, "Use unquoted replacement values")
    check_contains(errors, instructions, "The template already includes TOML quotes")
    check_contains(
        errors,
        instructions,
        "Inspect `pyproject.toml` for Ruff and mypy sections independently",
    )
    check_contains(errors, instructions, "Do not skip")
    check_contains(
        errors,
        instructions,
        "mypy setup just because Ruff is already configured",
    )
    check_contains(errors, instructions, "If `[tool.mypy]` is missing")
    check_not_contains(errors, instructions, 'e.g. `"py312"`')
    check_not_contains(errors, instructions, 'e.g. `"3.12"`')
    check_not_contains(errors, instructions, 'e.g. `"myapp"`')
    template = (
        "config/claudius/skills/setup-python-lint/assets/"
        "pyproject.ruff.toml.template"
    )
    check_not_contains(errors, template, "ANN101")
    check_not_contains(errors, template, "ANN102")


def check_existing_file_merge_policy_details(errors: list[str]) -> None:
    expectations = {
        "config/claudius/skills/setup-commitlint/instructions.md": [
            "If the script already exists",
            "Merge the missing merge-base behavior",
        ],
        "config/claudius/skills/setup-erb-lint/instructions.md": [
            "If `.erb-lint.yml` already exists",
            "If the initializer already exists",
            "If `.github/workflows/ci.yml` exists",
            "ask the user before",
            "Do NOT duplicate",
            "Preserve the existing workflow",
        ],
        "config/claudius/skills/setup-haskell-lint/instructions.md": [
            "If the script already exists",
            "Merge missing strict-check behavior",
        ],
        "config/claudius/skills/setup-lean-lint/instructions.md": [
            "If the script already exists",
            "Merge missing Lean quality checks",
        ],
        "config/claudius/skills/setup-secrets-scan/instructions.md": [
            "include exactly one `--config .gitleaks.toml`",
            "ask the user before changing it",
        ],
        "config/claudius/skills/setup-sql-lint/instructions.md": [
            "If `.sqlfluff` already exists",
            "Do not replace the file wholesale",
        ],
        "config/claudius/skills/setup-tftpl-lint/instructions.md": [
            "If the variables file already exists",
            "If the script already exists",
        ],
        "config/claudius/skills/setup-just/instructions.md": [
            "or replacing mandatory settings",
            "before adding the mandatory recipe name as a wrapper",
        ],
        "config/claudius/skills/setup-capybara-lint/instructions.md": [
            "If `.rubocop.yml` exists",
            "ask the user before changing it",
            "preserve the current values",
        ],
        "config/claudius/skills/setup-nix-ci-lint/instructions.md": [
            "check whether equivalent commitlint or PR-title",
            "Do NOT duplicate them",
            "replacing or rewriting those steps",
        ],
        "config/claudius/skills/setup-sorbet/instructions.md": [
            "If `.rubocop.yml` exists",
            "ask the user before changing it",
            "existing plugin order",
        ],
        "config/claudius/skills/setup-ruby-lint/instructions.md": [
            "If `.rubocop.yml` already exists",
            "ask the user before changing",
            "Do NOT duplicate plugin",
            "existing plugin order",
        ],
    }
    for relative_path, needles in expectations.items():
        for needle in needles:
            check_contains(errors, relative_path, needle)
    check_contains_normalized(
        errors,
        "config/claudius/skills/setup-capybara-lint/instructions.md",
        "Do not append duplicate rule blocks",
    )


def main() -> int:
    errors: list[str] = []

    check_ci_skill(errors)
    check_tofu_skill(errors)
    check_commitlint_skill(errors)
    check_tftpl_lint_skill(errors)
    check_biome_skill(errors)
    check_lean_lint_skill(errors)
    check_profile_policy(errors)
    check_pandacss_skill(errors)
    check_file_hygiene_skill(errors)
    check_rubocop_skill(errors)
    check_rails_ci_skill(errors)
    check_rails_testing_skill(errors)
    check_repository_baseline_skill(errors)
    check_i18n_lint_skill(errors)
    check_stylelint_skill(errors)
    check_rails_db_safety_skill(errors)
    check_tofu_project_skill(errors)
    check_c_lint_skill(errors)
    check_just_skill(errors)
    check_just_lint_skill(errors)
    check_google_workspace_iac_skill(errors)
    check_multicloud_iac_structure_skill(errors)
    check_typed_eslint_profile_selection(errors)
    check_skill_catalog_inventory(errors)
    check_setup_skill_safety_policy(errors)
    check_codex_rendered_setup_skill_policy(errors)
    check_git_hooks_skill(errors)
    check_ruby_lint_skill(errors)
    check_command_style_skills(errors)
    check_python_lint_placeholders(errors)
    check_existing_file_merge_policy_details(errors)
    check_shell_template_formatting(errors)

    if errors:
        print("Claudius skill guardrail checks failed:", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 1
    print("Claudius skill guardrail checks passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
