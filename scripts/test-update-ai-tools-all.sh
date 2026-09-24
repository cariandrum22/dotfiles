#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
fixture="$(mktemp -d)"
output_log="$(mktemp)"
trap 'rm -rf "$fixture"; rm -f "$output_log"' EXIT

fixture_git() {
  env -u GIT_DIR -u GIT_INDEX_FILE -u GIT_WORK_TREE git -C "$fixture" "$@"
}

mkdir -p "$fixture/scripts" "$fixture/bin" "$fixture/config/home-manager/home/packages"
cp "$repo_root/scripts/update-ai-tools-all.sh" "$fixture/scripts/update-ai-tools-all.sh"

printf '#!%s\n' "$(command -v bash)" >"$fixture/scripts/build-ai-tools.sh"
cat >>"$fixture/scripts/build-ai-tools.sh" <<'EOF'
if [ "$1" = "codex-cli" ]; then
  exit 1
fi
EOF
chmod +x "$fixture/scripts/build-ai-tools.sh"

printf '#!%s\n' "$(command -v bash)" >"$fixture/bin/python3"
cat >>"$fixture/bin/python3" <<'EOF'
case "$(basename "$1")" in
update-claude-code.py) target="config/home-manager/home/packages/claude-code.nix" ;;
update-codex-cli.py) target="config/home-manager/home/packages/codex.nix" ;;
update-droid.py) target="config/home-manager/home/packages/droid.nix" ;;
update-gemini-cli.py) target="config/home-manager/home/packages/gemini-cli.nix" ;;
*) exit 2 ;;
esac
printf 'updated\n' >>"$target"
EOF
chmod +x "$fixture/bin/python3"

for file in \
  claude-code.nix \
  codex.nix \
  droid.nix \
  gemini-cli.nix \
  rusty-v8-prebuilt-out-dir.patch \
  stub-runfiles.patch; do
  if [ "$file" != "gemini-cli.nix" ]; then
    printf 'baseline\n' >"$fixture/config/home-manager/home/packages/$file"
  fi
done

fixture_git init -q
fixture_git config user.email test@example.com
fixture_git config user.name Test
fixture_git config commit.gpgsign false
fixture_git add .
fixture_git commit -qm baseline

if ! (
  cd "$fixture"
  env -u GIT_DIR -u GIT_INDEX_FILE -u GIT_WORK_TREE \
    PATH="$fixture/bin:$PATH" bash "$fixture/scripts/update-ai-tools-all.sh"
) >"$output_log" 2>&1; then
  cat "$output_log" >&2
  exit 1
fi

for tool in claude-code droid gemini-cli; do
  grep -q '^updated$' "$fixture/config/home-manager/home/packages/$tool.nix"
done

if grep -q '^updated$' "$fixture/config/home-manager/home/packages/codex.nix"; then
  echo "failed Codex update was not rolled back" >&2
  exit 1
fi

grep -q 'codex-cli build failed; its changes were rolled back' "$output_log"
