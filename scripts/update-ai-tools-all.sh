#!/usr/bin/env bash
# Update and verify AI tools independently.

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

readonly PACKAGES_DIR="config/home-manager/home/packages"
readonly SNAPSHOT_ROOT="$(mktemp -d)"
declare -a failed_tools=()

cleanup() {
  rm -rf "$SNAPSHOT_ROOT"
}
trap cleanup EXIT

managed_paths() {
  case "$1" in
  claude-code)
    printf '%s\n' "$PACKAGES_DIR/claude-code.nix"
    ;;
  codex-cli)
    printf '%s\n' \
      "$PACKAGES_DIR/codex.nix" \
      "$PACKAGES_DIR/rusty-v8-prebuilt-out-dir.patch" \
      "$PACKAGES_DIR/stub-runfiles.patch"
    ;;
  droid)
    printf '%s\n' "$PACKAGES_DIR/droid.nix"
    ;;
  gemini-cli)
    printf '%s\n' "$PACKAGES_DIR/gemini-cli.nix"
    ;;
  *)
    printf 'Unknown AI tool: %s\n' "$1" >&2
    return 2
    ;;
  esac
}

update_script() {
  case "$1" in
  claude-code) printf '%s\n' scripts/update-claude-code.py ;;
  codex-cli) printf '%s\n' scripts/update-codex-cli.py ;;
  droid) printf '%s\n' scripts/update-droid.py ;;
  gemini-cli) printf '%s\n' scripts/update-gemini-cli.py ;;
  *)
    printf 'Unknown AI tool: %s\n' "$1" >&2
    return 2
    ;;
  esac
}

snapshot_tool() {
  local tool="$1"
  local snapshot_dir="$SNAPSHOT_ROOT/$tool"
  local path

  mkdir -p "$snapshot_dir/present"
  : >"$snapshot_dir/absent"

  while IFS= read -r path; do
    if [ -e "$path" ]; then
      mkdir -p "$snapshot_dir/present/$(dirname "$path")"
      cp -p "$path" "$snapshot_dir/present/$path"
    else
      printf '%s\n' "$path" >>"$snapshot_dir/absent"
    fi
  done < <(managed_paths "$tool")
}

restore_tool() {
  local tool="$1"
  local snapshot_dir="$SNAPSHOT_ROOT/$tool"
  local path

  while IFS= read -r path; do
    mkdir -p "$(dirname "$path")"
    cp -p "$snapshot_dir/present/$path" "$path"
  done < <(cd "$snapshot_dir/present" && find . -type f -print | sed 's#^./##')

  while IFS= read -r path; do
    [ -n "$path" ] && rm -f "$path"
  done <"$snapshot_dir/absent"
}

tool_has_changes() {
  local tool="$1"
  local path

  while IFS= read -r path; do
    if [ -n "$(git status --porcelain --untracked-files=normal -- "$path")" ]; then
      return 0
    fi
  done < <(managed_paths "$tool")
  return 1
}

record_failure() {
  local tool="$1"
  local phase="$2"

  failed_tools+=("$tool ($phase)")
  restore_tool "$tool"
  printf 'WARNING: %s %s failed; its changes were rolled back.\n' "$tool" "$phase" >&2
  if [ "${GITHUB_ACTIONS:-}" = "true" ]; then
    printf '::warning title=AI tool update skipped::%s %s failed; other tool updates will continue.\n' \
      "$tool" "$phase"
  fi
}

update_tool() {
  local tool="$1"
  local updater
  updater="$(update_script "$tool")"

  printf 'Updating %s...\n' "$tool"
  snapshot_tool "$tool"

  if ! python3 "$updater"; then
    record_failure "$tool" "update"
    return 0
  fi

  if ! tool_has_changes "$tool"; then
    printf '%s is already current; build verification skipped.\n\n' "$tool"
    return 0
  fi

  if ! ./scripts/build-ai-tools.sh "$tool"; then
    record_failure "$tool" "build"
    return 0
  fi

  printf '%s update accepted.\n\n' "$tool"
}

if [ "$#" -eq 0 ]; then
  set -- claude-code codex-cli droid gemini-cli
fi

for tool in "$@"; do
  # Validate tool names before checking the working tree.
  managed_paths "$tool" >/dev/null
done

if [ -n "$(git status --porcelain --untracked-files=normal)" ]; then
  echo "AI tool updates require a clean working tree." >&2
  exit 2
fi

printf 'Updating AI tools independently...\n\n'
for tool in "$@"; do
  update_tool "$tool"
done

if [ "${#failed_tools[@]}" -gt 0 ]; then
  printf 'Completed with skipped updates: %s\n' "${failed_tools[*]}" >&2
  if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
    {
      printf '### Skipped AI tool updates\n\n'
      printf '%s\n' "${failed_tools[@]/#/- }"
    } >>"$GITHUB_STEP_SUMMARY"
  fi
else
  echo "All requested AI tool updates succeeded."
fi
