#!/usr/bin/env bash
set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
config_path="$repo_root/commitlint.config.cjs"
commitlint_bin="${COMMITLINT_BIN:-commitlint}"
zero_sha="0000000000000000000000000000000000000000"

while read -r _local_ref local_sha _remote_ref remote_sha; do
  if [ "$local_sha" = "$zero_sha" ]; then
    continue
  fi

  if [ "$remote_sha" = "$zero_sha" ]; then
    base_ref=""
    if git show-ref --verify --quiet refs/remotes/origin/HEAD; then
      base_ref="refs/remotes/origin/HEAD"
    elif git show-ref --verify --quiet refs/remotes/origin/main; then
      base_ref="refs/remotes/origin/main"
    elif git show-ref --verify --quiet refs/remotes/origin/master; then
      base_ref="refs/remotes/origin/master"
    fi

    if [ -n "$base_ref" ]; then
      base_sha="$(git merge-base "$local_sha" "$base_ref" || true)"
    else
      base_sha=""
    fi

    if [ -n "$base_sha" ] && [ "$base_sha" != "$local_sha" ]; then
      "$commitlint_bin" --config "$config_path" --from "$base_sha" --to "$local_sha"
    else
      tmp_msg="$(mktemp)"
      git log -1 --format=%B "$local_sha" > "$tmp_msg"
      "$commitlint_bin" --config "$config_path" --edit "$tmp_msg"
      rm -f "$tmp_msg"
    fi
  else
    "$commitlint_bin" --config "$config_path" --from "$remote_sha" --to "$local_sha"
  fi
done
