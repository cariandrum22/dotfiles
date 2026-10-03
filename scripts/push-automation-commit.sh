#!/usr/bin/env bash

set -euo pipefail

: "${APP_TOKEN:?APP_TOKEN is required}"
: "${GITHUB_REPOSITORY:?GITHUB_REPOSITORY is required}"
: "${HEAD_REF:?HEAD_REF is required}"

git check-ref-format "refs/heads/${HEAD_REF}"
temporary_directory="$(mktemp -d "$HOME/.automation-push.XXXXXX")"
trap 'rm -rf "$temporary_directory"' EXIT

printf '#!%s\n' "$(command -v bash)" >"$temporary_directory/askpass"
cat >>"$temporary_directory/askpass" <<'EOF'
case "$1" in
  *Username*) printf '%s\n' 'x-access-token' ;;
  *Password*) printf '%s\n' "$APP_TOKEN" ;;
esac
EOF
chmod 700 "$temporary_directory/askpass"

export GIT_ASKPASS="$temporary_directory/askpass"
export GIT_TERMINAL_PROMPT=0

# Retry HTTP failures, including transient authentication errors, with backoff.
# Never force-push or hide a persistent denial.
for attempt in 1 2 3 4; do
  if git -c credential.helper= -c http.https://github.com/.extraheader= \
    push "https://github.com/${GITHUB_REPOSITORY}.git" "HEAD:refs/heads/${HEAD_REF}" \
    >"$temporary_directory/output" 2>&1; then
    cat "$temporary_directory/output"
    exit 0
  fi
  cat "$temporary_directory/output" >&2
  if [ "$attempt" -eq 4 ] ||
    ! grep -Eq 'returned error: (401|403|429|500|502|503|504)([^0-9]|$)' "$temporary_directory/output"; then
    echo "Automation push failed; check App permissions and branch state." >&2
    exit 1
  fi
  delay=$((attempt * 5))
  echo "GitHub HTTP error; retrying push in ${delay}s (${attempt}/4)." >&2
  sleep "$delay"
done
