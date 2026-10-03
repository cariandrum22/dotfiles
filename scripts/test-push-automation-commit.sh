#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
fixture="$(mktemp -d "$HOME/.automation-push-test.XXXXXX")"
trap 'rm -rf "$fixture"' EXIT
mkdir -p "$fixture/bin"

printf '#!%s\n' "$(command -v bash)" >"$fixture/bin/git"
cat >>"$fixture/bin/git" <<'EOF'
set -euo pipefail
if [ "$1" = check-ref-format ]; then
  exit 0
fi

# The push must use the supplied token, without inherited credentials or force.
[ "$*" = '-c credential.helper= -c http.https://github.com/.extraheader= push https://github.com/example/repository.git HEAD:refs/heads/dependabot/example' ]
[ "$GIT_TERMINAL_PROMPT" = 0 ]
[ "$("$GIT_ASKPASS" Username)" = x-access-token ]
[ "$("$GIT_ASKPASS" Password)" = "$APP_TOKEN" ]
printf '%s\n' "$GIT_ASKPASS" >>"$FIXTURE/attempts"
attempt="$(wc -l <"$FIXTURE/attempts")"
if [ "$attempt" -le "$FAILURES" ]; then
  printf '%s\n' "$FAILURE_MESSAGE" >&2
  exit 128
fi
echo 'Push succeeded'
EOF

printf '#!%s\n' "$(command -v bash)" >"$fixture/bin/sleep"
cat >>"$fixture/bin/sleep" <<'EOF'
printf '%s\n' "$1" >>"$FIXTURE/delays"
EOF
chmod +x "$fixture/bin/git" "$fixture/bin/sleep"

run_case() {
  local failures="$1" message="$2" expected_status="$3" expected_attempts="$4"
  local status=0 askpass
  : >"$fixture/attempts"
  : >"$fixture/delays"
  PATH="$fixture/bin:$PATH" FIXTURE="$fixture" \
    APP_TOKEN=test-token GITHUB_REPOSITORY=example/repository HEAD_REF=dependabot/example \
    FAILURES="$failures" FAILURE_MESSAGE="$message" \
    bash "$repo_root/scripts/push-automation-commit.sh" >"$fixture/output" 2>&1 || status=$?
  if [ "$status" -ne "$expected_status" ] ||
    [ "$(wc -l <"$fixture/attempts")" -ne "$expected_attempts" ]; then
    cat "$fixture/output" >&2
    echo "Unexpected push outcome for: $message" >&2
    exit 1
  fi
  while IFS= read -r askpass; do
    [ ! -e "$askpass" ]
  done <"$fixture/attempts"
  if grep -q test-token "$fixture/output"; then
    echo 'Token leaked into push output' >&2
    exit 1
  fi
}

run_case 0 unused 0 1
[ ! -s "$fixture/delays" ]

for code in 401 403 429 500 502 503 504; do
  run_case 2 "fatal: unable to access repository: The requested URL returned error: $code" 0 3
  [ "$(cat "$fixture/delays")" = $'5\n10' ]
done

run_case 10 'fatal: The requested URL returned error: 403' 1 4
[ "$(cat "$fixture/delays")" = $'5\n10\n15' ]

run_case 1 '! [rejected] HEAD -> dependabot/example (non-fast-forward)' 1 1
[ ! -s "$fixture/delays" ]

run_case 1 'remote: error: GH013: Repository rule violations found' 1 1
[ ! -s "$fixture/delays" ]

run_case 1 'fatal: The requested URL returned error: 404' 1 1
[ ! -s "$fixture/delays" ]

echo 'Automation push regression tests passed'
