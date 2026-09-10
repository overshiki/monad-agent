#!/usr/bin/env bash
set -euo pipefail
[ -f message.txt ] || { echo "message.txt missing"; exit 1; }
expected='The quick brown fox jumps over the lazy dog.'
got="$(cat message.txt)"
[ "$got" = "$expected" ] || { echo "still has a typo"; exit 1; }
echo "ok"
