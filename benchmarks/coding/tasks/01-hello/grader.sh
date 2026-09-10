#!/usr/bin/env bash
set -euo pipefail
[ -f hello.py ] || { echo "hello.py missing"; exit 1; }
out="$(python3 hello.py)"
[ "$out" = "hello, benchmark" ] || { echo "expected 'hello, benchmark', got '$out'"; exit 1; }
echo "ok"
