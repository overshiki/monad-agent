#!/usr/bin/env bash
set -euo pipefail
[ -f cli.py ] || { echo "cli.py missing"; exit 1; }
[ "$(python3 cli.py --name ada)" = "hello, ada" ] || { echo "base case failed"; exit 1; }
[ "$(python3 cli.py --name ada --shout)" = "HELLO, ADA" ] || { echo "--shout flag missing or wrong"; exit 1; }
echo "ok"
