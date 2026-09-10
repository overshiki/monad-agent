#!/usr/bin/env bash
set -euo pipefail
[ -f sum_even.py ] || { echo "sum_even.py missing"; exit 1; }
python3 test_sum_even.py
