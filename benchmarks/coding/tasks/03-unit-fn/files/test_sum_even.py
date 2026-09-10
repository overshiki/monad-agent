import sys
from sum_even import sum_even

cases = [(1, 10, 30), (2, 5, 6), (3, 3, 0), (-4, 4, 0), (1, 1, 0)]
for a, b, expected in cases:
    got = sum_even(a, b)
    if got != expected:
        print(f"sum_even({a}, {b}) = {got}, expected {expected}")
        sys.exit(1)
print("ok")
