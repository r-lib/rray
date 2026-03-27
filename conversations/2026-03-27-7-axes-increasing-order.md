# Axes increasing order enforcement

**Running cost: ~$0.15**

## Prompt

> arg_as_axes should enforce that axes are in increasing order, this should also simplify the duplication check, i think

## Changes

Replaced the O(n^2) duplicate-detection inner loop in `arg_as_axes()` with a single check that each axis is strictly greater than the previous one. This enforces increasing order and implicitly prevents duplicates.

### Files changed

- `src/axes.c` — replaced inner duplicate loop with `axis <= v_axes[i - 1]` check
- `tests/testthat/test-sum.R` — renamed test, added decreasing-order case
- `tests/testthat/test-split.R` — added decreasing-order case
- `tests/testthat/_snaps/sum.md` — updated snapshot
- `tests/testthat/_snaps/split.md` — updated snapshot

All 113 tests pass.
