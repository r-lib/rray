# Integer add helper performance

## Summary

While adding `rray_cumulative_sum()`, rewriting the overflow check in
`rray_add_int_one()` (`src/one-add.h`) made integer sums about 2x faster when
the values have mixed signs. This helps both `rray_cumulative_sum()` and
`rray_add()`. The change was tried and undone on the `feature/cumulative-sum`
branch. It belongs in its own PR, which should also check
`rray_subtract_int_one()`.

## The change

Before:

```c
if ((y > 0 && x > INT_MAX - y) || (y < 0 && x < -INT_MAX - y)) {
  stop_int_overflow(error_call);
}

return x + y;
```

After:

```c
const int64_t out = (int64_t) x + y;

if (out > INT_MAX || out < -INT_MAX) {
  stop_int_overflow(error_call);
}

return (int) out;
```

Add `#include <stdint.h>` to `src/one-add.h`. The missing value check at the
top of the helper stays as is.

## Why it is faster

The old check branches on the sign of `y`. When `y` is a random mix of
negative, zero, and positive values, the CPU can't predict that branch, and
every wrong guess costs a pipeline flush.

The new check does not depend on the sign of `y`. Its only branch is "did it
overflow", which is almost never taken and so is always predicted right.

The old form is fine when every value has the same sign, which is why
logical input (only `0` and `1`) was already fast.

## Measurements

Apple Silicon (arm64), R's default compiler flags through
`devtools::load_all()`, median of 15 iterations, 1.6e7 elements.

| Call | Data | Before | After |
|---|---|---|---|
| `rray_cumulative_sum(x, 1L)` | `sample(-1:1)` | 52ms | 24ms |
| `rray_cumulative_sum(x, 1L)` | `sample(0:1)` | 26ms | 24ms |
| `rray_cumulative_sum(x, 1L)` | `sample(c(TRUE, FALSE))` | 27ms | 23ms |
| `rray_cumulative_sum(m, 2L)`, 4000 x 4000 | `sample(-1:1)` | 57ms | 23ms |
| `rray_add(x, y)` | both `sample(-1:1)` | 62ms | 23ms |

For reference, base `cumsum()` took 33ms and base `x + y` took 48ms on the
same mixed sign data.

The inputs were:

```r
set.seed(1)
n <- 16e6
xs <- sample(-1:1, n, TRUE)
xp <- sample(0:1, n, TRUE)
xs2 <- sample(-1:1, n, TRUE)
xl2 <- sample(c(TRUE, FALSE), n, TRUE)
m <- matrix(xs, 4000)
```

## Behavior

None. Both forms error on exactly the same inputs: a result above `INT_MAX`
or below `-INT_MAX`. `INT_MIN` is the integer `NA`, so it stays out of range
in both.

## Helpers to investigate

- `rray_add_int_one()`: the change above.

- `rray_subtract_int_one()` in `src/arithmetic-subtract.c`: it has the same
  sign branch, `(y < 0 && x > INT_MAX + y) || (y > 0 && x < -INT_MAX + y)`.
  Try `(int64_t) x - y` with the same range check. Expect the same win on
  mixed sign data.

- `int_add_checked()` in `src/utils.c`: same pattern, but it is not on a hot
  path. Leave it unless it shows up in a benchmark.

- `rray_multiply_int_one()` already uses a range check on a wider type, so
  there is nothing to change.

## Benchmarks for the new PR

Measure before and after, on mixed sign and same sign integer data:

- `rray_cumulative_sum()` for logical and integer.

- `rray_add()` and `rray_subtract()` for integer, with both inputs full size
  and with one input broadcast.

- Axis 1 of a long vector, axis 1 and axis 2 of a 4000 x 4000 matrix.

`bench/cumulative.R` and `bench/binary.R` cover most of these, but their
integer data is `sample(100L)`, which is all positive. Add mixed sign data,
otherwise the benchmarks will not show the change. Results on x86 would also
help.

## Tests for the new PR

- Existing overflow tests for `rray_add()`, `rray_subtract()`, and
  `rray_cumulative_sum()` already cover both bounds. Check that sums that
  land exactly on `INT_MAX` and `-INT_MAX` still succeed.

- The full suite.
