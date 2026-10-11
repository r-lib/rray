# Product helper performance

## Summary

While adding `rray_cumulative_prod()`, a one-line rewrite of
`rray_prod_lgl_one()` and `rray_prod_int_one()` in `src/one-multiply.h` made
logical and integer products 25% to 35% faster along a contiguous axis. This
helped both `rray_cumulative_prod()` and `rray_prod()`. That change was made
and then undone on the `feature/cumulative-prod` branch (commit `32aabcc`,
reverted after it). It belongs in its own PR. That PR should check every
helper in `src/one-multiply.h` at once.

## The change

Before:

```c
static inline double rray_prod_int_one(double out, int x) {
  if (rray_int_is_missing(x)) {
    return r_globals.na_dbl;
  }

  return out * x;
}
```

After:

```c
static inline double rray_prod_int_one(double out, int x) {
  return out * (rray_int_is_missing(x) ? r_globals.na_dbl : (double) x);
}
```

`rray_prod_lgl_one()` got the same rewrite.

## Why it is faster

Along axis 1, both `RRAY_REDUCE()` and `RRAY_CUMULATIVE()` take the fixed zero
stride path. Every step feeds `out` into the next one:

```c
for (r_ssize i = start; i < end; ++i) {
  elt = ONE(elt, v_x[i]);
  v_out[i] = elt;
}
```

This is a chain. Each step must wait for the one before it to finish, so the
loop runs at the speed of one step's latency, not its throughput.

- Before: the select (`missing ? NA : out * x`) comes after the multiply, and
  it uses the multiply's result. Each step in the chain costs a multiply plus
  a select.

- After: the select only depends on `x`, which is known ahead of time. The CPU
  can work it out early, outside the chain. Each step in the chain costs a
  single multiply.

Doubles never had this problem. `rray_prod_dbl_one()` is a bare multiply, and
that is why double was already faster than integer and logical before the
change.

Axes other than 1 do not benefit. There, the scan advances many independent
lanes at once, so there is no single chain. Those cases are limited by memory.
`rray_cumulative_prod()` over axis 2 of a 4000 x 4000 double took 16ms, against
12.5ms for `rray_multiply(x, 1)` on the same data.

## Measurements

Apple Silicon (arm64), R's default compiler flags through
`devtools::load_all()`, median of 10 iterations, a vector of 1.6e7 elements
reduced or scanned along axis 1.

| Call | Type | Before | After |
|---|---|---|---|
| `rray_cumulative_prod()` | integer | 33ms | 25ms |
| `rray_cumulative_prod()` | logical | 33ms | 25ms |
| `rray_prod()` | integer | 27ms | 18ms |
| `rray_prod()` | logical | 28ms | 18ms |

Doubles do not use the changed helpers. For reference, `rray_cumulative_prod()`
took 24ms to 25ms on doubles and `rray_prod()` took 18ms. Base `cumprod()` took
40ms on integers and 25ms on doubles. After the change, integer and logical
match double. For comparison,
`rray_cumulative_max()` on the same integer data took 12ms, because a
comparison has a shorter latency than a multiply.

The inputs were:

```r
set.seed(1)
xi <- sample(100L, 16e6, TRUE)
xi[sample(16e6, 10)] <- NA
xl <- sample(c(TRUE, FALSE, NA), 16e6, TRUE)
x <- runif(16e6, 0.5, 1.5)
```

This integer data overflows to `Inf` quickly, and the logical data becomes
missing quickly. To rule out special values as the cause, the new form was
also run on integers drawn from `-1L`, `1L`, and `2L`, which stay finite. It
took 23.5ms, in line with the data above. The old form was not run on that
data, so measure it in the new PR.

## Behavior

For logical input, results cannot change. A product of `0`, `1`, and `NA`
values can never be `Inf` or `NaN`.

For integer input, results can change in one case: the running product is
already `NaN` and the next value is `NA`. This needs an overflow to `Inf`
followed by a `0`:

```r
x <- c(rep(.Machine$integer.max, 40L), 0L, NA)
rray_prod(x, 1L)
```

Before, the result is always `NA`, because the helper returns `NA` without
multiplying. After, it is `NaN * NA`, and which one wins depends on the
platform and the compiler's operand order. On arm64 it still gave `NA`. The
`rray_prod()` docs already say that the winner between `NA` and `NaN` is not
promised for doubles. The new PR should say the same holds for logical and
integer input, and test it with `is.na()` only.

## Helpers to investigate

Check every helper in `src/one-multiply.h` and change all that benefit in one
PR.

- `rray_prod_lgl_one()` and `rray_prod_int_one()`: the change above. Expect
  the same results as the table.

- `rray_prod_lgl_one_na_rm()` and `rray_prod_int_one_na_rm()`: these return
  `out` unchanged for a missing value, so the select also sits on the chain.
  Try `out * (missing ? 1.0 : (double) x)`. Multiplying by `1.0` returns `out`
  exactly, including `-0`, `Inf`, `NA`, and `NaN`, so results should not
  change. Confirm that with tests.

- `rray_prod_dbl_one_na_rm()`: same idea, `out * (missing ? 1.0 : x)`. Same
  expected outcome.

- `rray_prod_cpl_one_na_rm()`: the same trick is not safe. Under C99 rules,
  multiplying by `1+0i` is not exact. `(a + bi) * (1 + 0i)` computes `b * 0`
  and `a * 0`, which gives `NaN` for an infinite part and can lose the sign of
  a `-0` part. If this helper is worth speeding up, it needs a different form,
  such as selecting the whole result after the multiply on both parts. That
  puts the select back on the chain, so measure before keeping anything.

- `rray_prod_dbl_one()`, `rray_prod_cpl_one()`, `rray_multiply_dbl_one()`, and
  `rray_multiply_cpl_one()`: no branch, nothing to change. Leave them, but keep
  them in the benchmark as the target that the other types should reach.

- `rray_multiply_int_one()`: an elementwise helper, so it is not in a chain.
  The rewrite does not apply. Its overflow error may still block
  vectorization. Measure it, but do not expect a win from this change.

## Benchmarks for the new PR

Measure before and after for each changed helper:

- `rray_prod()` with `na_rm = FALSE` and `na_rm = TRUE`, for logical, integer,
  and double.

- `rray_cumulative_prod()` for logical and integer.

- Axis 1 of a long vector, axis 1 and axis 2 of a 4000 x 4000 matrix, and
  axis 1 of a 10 x 1e6 matrix. Only axis 1 should move. The others are a check
  that nothing got slower.

`bench/reduce.R` and `bench/cumulative.R` cover most of these. Results on x86
would also help, since its select and multiply latencies differ from arm64.

## Tests for the new PR

- `rray_prod()` and `rray_cumulative_prod()` on integer input with `NaN`
  followed by `NA`: check with `is.na()` only.

- The `na_rm` helpers keep `out` exactly for missing values: a running `-0`
  stays `-0` (check with `1 / x`), and `Inf`, `NA`, and `NaN` are unchanged.

- The full suite, since `rray_prod()` uses the same helpers.
