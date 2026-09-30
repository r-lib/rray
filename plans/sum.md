# Vectorizing `rray_sum()` for integer and logical input

Integer and logical `rray_sum()` ran one element at a time, because the overflow check could abort in the middle of the loop. This document records what we measured, what is done on the `feature/sum-vectorize` branch, the designs we rejected, and what is left.

---

# Status

- Branch: `feature/sum-vectorize`, created from `main` at `d45c73b`.

- The work is uncommitted in the working tree. Nothing has been committed or pushed.

- All 3352 tests pass. `clang-format` and `air format` have been run.

- Done: summing along axis 2 (the `rowSums()` shape) vectorizes, and is about 2.8x faster.

- Not done: the full sum and summing along axis 1 (the `colSums()` shape) are only about 10% faster. See "Next step".

---

# Benchmarks

10 million integers from `sample(100L, 1e7, TRUE)`, Apple clang 17, `-O2`, Apple silicon.

| Case | `main` | Branch | Base R | matrixStats |
|---|---|---|---|---|
| Full sum, `array(v, c(1e7, 1))` over axis 1 | 8.47 ms | 7.61 ms | 5.63 ms | 8.45 ms |
| 1e4 x 1e3 matrix, sum along axis 1 | 8.54 ms | 7.61 ms | 8.42 ms | 8.46 ms |
| 1e4 x 1e3 matrix, sum along axis 2 | 8.50 ms | 3.06 ms | 5.96 ms | 8.56 ms |

`rray_prod()`, `rray_all()` and the double sum run at the same speed as on `main`.

The benchmark script:

```r
devtools::load_all(quiet = TRUE)
set.seed(1)
v <- sample(100L, 1e7, TRUE)
x <- array(v, c(1e7, 1))
m <- matrix(v, 1e4, 1e3)
bench::mark(
  rray = rray_sum(x, 1),
  base = sum(v),
  ms = matrixStats::sum2(v),
  check = FALSE,
  min_iterations = 20
)
bench::mark(
  rray_cols = rray_sum(m, 1),
  base_cols = colSums(m),
  ms_cols = matrixStats::colSums2(m),
  rray_rows = rray_sum(m, 2),
  base_rows = rowSums(m),
  ms_rows = matrixStats::rowSums2(m),
  check = FALSE,
  min_iterations = 20
)
```

To see what clang vectorizes:

```sh
cd src
clang -O2 -I$(R RHOME)/include -I./rlang -c reduce-sum.c -o /dev/null \
  -Rpass=loop-vectorize -Rpass-missed=loop-vectorize -Rpass-analysis=loop-vectorize
```

---

# Why `main` doesn't vectorize

On `main`, clang rejects the int sum loops with "could not determine number of loop iterations".

- `check_sum_int_overflow()` called `r_abort()` inside the loop. A call that never returns is an exit from the middle of the loop, so clang can't know how many times the loop runs.

- The NA and overflow checks were branches on the running total at every step.

Base R's `isum()` in `src/main/summary.c` is also scalar. It is faster than `main` because it adds into a 64-bit `LONG_INT` and only checks for overflow now and then, not every step.

---

# What the branch does

## `src/reduce.h`

`RRAY_REDUCE()` gains a `ONE_ARGS` parameter, copying the pattern in `src/binary.h`:

```c
#define RRAY_REDUCE_ARGS(...) , __VA_ARGS__
#define RRAY_REDUCE_NO_ARGS
```

The kernel is called as `ONE(out, v_x[i] ONE_ARGS)`. This is portable C99 and doesn't warn under `-pedantic`, because `...` is never empty. Only the `ONE_ARGS` argument expands to nothing, and C99 allows empty macro arguments.

When `out_run_stride == 0` (one output per run), the loop now keeps the total in a local `out_elt` and writes it to `v_out` once per run. Without it, the full sum took 11.4 ms. clang can't keep the total in a register by itself because `v_x` and `v_out` are both `int*` and might overlap.

That is the whole diff to `reduce.h`. All `RRAY_REDUCE()` callers in `reduce-prod.c`, `reduce-logical.c` and the double and complex sums pass `RRAY_REDUCE_NO_ARGS`.

## `src/reduce-sum.c`

Logical and integer sums share two kernels. The logical kernels are gone.

```c
static inline int rray_sum_int_one(int out, int x, bool* error) {
  if (out == r_globals.na_int) {
    return r_globals.na_int;
  }

  if (x == r_globals.na_int) {
    return r_globals.na_int;
  }

  const int64_t sum = (int64_t) out + x;
  *error |= (sum > INT_MAX) | (sum < -INT_MAX);

  return (int) sum;
}
```

`rray_sum_int_one_na_rm()` is the same, but returns `out` when `x` is NA.

Each checked reducer wraps an `_impl` that runs the macro. The caller checks the flag afterwards, because the macro ends with `return out;`:

```c
static r_obj* rray_sum_int(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan
) {
  bool error = false;
  r_obj* out = rray_sum_int_impl(x, out_size, plan, &error);

  if (error) {
    stop_sum_int_overflow();
  }

  return out;
}
```

There are four of these: `rray_sum_lgl()`, `rray_sum_lgl_na_rm()`, `rray_sum_int()` and `rray_sum_int_na_rm()`. `stop_sum_int_overflow()` calls `r_abort("Integer overflow.")`, so the error text and snapshots are unchanged.

Checking once at the end gives the same results as checking every step. The flag only goes from `false` to `true`. After an overflow, the loop keeps adding garbage, but that's harmless: the add is 64-bit, and narrowing an out-of-range value to `int` is implementation-defined, not undefined. We abort at the end anyway.

The vectorizer report shows all four `_impl` axis 2 loops at width 16. Writing the flag through `bool*` doesn't block vectorization.

## Behavior

Results and errors are the same as on `main`. The total is still checked at every step, in the same order.

- An NA before an overflow gives NA: `c(NA, INT_MAX, 1L)` returns `NA`.

- An overflow before an NA errors: `c(INT_MAX, 1L, NA)` errors.

It also fixes a bug on `main`. `rray_sum_lgl_one()` never checked for overflow, based on a comment saying arrays can't be long vectors. They can: `dimgets()` in R's `src/main/attrib.c` computes the total length as `R_xlen_t`, and rray4 bounds sizes by `R_SSIZE_MAX` in `rray_size_from_dimensions_checked()`. An output receiving more than `INT_MAX` `TRUE` values overflowed `int`, which is undefined behavior. Logical sums are now checked like integer sums.

## Tests added to `tests/testthat/test-reduce-sum.R`

- Integer and logical NA along axis 2.

- NA before a later overflow gives NA, along axis 1 and axis 2.

- `na_rm` along axis 2, for integer and logical.

- Overflow and underflow along axis 2, with and without `na_rm`.

- Overflow before a later NA errors.

- Sums that land exactly on `INT_MAX` and `-INT_MAX`.

The existing overflow tests only covered axis 1, which runs through the loop that doesn't vectorize.

---

# Designs we rejected, and why

## A one-shot 64-bit buffer with the checks moved out of the loop

This means adding into an `int64_t` scratch buffer of `out_size` elements, tracking NA with a flag that never branches, and checking overflow once per output at the end. Axis 2 ran in 1.14 ms, the fastest option we found.

Rejected because it allocates a buffer that can be large.

It also relied on a wrong premise, that arrays can't be long vectors. With long arrays, a 64-bit total can overflow once one output gets 2^32 or more elements.

## The nested reducer (`RRAY_REDUCE_OUTER()` / `RRAY_REDUCE_INNER()`)

The total lives in a local `int64_t`, like `src/reduce-mean.c`. Prototyped:

| Case | Flat on `main` | Nested |
|---|---|---|
| Full sum | 8.46 ms | 0.73 ms |
| Sum along axis 1 | 8.55 ms | 0.74 ms |
| Sum along axis 2 | 8.50 ms | 7.33 ms |

It is fast only when axis 1 is summed. When axis 1 is kept, the inner loop jumps `nrow` elements between reads, so it can't vectorize and misses the cache.

It also checks overflow only on the final total, so `c(INT_MAX, 1L, -1L)` would return `INT_MAX` instead of erroring, like base R. That changes behavior, and it was never decided.

## Kernels with no branches at all

```c
const bool na = (out == NA) | (x == NA);
const int64_t s = (int64_t) out + x;
*error |= !na & ((s > INT_MAX) | (s < -INT_MAX));
return na ? NA : (int) s;
```

This vectorizes along axis 2 just as well, but made the full sum slower: 13.7 ms against 8.5 ms on `main`. The NA compare and select sit on the path from one element to the next. With plain `if` statements, clang still turns them into selects in the vectorized axis 2 loop, while the axis 1 loop gets branches the CPU predicts well.

## A 32-bit check using sign bits

Add as `unsigned`, then flag overflow when `((o ^ s) & (e ^ s)) < 0` or `s == INT_MIN`. It was only 6% faster than the 64-bit check (2.88 ms against 3.05 ms), and harder to read.

## A separate `RRAY_REDUCE_CHECKED()` macro

The first version on the branch added `RRAY_REDUCE_CHECKED()`, with both macros expanding to a hidden `RRAY_REDUCE_LOOP()` that ran a per-run `if (error) STOP();`. It was replaced by `ONE_ARGS` plus a check in the caller, which gives one macro and matches `binary.h`.

## Using `##__VA_ARGS__` or `__VA_OPT__` for optional kernel arguments

Both warn under `-pedantic` in C99, C11 and C17. The `binary.h` pattern avoids that.

---

# Next step: vectorize summing along axis 1

This is what's left for the case originally reported. The full sum is still 7.6 ms, against 5.6 ms for base R.

## Why the axis 1 loop can't vectorize now

The report says "value that could not be identified as reduction is used outside the loop", meaning `out_elt`. Vectorizing one total means splitting it into partial totals and adding them at the end. That changes which intermediate totals exist, and we check every one:

- One at a time, `c(INT_MAX, 1L, -1L)` overflows at step 2 and errors.

- Split in two, the partial totals are `INT_MAX - 1` and `1`, neither overflows, and the result is `INT_MAX`.

So as long as every step is checked, this loop has to run one element at a time.

## Proposed fix: a bound on the absolute values

This is proposed but not prototyped. For each run where `out_run_stride == 0`, compute these with plain 64-bit adds that clang can reorder:

```c
sum += x_elt;
abs_sum += x_elt < 0 ? -(int64_t) x_elt : x_elt;
any_na |= x_elt == r_globals.na_int;
```

- If `|starting total| + abs_sum <= INT_MAX`, no intermediate total could have left the `int` range in any order. The result is then exactly what the step-by-step loop gives: NA if `any_na` is set, otherwise `sum`.

- If the bound fails, rerun that run with the current step-by-step loop. That keeps today's behavior for overflow before NA and NA before overflow.

- The 64-bit sums can't overflow. A run with `out_run_stride == 0` covers at most one axis of the array, so fewer than 2^31 elements, each at most 2^31 in absolute value. This needs checking against how the iterator merges axes into runs. If merged runs can exceed 2^31 elements, split them into chunks.

- For `na_rm`, both sums skip NA elements, and there's no `any_na`.

- A starting total that is already NA stays NA.

- Handle `INT_MIN` carefully: it is NA, so take it out of `abs_sum` or mask it before negating.

The standalone prototype without overflow checks ran in 0.73 ms. With the second sum, expect about 1 ms. This is not measured.

`RRAY_REDUCE()` has no way to do this in its `out_run_stride == 0` branch today. It needs a design choice: an optional per-run hook, or a separate fast path only for int and logical sums.

---

# Other notes

- The double sum along axis 2 (`out_run_stride == 1`) could vectorize if the `ISNAN()` branches in `rray_sum_dbl_one()` became a plain `out + x`, with NA-over-NaN precedence fixed in a rescan when a result is NaN. The full double sum can't vectorize at `-O2`, because reordering double adds changes the result. Base R's double sum is scalar for the same reason.

- `rray_mean()` adds into a `long double`. That is plain `double` on Apple silicon, so its `na_rm` loops vectorize there. On x86, `long double` is 80 bits, and those loops won't vectorize.

- All results here are from Apple silicon with Apple clang 17. They have not been checked on x86 or with gcc.

- Before any C change is done, the repo rules require a separate pass over every new or touched `r_obj*`. For the branch: `out` comes back from `_impl` after the macro's `FREE(1)`. If the flag is set, `stop_sum_int_overflow()` aborts and `out` is never used again. Otherwise `out` is returned with nothing allocated in between, and `rray_reduce()` protects it with `KEEP()` straight away.
