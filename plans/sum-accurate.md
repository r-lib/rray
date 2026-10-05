# Accurate and fast `rray_sum()`

This plan moves every `rray_sum()` variant off `RRAY_REDUCE()` and onto hand written run iterator loops, the way `rray_mean()` works since #124. Integer and logical sums add into an `int64_t` buffer and only check the final total. That makes integer sums 4 to 9x faster, and changes two overflow rules to match base R. Double and complex sums keep their exact results and get a few fixes for speed.

All of it lands as one pull request on `feature/accurate-fast-sum`. The numbers and the `src/reduce-sum.c` code below come from a prototype that passed the full sum test file.

---

# Decisions

- Hand written loops, like `src/reduce-mean.c`. No new macro. The `RRAY_REDUCE_ACC()` macro from `plans/accuracy.md` is dropped. `RRAY_REDUCE()` stays for product, extremum and logical reducers.

- Integer and logical sums share one loop, `rray_sum_lgl_or_int()`. It adds into an `int64_t` per output and errors only when the final total doesn't fit in an `int`.

- `NA` is tracked with a `bool` per output, set during the summing pass. There is no second pass.

- An integer output with more than 2^32 elements goes to `rray_sum_int_fallback()`, which keeps an exact 128-bit total.

- Double and complex sums get their own loops. The order of the adds doesn't change, so results are identical to `main`. Pairwise summation is a later pull request.

- `one-add.h` loses its whole Reduce section. Your comment on `NA` against `NaN` moves, word for word, to `rray_add_dbl_one()`. `one-multiply.h` points there instead of at `rray_sum_dbl_one()`.

- `plans/sum.md` is deleted. `plans/accuracy.md` drops its sum and `RRAY_REDUCE_ACC()` sections and points here.

---

# Behavior changes

Only the final total of an integer or logical sum is checked. This matches base R.

```r
x <- c(.Machine$integer.max, 1L, -1L)

rray_sum(x, 1L)
# main: Error: Integer overflow.
# now:  2147483647
```

`NA` always wins over an overflow, wherever it is. This also matches base R. On `main`, the answer depended on whether the `NA` came before or after the overflow.

```r
rray_sum(c(NA, .Machine$integer.max, 1L), 1L)
# main: NA
# now:  NA

rray_sum(c(.Machine$integer.max, 1L, NA), 1L)
# main: Error: Integer overflow.
# now:  NA
```

A total that doesn't fit still errors, with the same message as today. Base R returns `NA` with a warning. We keep the error.

```r
rray_sum(c(.Machine$integer.max, 1L), 1L)
# Error: Integer overflow.

rray_sum(c(.Machine$integer.max, NA, 1L), 1L, na_rm = TRUE)
# Error: Integer overflow.
```

A logical sum with more than `.Machine$integer.max` `TRUE` values in one output now errors. On `main` it overflowed an `int`, which is undefined behavior.

Double and complex results don't change at all.

---

# Integer and logical sums

## The loop

`rray_sum_lgl()`, `rray_sum_lgl_na_rm()`, `rray_sum_int()` and `rray_sum_int_na_rm()` all end up in one function. It takes the type's `NA` value and `na_rm` as arguments.

```c
static r_obj* rray_sum_lgl_or_int(
  const int* v_x,
  int na_value,
  bool na_rm,
  r_ssize out_size,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_out_broadcast_strides,
  struct r_lazy error_call
) {
  r_obj* sums = KEEP(r_alloc_raw0(out_size * sizeof(int64_t)));
  int64_t* v_sums = (int64_t*) r_raw_begin(sums);

  r_obj* missings = KEEP(r_alloc_raw0(out_size * sizeof(bool)));
  bool* v_missings = (bool*) r_raw_begin(missings);

  struct rray_run_iterator it;
  rray_run_iterator_init1(
    &it,
    v_dimensions,
    dimensionality,
    v_out_broadcast_strides
  );

  for (; !rray_run_iterator_done(&it); rray_run_iterator_next1(&it)) {
    const r_ssize start = rray_run_iterator_start(&it);
    const r_ssize end = rray_run_iterator_end(&it);

    r_ssize out_loc = rray_run_iterator_loc(&it, 0);
    const r_ssize out_stride = rray_run_iterator_stride(&it, 0);

    if (out_stride == 0) {
      int64_t sum = v_sums[out_loc];
      bool missing = v_missings[out_loc];

      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_value;
        sum += na ? 0 : x_elt;
        missing |= na;
      }

      v_sums[out_loc] = sum;
      v_missings[out_loc] = missing;
    } else {
      for (r_ssize i = start; i < end; ++i) {
        const int x_elt = v_x[i];
        const bool na = x_elt == na_value;
        v_sums[out_loc] += na ? 0 : x_elt;
        v_missings[out_loc] |= na;
        out_loc += out_stride;
      }
    }
  }

  r_obj* out = KEEP(r_alloc_integer(out_size));
  int* v_out = r_int_begin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    if (!na_rm && v_missings[i]) {
      v_out[i] = r_globals.na_int;
      continue;
    }

    const int64_t sum = v_sums[i];

    if (sum > INT_MAX || sum < -INT_MAX) {
      stop_int_overflow(error_call);
    }

    v_out[i] = (int) sum;
  }

  FREE(3);
  return out;
}
```

- `na_rm` doesn't need its own loop. The summing pass is the same either way: `NA` adds 0 and sets the flag. `na_rm` only decides whether the flag matters at the end.

- `stop_int_overflow()` from `arithmetic.h` gives the same "Integer overflow." message as today, so every snapshot stays the same.

- `rray_sum_lgl()` and friends become thin wrappers that pick the `NA` value and `na_rm`, like `rray_mean_lgl()`.

## Why it is fast now

clang vectorizes both inner loops at width 16. On `main` the integer loops didn't vectorize at all.

- Nothing in the loop can stop it early. On `main`, `rray_add_int_one()` could call `r_abort()` at any element.

- With no check on the running total, the adds can happen in any order. So the `out_stride == 0` loop can split into partial totals, which is what vectorizing it means.

- `v_sums` is `int64_t*` and `v_x` is `const int*`. The compiler knows they can't overlap, so the `out_stride != 0` loop vectorizes. On `main`, the output and the input were both `int*`, so the compiler had to assume they might.

## Why an `int64_t` total can't overflow

- Integer: every element is at most `INT_MAX` in size, which is below 2^31. Up to 2^32 of them add up to less than 2^63. `rray_sum_int()` and `rray_sum_int_na_rm()` send anything above that to the fallback, using the same `x_size / out_size` count as `rray_mean_int()`.

- Logical: every element is 0 or 1, and no output can get more than `R_XLEN_T_MAX` (2^52) elements. Logical sums never need a fallback.

## Why flags in the first pass

`rray_mean()` uses one `any_missing` flag and a second pass over `x` that writes `NA` only when the flag is set. For sum we measured both head to head, on a 1e4 x 1e3 integer matrix:

| Case | Second pass | First pass flags |
|---|---|---|
| No `NA`, along axis 1 | 0.98 ms | 0.97 ms |
| No `NA`, along axis 2 | 1.24 ms | 1.52 ms |
| 10 `NA`, along axis 1 | 4.33 ms | 0.97 ms |
| 10 `NA`, along axis 2 | 2.99 ms | 1.52 ms |

The flags cost 0.3 ms along axis 2 when there is no `NA`, from the extra byte written per element. In return, speed doesn't depend on whether the data has `NA`, and there is one less function. `main` takes about 9 ms in every one of these cases.

---

# The integer fallback past 2^32 elements

It keeps a 128-bit total in two halves. `lo` wraps around like an odometer, and `hi` counts how many times it wrapped.

```c
struct rray_sum_int128 {
  uint64_t lo;
  int64_t hi;
};

static inline struct rray_sum_int128 rray_sum_int128_add(
  struct rray_sum_int128 sum,
  int x
) {
  const uint64_t lo = sum.lo + (uint64_t) (int64_t) x;
  const int64_t hi = sum.hi + (x < 0 ? -1 : 0) + (lo < sum.lo);
  return (struct rray_sum_int128) {.lo = lo, .hi = hi};
}
```

The final check has one case for each sign:

```c
const struct rray_sum_int128 sum = v_sums[i];

if (sum.hi == 0 && sum.lo <= INT_MAX) {
  v_out[i] = (int) sum.lo;
} else if (sum.hi == -1 && sum.lo >= -(uint64_t) INT_MAX) {
  v_out[i] = -(int) -sum.lo;
} else {
  stop_int_overflow(error_call);
}
```

- Every operation is defined by the C standard. Unsigned math wraps. Converting a negative `int` to `uint64_t` is defined. There is no conversion from a large unsigned value back to a signed one.

- `hi` moves by at most 1 per element, so it stays within 2^52.

- `rray_sum_int_fallback()` takes `na_rm` and uses `v_missings` exactly like `rray_sum_lgl_or_int()`.

- It has one loop for every `out_stride`, not two. Speed doesn't matter here, and one loop means the long vector tests below cover all of it.

How it was checked:

- A standalone C program compared it with clang's `__int128` on 200,000 random sums, including totals pushed far past 2^64 and back, and a run of 2^33 `INT_MAX` values. No mismatches.

- With the 2^32 threshold set to 0, every integer sum went through the fallback, and the whole sum test file passed, plus the behavior examples above.

---

# Double and complex sums

Each of `rray_sum_dbl()`, `rray_sum_dbl_na_rm()`, `rray_sum_cpl()` and `rray_sum_cpl_na_rm()` gets its own loop, shaped like the first pass of `rray_mean_dbl()`.

```c
if (out_stride == 0) {
  double sum = v_out[out_loc];

  for (r_ssize i = start; i < end; ++i) {
    const double x_elt = v_x[i];
    sum += ISNAN(x_elt) ? 0 : x_elt;
  }

  v_out[out_loc] = sum;
} else {
  for (r_ssize i = start; i < end; ++i) {
    const double x_elt = v_x[i];
    v_out[out_loc] += ISNAN(x_elt) ? 0 : x_elt;
    out_loc += out_stride;
  }
}
```

- The adds happen in the same order as on `main`, so the results are identical. The test `"summing preserves accumulation order across runs"` still passes.

- `na_rm` uses `ISNAN(x) ? 0 : x` instead of an early return. This made double `na_rm` along axis 1 about 1.8x faster, and complex `na_rm` about 1.7x.

- Complex loops must write the whole struct at once:

  ```c
  const r_complex out_elt = v_out[out_loc];
  v_out[out_loc] = (r_complex) {
    .r = out_elt.r + (ISNAN(x_elt.r) ? 0 : x_elt.r),
    .i = out_elt.i + (ISNAN(x_elt.i) ? 0 : x_elt.i),
  };
  ```

  Updating `.r` and `.i` one at a time made complex `na_rm` along axis 2 2x slower than `main` (2.47 ms against 1.24 ms).

- A double sum along axis 1 doesn't get faster from the local total. Each add waits for the one before it, and that wait is the whole cost. Only pairwise summation will fix that.

---

# Other changes

## `src/one-add.h`

Delete the whole Reduce section: `rray_sum_lgl_one()`, `rray_sum_lgl_one_na_rm()`, `rray_sum_int_one()`, `rray_sum_int_one_na_rm()`, `rray_sum_dbl_one()`, `rray_sum_dbl_one_na_rm()`, `rray_sum_cpl_one()` and `rray_sum_cpl_one_na_rm()`.

Move your comment from `rray_sum_dbl_one()` above `rray_add_dbl_one()`, word for word:

```c
// Purposefully choose to match `rray_add()` rather than `sum()` regarding
// `c(NA, NaN)` behavior. Base R `sum()` forces `NA` if present, but `+`
// doesn't, so R is inconsistent. It's much faster to avoid checking for this,
// so we just say "it's implementation defined" for both add and sum in rray.
static inline double rray_add_dbl_one(double x, double y) {
  return x + y;
}
```

## `src/one-multiply.h`

In the comment above `rray_prod_dbl_one()`, change "see `rray_sum_dbl_one()`" to "see `rray_add_dbl_one()`".

## `src/reduce-sum.c`

Top down, following the repo's order rule:

1. `ffi_rray_sum()`, `rray_sum()`, `rray_sum_switch()`

2. `rray_sum_lgl()`, `rray_sum_lgl_na_rm()`, `rray_sum_int()`, `rray_sum_int_na_rm()`

3. `rray_sum_dbl()`, `rray_sum_dbl_na_rm()`, `rray_sum_cpl()`, `rray_sum_cpl_na_rm()`

4. `rray_sum_lgl_or_int()`, `rray_sum_int_fallback()`

5. `rray_sum_int128_add()`, `rray_sum_count()`

`RRAY_SUM_INT64_MAX_COUNT` and `struct rray_sum_int128` sit at the top of the file. Includes become `<stdint.h>`, `arithmetic.h`, `reduce.h`, `type.h` and `utils.h`. `one-add.h` is no longer included.

`src/decl/reduce-sum-decl.h` declares every helper.

## `R/reduce.R`

The Sum section currently says "If summing an integer array would overflow, an error is thrown." Proposed replacement:

```r
#' @section Sum:
#' Logicals are summed as integers. If an integer sum doesn't fit in an
#' integer, an error is thrown. Only the final sum is checked, and `NA` wins
#' over an overflow:
#'
#' ```r
#' x <- c(.Machine$integer.max, 1L, -1L)
#' rray_sum(x, 1L) # .Machine$integer.max
#'
#' x <- c(.Machine$integer.max, 1L, NA)
#' rray_sum(x, 1L) # NA
#' ```
```

## Protection

- `rray_sum_lgl_or_int()` and `rray_sum_int_fallback()`: `sums`, `missings` and `out` are each protected with `KEEP()` as soon as they are allocated, and released with `FREE(3)`. Nothing between the allocations reads an unprotected object. `stop_int_overflow()` jumps out, and R resets the protection stack.

- Double and complex: `out` is protected with `KEEP()` and released with `FREE(1)`. `rray_run_iterator_init1()` doesn't allocate.

- `rray_reduce()` protects whatever comes back with `KEEP()` straight away.

---

# Tests

Added to `tests/testthat/test-reduce-sum.R`, next to the existing overflow and `NA` tests. Each one runs along axis 1 and axis 2, since those go through different loops.

- Only the final sum is checked: `c(.Machine$integer.max, 1L, -1L)` gives `.Machine$integer.max`, and the same with `-.Machine$integer.max`.

- `NA` wins over overflow, whether the `NA` comes first or last.

- Sums that land exactly on `.Machine$integer.max` and `-.Machine$integer.max`.

- Overflow along axis 2 errors, with and without `na_rm`. The existing snapshots only cover axis 1.

- Each output handles its own `NA` and overflow: one row with `NA` and an overflow gives `NA`, while the row next to it gives a normal total.

- An output fed by several runs, from reducing a 3D array over axes 1 and 3, cancels back into range: `NA` in one column, `.Machine$integer.max - 1L` in another.

- `na_rm` with every value missing gives `0L`, along axis 2. The existing test only covers axis 1.

- Logical `NA` and `na_rm` along axis 2.

Behind `skip_if_not_testing_long_vectors()`, like the `rray_mean()` fallback tests:

- More than `.Machine$integer.max` `TRUE` values in one output errors: `array(TRUE, c(2^16, 2^15 + 1))` summed over `1:2`. This is 8 GB.

- The fallback with an output that overflows: `array(.Machine$integer.max, c(2^16 + 1, 2^16))` summed over `1:2`. This is 16 GB.

- The fallback with a total that fits after running far out of range: the same shape, with the first half of the columns set to `.Machine$integer.max`, the second half set to `-.Machine$integer.max`, and `x[1, 1]` lowered by 5. The result is `-5L`.

- The fallback with an `NA`, with and without `na_rm`.

The ``"errors on integer overflow with `na_rm = TRUE`"`` test and its snapshot stay as they are.

---

# Benchmarks

Medians from `bench::mark()`, 3 rounds alternating between `main` and the prototype, 40 iterations each. Apple silicon, Apple clang 17, `-O2`. No case varied by more than 6% between rounds. Base R is `sum()`, `colSums()` or `rowSums()`, where one exists.

| Input | Axes | Base R | `main` | New | Speedup |
|---|---|---|---|---|---|
| Integer, 1e7 x 1 | 1 | 6.01 ms | 9.11 ms | 0.96 ms | 9.5x |
| Integer, 1e4 x 1e3 | 1 | 8.93 ms | 9.05 ms | 0.97 ms | 9.3x |
| Integer, 1e4 x 1e3 | 2 | 6.28 ms | 9.13 ms | 1.50 ms | 6.1x |
| Integer, 1e4 x 1e3 | 1, 2 | | 9.03 ms | 0.96 ms | 9.4x |
| Integer with 10 `NA`, 1e4 x 1e3 | 1 | | 9.09 ms | 0.98 ms | 9.3x |
| Integer with 10 `NA`, 1e4 x 1e3 | 2 | | 9.03 ms | 1.50 ms | 6.0x |
| Integer with 10 `NA`, `na_rm`, 1e4 x 1e3 | 1 | | 6.84 ms | 0.98 ms | 7.0x |
| Integer with 10 `NA`, `na_rm`, 1e4 x 1e3 | 2 | | 6.37 ms | 1.51 ms | 4.2x |
| Logical, 1e4 x 1e3 | 1 | 8.92 ms | 6.10 ms | 0.97 ms | 6.3x |
| Logical, 1e4 x 1e3 | 2 | 6.36 ms | 1.47 ms | 1.53 ms | 0.96x |
| Double, 4000 x 4000 | 1 | 14.01 ms | 13.94 ms | 13.94 ms | 1.0x |
| Double, 4000 x 4000 | 2 | 2.44 ms | 2.45 ms | 2.45 ms | 1.0x |
| Double, 4000 x 4000 | 1, 2 | 24.15 ms | 14.34 ms | 14.36 ms | 1.0x |
| Double, `na_rm`, 4000 x 4000 | 1 | | 24.80 ms | 13.86 ms | 1.8x |
| Double, `na_rm`, 4000 x 4000 | 2 | | 2.47 ms | 2.46 ms | 1.0x |
| Double, 200 x 200 x 200 | 1 | | 3.88 ms | 3.88 ms | 1.0x |
| Double, 200 x 200 x 200 | 3 | | 1.83 ms | 1.81 ms | 1.0x |
| Double, 200 x 200 x 200 | 1, 3 | | 3.82 ms | 3.86 ms | 1.0x |
| Complex, 2000 x 2000 | 1 | 3.48 ms | 3.55 ms | 3.54 ms | 1.0x |
| Complex, 2000 x 2000 | 2 | 1.27 ms | 1.23 ms | 1.18 ms | 1.0x |
| Complex, `na_rm`, 2000 x 2000 | 1 | | 6.00 ms | 3.57 ms | 1.7x |
| Complex, `na_rm`, 2000 x 2000 | 2 | | 1.24 ms | 1.24 ms | 1.0x |

The one case that doesn't improve is logical along axis 2. `main`'s logical kernel never checked for overflow, so it already vectorized there. Writing an `int64_t` total and a flag per element costs about 4% against writing one `int`.

The script:

```r
set.seed(1)
v <- sample(100L, 1e7, TRUE)
x_full <- array(v, c(1e7, 1))
m_int <- matrix(v, 1e4, 1e3)
m_int_na <- m_int
m_int_na[sample(length(m_int_na), 10)] <- NA
m_lgl <- matrix(v > 50L, 1e4, 1e3)
m_dbl <- matrix(runif(16e6), 4000, 4000)
a_dbl <- array(runif(8e6), c(200, 200, 200))
m_cpl <- matrix(complex(real = runif(4e6), imaginary = runif(4e6)), 2000, 2000)

bench::mark(rray_sum(m_int, 1L), min_iterations = 40)
# and so on for each case in the table
```

To see what clang vectorizes:

```sh
cd src
clang -std=c99 -O2 -I$(R RHOME)/include -I./rlang -c reduce-sum.c -o /dev/null \
  -Rpass=loop-vectorize -Rpass-missed=loop-vectorize
```

---

# Designs we rejected

## `RRAY_REDUCE_ACC()`

`plans/accuracy.md` proposed a second macro with `ACC_CTYPE`, `RUN` and `FINISH` hooks. `rray_mean()` showed that hand written loops are easier to read and tune. Sum needs three different shapes (an `int64_t` total with flags, a 128-bit total, and plain floating point totals), which would stretch a macro too far.

## A second pass for `NA`, like `rray_mean()`

See "Why flags in the first pass". It is 2 to 4x slower with any `NA`, and needs one more function.

## A sentinel value in the sums buffer

An earlier prototype wrote `INT64_MIN` into `v_sums` for an output with `NA`. The fallback would have needed a second sentinel of its own. The flags replace both.

## Other fallbacks

- A `double` total, like `rray_mean_int_fallback()`. Totals above 2^53 round, so it could return a wrong integer when large values cancel. That is fine for a mean, but not for an integer result.

- An `int64_t` total that errors once a running total passes 2^62. It is exact when it returns, but it can error on a sum that would have come back into range.

## Updating complex fields one at a time

See "Double and complex sums". It was 2x slower along axis 2.

---

# Later

- Pairwise summation in the `out_stride == 0` loops of `rray_sum_dbl()` and `rray_sum_cpl()`, from plan 2 of `plans/accuracy.md`. That is the only way to speed up the double sum along axis 1, and it changes results in the last bits.

- `rray_mean_lgl_or_int()` could switch to flags in the first pass too, for the same speed with `NA` and one less function.

- A local total in the `out_stride == 0` loop of `RRAY_REDUCE()` isn't worth it. For doubles it measured the same as `main`, because the cost is waiting on each add, not the memory traffic.

---

# Order of work

One pull request:

1. Rewrite `src/reduce-sum.c` and `src/decl/reduce-sum-decl.h`.

2. Trim `src/one-add.h`, move the comment, and update `src/one-multiply.h`.

3. Update the Sum section in `R/reduce.R`, then redocument.

4. Add the tests, and run the long vector ones once locally with `RRAY_TESTING_LONG_VECTORS=true`.

5. Run `clang-format` and `air format`, do the protection pass, and benchmark against `main`.

6. Delete `plans/sum-accurate.md`. `plans/sum.md` is already deleted and `plans/accuracy.md` already trimmed on this branch.
