# Faster stride 0 loops for min and max

## Summary

Reducing over an axis that is contiguous in memory gives runs where every
element goes to the same output location (`out_stride == 0`). For several
reducers that loop is scalar and runs 5 to 10 times slower than the other
branch. The fix is to write the loop so clang can vectorize it.

Everything below was measured on an M2 Mac with Apple clang 17 at `-O2`,
16M element inputs, fastest of 3 alternating rounds against `main`.

## What blocks vectorization

clang only vectorizes a loop that carries a value across iterations if it
recognizes that value as a reduction (a sum, a min, a max, an or). Each of
these breaks that:

- A select that is not a plain min or max. `has_na ? NA : max(out, x)` is
  not recognized, so int max without `na_rm` stays scalar.

- Floating point compare and select. `out < x ? x : out` on doubles is only
  vectorized with `-ffast-math`, because of `NaN` and signed zero. There is
  no pragma to allow it for a single loop. `fmax()` does not help either,
  clang 17 keeps it scalar.

- A `uint8_t` running value. `missing < x_missing ? x_missing : missing`
  promotes to `int` and truncates back, and the reduction is not recognized.
  An `int` or `int64_t` running value vectorizes.

`clang -O2 -Rpass-analysis=loop-vectorize` names the reason for each loop,
for example "value that could not be identified as reduction is used outside
the loop".

The other branch (`out_stride != 0`) does not have this problem, since each
lane updates a different output. clang emits a SIMD version guarded by a
runtime overlap check.

## Keeping the running value in a local

`RRAY_REDUCE` wrote `v_out[out_loc]` on every iteration of the stride 0 loop.
Reading it into a local before the loop and writing it back after (as
`rray_sum_dbl()` already does) is a small win.

```c
OUT_CTYPE out_elt = v_out[out_loc];

for (r_ssize i = start; i < end; ++i) {
  out_elt = ONE(out_elt, v_x[i]);
}

v_out[out_loc] = out_elt;
```

- Most reducers: 0.97 to 1.00. The loop is limited by each step waiting on
  the previous one, and store to load forwarding hides most of the store.

- prod double, `[200, 200, 200]` over 1, 3: 0.85.

- max double with `na_rm`, `[10, 1e6]` over 1: 0.64. Short runs gain the
  most.

Worth doing on its own.

## Which stride 0 loops are already SIMD

From the assembly of `reduce-extremum.c`:

- int max with `na_rm`: SIMD. `rray_max_int_one_na_rm()` is a plain max,
  since integer `NA` is `INT_MIN`.

- int min without `na_rm`: SIMD. A plain min already returns `NA`.

- int max without `na_rm`: scalar, 10 ms vs 1 ms.

- int min with `na_rm`: scalar, 9 ms vs 1 ms.

- double max and min, both `na_rm` settings: scalar.

The logical versions mirror the integer ones.

## Integer max and min

Write the stride 0 loop as a plain max or min plus a separate missing flag.

Max without `na_rm`:

```c
int out_elt = v_out[out_loc];
bool any_missing = rray_int_is_missing(out_elt);

for (r_ssize i = start; i < end; ++i) {
  const int x_elt = v_x[i];
  out_elt = out_elt < x_elt ? x_elt : out_elt;
  any_missing = bool_bitwise_or(any_missing, rray_int_is_missing(x_elt));
}

v_out[out_loc] = any_missing ? r_globals.na_int : out_elt;
```

`any_missing` starts from the stored output, otherwise an `NA` from an
earlier run is lost, since `max(NA, x)` is `x`.

Min with `na_rm`:

```c
int x_elt = v_x[i];
x_elt = rray_int_is_missing(x_elt) ? INT_MAX : x_elt;
out_elt = x_elt < out_elt ? x_elt : out_elt;
```

Results:

| Case | Shape | main | new | ratio |
|---|---|---|---|---|
| max, `na_rm = FALSE` | `[4000, 4000]` over 1 | 10.06 | 1.03 | 0.10 |
| max, `na_rm = FALSE` | `[200, 200, 200]` over 1, 3 | 5.07 | 0.58 | 0.11 |
| max, `na_rm = FALSE` | `[10, 1e6]` over 1 | 6.29 | 3.55 | 0.56 |
| min, `na_rm = TRUE` | `[4000, 4000]` over 1 | 9.03 | 1.45 | 0.16 |
| min, `na_rm = TRUE` | `[200, 200, 200]` over 1, 3 | 4.51 | 0.84 | 0.19 |
| min, `na_rm = TRUE` | `[10, 1e6]` over 1 | 4.99 | 4.98 | 1.00 |

All other int max and min cases were 0.94 to 1.01. Results matched base R
exactly.

Only change the stride 0 loop. Rewriting `rray_min_int_one_na_rm()` itself
made the stride 0 loop fast but made the other branch 26% slower.

## Double max and min

### The int64 key

Read a double's bits as an `int64_t` and flip every bit except the sign for
negative numbers. Integer order then matches double order, and an integer
max vectorizes.

```c
static inline int64_t rray_dbl_key(double x) {
  int64_t bits;
  memcpy(&bits, &x, sizeof(bits));
  return bits ^ ((bits >> 63) & INT64_MAX);
}

static inline double rray_key_dbl(int64_t key) {
  const int64_t bits = key ^ ((key >> 63) & INT64_MAX);
  double out;
  memcpy(&out, &bits, sizeof(out));
  return out;
}
```

```
-Inf < -2.0 < -1.0 < -0.0 < 0.0 < 1.0 < 2.0 < Inf
```

The flip is its own inverse. `memcpy()` costs nothing once inlined. Detect
`NaN` with `isnan()` on the double, not on the bits, since clang emits a
single `fcmeq` and shares it with `rray_dbl_classify()`.

### Missing values

Follow `rray_sum_int()`: the main loop skips missing values and tracks them
per output in `v_missings`, then a fix-up loop writes `NA` or `NaN`. Doubles
have two kinds of missing and `NA` beats `NaN`, so `v_missings` holds a
`uint8_t` class instead of a `bool`. Reordering `enum rray_dbl_class` to
`number, nan, missing` lets a plain max pick the right one. Nothing depends
on the enum's numeric values.

Only use the key inside the stride 0 loop. Convert `v_out[out_loc]` to a key
when the run starts and back when it ends. The other branch keeps plain
doubles with `rray_max_dbl_one_na_rm()`, which skips `NaN` for free since
comparisons with `NaN` are false. This needs no key buffer and no extra
conversion pass.

Stride 0 without `na_rm`:

```c
int64_t out_key = rray_dbl_key(v_out[out_loc]);
int missing = v_missings[out_loc];

for (r_ssize i = start; i < end; ++i) {
  const double x_elt = v_x[i];
  const int x_missing = rray_dbl_classify(x_elt);
  const int64_t key = x_missing ? INT64_MIN : rray_dbl_key(x_elt);
  out_key = out_key < key ? key : out_key;
  missing = missing < x_missing ? x_missing : missing;
}

v_out[out_loc] = rray_key_dbl(out_key);
v_missings[out_loc] = missing;
```

The other branch without `na_rm`:

```c
for (r_ssize i = start; i < end; ++i) {
  const double x_elt = v_x[i];
  const uint8_t x_missing = rray_dbl_classify(x_elt);
  v_out[out_loc] = rray_max_dbl_one_na_rm(v_out[out_loc], x_elt);
  v_missings[out_loc] =
    v_missings[out_loc] < x_missing ? x_missing : v_missings[out_loc];
  out_loc += out_stride;
}
```

Fix-up:

```c
for (r_ssize i = 0; i < out_size; ++i) {
  switch (v_missings[i]) {
  case RRAY_DBL_number:
    break;
  case RRAY_DBL_nan:
    v_out[i] = R_NaN;
    break;
  case RRAY_DBL_missing:
    v_out[i] = r_globals.na_dbl;
    break;
  }
}
```

With `na_rm`, drop `v_missings` and the fix-up, and use
`isnan(x_elt) ? INT64_MIN : rray_dbl_key(x_elt)`.

Results for double max:

| `na_rm` | Shape | main | new | ratio |
|---|---|---|---|---|
| FALSE | `[4000, 4000]` over 1 | 22.52 | 9.12 | 0.40 |
| FALSE | `[4000, 4000]` over 2 | 13.59 | 5.50 | 0.40 |
| FALSE | `[200, 200, 200]` over 1, 3 | 11.16 | 4.62 | 0.41 |
| FALSE | `[10, 1e6]` over 1 | 10.00 | 9.19 | 0.92 |
| FALSE, early `NA` | `[4000, 4000]` over 1, 2 | 9.13 | 9.09 | 1.00 |
| TRUE | `[4000, 4000]` over 1 | 17.86 | 4.01 | 0.22 |
| TRUE | `[4000, 4000]` over 2 | 2.28 | 2.26 | 0.99 |
| TRUE | `[200, 200, 200]` over 1, 3 | 7.55 | 2.06 | 0.27 |
| TRUE | `[10, 1e6]` over 1 | 6.95 | 5.19 | 0.75 |

Results with sparse `NA` matched the rows without.

### Behavior changes

- `max(-0, 0)` is always `0` and `min(-0, 0)` is always `-0`. Base R and
  `main` return whichever came first. This was the only difference from base
  R across 5,600 random cases.

- A `NaN` result is always `R_NaN`, not the input's exact `NaN` bits. R
  cannot tell these apart.

### What did not work

- Four running values by hand. More code, still scalar, and it changes the
  sign of zero just like the key.

- Blocks of 1024 with a slow fallback for blocks containing `NaN` and an
  early exit on `NA`. Fast, but too complicated.

- A full `int64_t` key buffer for every output. The extra work made the
  other branch slower (`na_rm = TRUE` over 2 went from 2.3 to 4.0 ms), and it
  needed a conversion pass at the end.

- Plain doubles with `v_missings` and no key. Simple and exact, and over 2
  without `na_rm` dropped to 5.5 ms, but stride 0 stayed scalar at about
  18 ms.

## Open questions

- Without `na_rm`, stride 0 is about 2x the `na_rm` loop (9.1 vs 4.0 ms).
  The `NA` check costs a second compare, and the `int` running value forces
  each 64-bit lane result to be narrowed to 32 bits (`xtn`). An `int64_t`
  running value avoids the narrowing. It vectorized in a standalone test but
  was not benchmarked.

- The other branch without `na_rm` still reads and writes `v_missings` one
  byte per lane.

- Double min is untested. It mirrors max with `INFINITY` and `INT64_MAX`.

- Logical max and min have the same two slow cases as integer. With four
  copies of the integer pattern, a shared macro is likely better.

- Is the signed zero change acceptable?

## Other reducers using `RRAY_REDUCE`

- all and any: about 14 ms with or without `na_rm` on stride 0, so both are
  scalar. Not investigated.

- prod double and complex: a true multiply chain. Only several running
  values would help, and that changes rounding.

## Benchmarking

Run the same script from a `main` worktree and the branch, alternating for 3
rounds, and compare the fastest round. Spread was under 2% for nearly every
case at 40 to 60 iterations. Check correctness against base R `max()` and
`min()` through `apply()` on random 3D arrays mixing `NA`, `NaN`, `Inf`,
`-Inf`, `0`, `-0`, denormals, and runs longer than 1024.
