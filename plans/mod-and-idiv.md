# `rray_modulo()` and `rray_integer_divide()`

Future work. This document collects what we worked out about `%%` and `%/%`
before deciding to defer them, so that work isn't lost. Nothing here is built.

---

# Part 1: Types

Same family as `rray_add()` and friends: `lgl`, `int`, `dbl`. `cpl` errors, the
same way `chr`, `raw` and `list` do for every arithmetic operator. Base R does
the same: `1i %% 1` is "unimplemented complex operation".

Promotion table, matching the shape of 2.4 in `plans/implementation.md`:

| op | lgl | int | dbl | cpl |
|---|---|---|---|---|
| `%%` `%/%` | int | int | dbl | error |

So the switch has the same 16 arm shape as `rray_add_switch()`, except the four
`cpl` arms (`logical_complex`, `integer_complex`, `double_complex`,
`complex_complex`) join the `chr`/`raw`/`list` arms calling
`stop_unsupported_arithmetic()`, the way `rray_exponentiate_switch()` already
does it. That means two scalar operations per operator, `_int_one` and
`_dbl_one`, not three. There is no `_cpl_one`.

---

# Part 2: Why we don't port R's code

R computes `%%` and `%/%` with two static functions in `src/main/arithmetic.c`,
`myfmod()` and `myfloor()`. They are not exported, so matching R would mean
porting them. We don't, for three reasons.

- **They lean on `long double`.** Both compute a correction term in
  `long double` to cut rounding error. rray only uses `double`. On arm64 macOS,
  `long double` is the same as `double`, so R itself already runs the double
  only version there, and R's results differ between x86_64 Linux and arm64
  macOS. "Bit for bit with R" was never one fixed target.

- **Licensing.** R is GPL-2 and rray is MIT. Porting R's specific
  implementation into an MIT package is a problem. Writing the operations from
  their definitions, on top of C's `fmod()`, `floor()` and `fma()`, is not.

- **There is a simpler, more accurate route.** C's `fmod()` returns the exact
  remainder of two doubles, with no rounding at all. C's `fma(a, b, c)`
  computes `a * b + c` with a single rounding. Together they give results that
  match R everywhere R is correct, and are exact in places where R is not.

Two things in the plan get simpler as a result:

- **No "loss of accuracy" warning.** `myfmod()` warns when the quotient is so
  large that its double subtraction can't be trusted. `fmod()` is exact at any
  size, so there is nothing to warn about.

- **No `long double`, `floorl()`, or `c_eps`.**

A note on R's short circuit in `myfmod()`, which an earlier version of this
plan described wrong: it only applies when `fabs(x2) * DBL_EPSILON > 1`, meaning
the divisor is bigger than 2^52 (about 4.5e15). It is not a general shortcut
for `fabs(x1) <= fabs(x2)`. It matters below, because it is why `-1 %% 1e300`
returns `1e300` in R but `-1e-10 %% 1e9` returns `0`.

---

# Part 3: The double cores

## `%%`

```c
static inline double rray_modulo_dbl_one(double x, double y) {
  // R returns `NaN` (not `NA`) for a zero divisor, for any `x`, including
  // `NA`. `fmod(x, 0)` would also give `NaN`, but this states it outright and
  // matches R's `NaN` exactly rather than relying on `fmod()`'s choice.
  if (y == 0.0) {
    return R_NaN;
  }

  // The exact remainder of `x / y`, with no rounding. C's `fmod()` truncates,
  // so the result has the sign of `x` (e.g. `fmod(-7, 3)` is `-1`). It also
  // passes `NA` and `NaN` through, and gives `NaN` when `x` is infinite, and
  // returns `x` itself when `y` is infinite.
  double out = fmod(x, y);

  // R's `%%` floors instead, so the result must take the sign of `y`
  // (e.g. `-7 %% 3` is `2`). If the remainder is nonzero and its sign
  // disagrees with `y`, shift it by one whole `y` into the right range:
  // `-1 + 3` is `2`. This addition is the only step that can round, and only
  // when `out` is tiny next to `y`.
  if (out != 0.0 && (out < 0.0) != (y < 0.0)) {
    out += y;
  }

  // When that addition rounds, it can land exactly on `y`
  // (e.g. `-1e-20 + 1` rounds to `1`). R returns `0` in that case, except when
  // `|y| > 2^52`, where R's short circuit returns `y`. Copy both.
  if (out == y && fabs(y) <= 0x1p52) {
    out = 0.0;
  }

  return out;
}
```

## `%/%`

```c
static inline double rray_integer_divide_dbl_one(double x, double y) {
  // The rounded quotient. Usually right, but rounding can push it up onto the
  // next whole number (e.g. `456.09 / 0.01` gives `45609` exactly, while the
  // true quotient is just under that).
  const double q = x / y;

  // Return the plain quotient, like R, when:
  // - `y` is `0`: `q` is already `Inf`, `-Inf` or `NaN`.
  // - `q` is not finite: `Inf`, `NA` or `NaN` pass straight through.
  // - `|q| > 2^52`: every double this large is already a whole number, so
  //   there is nothing to floor.
  if (y == 0.0 || !R_FINITE(q) || fabs(q) > 0x1p52) {
    return q;
  }

  // Our candidate answer. It is never too low: a rounded quotient cannot drop
  // below a whole number that the true quotient is at or above. It can only
  // be one too high, which the next lines check for.
  const double out = floor(q);

  // The leftover `x - out * y`, with a single rounding, so its sign is always
  // correct. When `y` is infinite, `q` is `±0` and so `out` is `0`. The
  // leftover is then just `x`, and `fma()` would wrongly give `NaN` from
  // `0 * Inf`, so skip it.
  const double r = R_FINITE(y) ? fma(-out, y, x) : x;

  // For a correct floor, the leftover has the same sign as `y` (or is `0`).
  // If it has the opposite sign, `out * y` overshot `x`, meaning `out` is one
  // too high.
  if (r != 0.0 && (r < 0.0) != (y < 0.0)) {
    return out - 1.0;
  }

  return out;
}
```

`R_NaN` and `R_FINITE()` come from `Rmath.h`, which is already linked and
already used in `src/reduce-mean.c`. `fmod()`, `floor()`, `fabs()` and `fma()`
come from `math.h`. On arm64 `fma()` is a single hardware instruction.

---

# Part 4: The int cores

For `%%`, C's `%` truncates and R's `%%` floors. They only agree when `x >= 0`
and `y > 0`, so that is the fast path. Everything else goes through the double
core. That round trip is exact, because every `int` fits in a double.

```c
static inline int rray_modulo_int_one(int x, int y) {
  // Unlike the double core, R returns `NA` (not `NaN`) for a zero divisor here
  if (rray_int_is_missing(x) || rray_int_is_missing(y) || y == 0) {
    return r_globals.na_int;
  }

  // Both nonnegative, so C's truncating `%` already gives the floored answer
  if (x >= 0 && y > 0) {
    return x % y;
  }

  // Any other sign combination: reuse the double core, which is exact for
  // `int` inputs
  return (int) rray_modulo_dbl_one((double) x, (double) y);
}
```

For `%/%`, the double core isn't needed. With `int` inputs, `x / y` can never
round up onto a whole number: the gap between the true quotient and the next
whole number is at least `1 / |y|`, far bigger than double rounding error at
these sizes.

```c
static inline int rray_integer_divide_int_one(int x, int y) {
  // R returns `NA` for a zero divisor here too
  if (rray_int_is_missing(x) || rray_int_is_missing(y) || y == 0) {
    return r_globals.na_int;
  }

  // `floor()` rounds toward negative infinity for any signs, which is exactly
  // `%/%`. The result always fits in an `int`, since `INT_MIN` is `NA`.
  return (int) floor((double) x / (double) y);
}
```

Neither int core can error, so both are called with `RRAY_BINARY_NO_ARGS`, the
way `rray_divide_dbl_one()` is.

Both `int` cores are shared across the `lgl_lgl`, `lgl_int`, `int_lgl` and
`int_int` type pairs, the way `rray_multiply_int_one()` is. Both `dbl` cores
are shared across every pair involving a `dbl`, the way `rray_divide_dbl_one()`
is.

---

# Part 5: How these were tested against R

On arm64 macOS (R 4.6, double only `long double`), each core was compiled into
a small shared library and compared with R's `%%` and `%/%` on these inputs:

| Inputs | Pairs |
|---|---|
| normal / normal | 1,000,000 |
| uniform / `0.1`, `0.2`, `0.3`, `1`, `2.5`, `-0.7` | 1,000,000 |
| 2 decimal places / `0.01`, `0.05`, `0.1`, `7`, `-3` | 1,000,000 |
| exact multiples (`k * 0.3` / `0.3` and similar) | 1,000,000 |
| one ULP off an exact multiple | 1,000,000 |
| whole numbers stored as doubles | 1,000,000 |
| magnitudes from 1e-10 to 1e20 over 1e-10 to 1e10 | 1,000,000 |
| quotients from 1e15 to 1e300 | 1,000,000 |
| every pair of `0`, `-0`, `±1`, `±0.5`, `±3`, `±1e300`, `5e-324`, `±Inf`, `NaN`, `NA` | 225 |
| `int` inputs, small and near `±2e9` | 1,000,000 |

Results:

- **`%/%` (double)**: 0 mismatches anywhere. For comparison, plain
  `floor(x / y)` had 105,875 mismatches on the 2 decimal place set alone.

- **`%/%` (int)**: 0 mismatches.

- **`%%` (int)**: 0 mismatches.

- **`%%` (double)**: 0 mismatches except 14 cases, all of them cases where R
  itself is wrong:

  - `±1e300 %% ±3`: R warns "probable complete loss of accuracy" and returns
    `±2.06e267`. We return the exact remainder (`1` or `2`). One random pair
    with a quotient above 2^52 failed the same way.

  - `x %% 5e-324` for `x` in `±1`, `±0.5`, `±3`, `±1e300`: R returns `NaN`.
    Every double is an exact multiple of `5e-324`, so we return `0`.

  - `5e-324 %% -3`: R returns `5e-324`, which has the wrong sign for a floored
    modulo. We return `0`.

An alternative `%/%` built on `fmod()`, the way CPython's `float_divmod()` does
it, was also tried and rejected. It was off by one for quotients between about
2^51 and 2^52, because `x - fmod(x, y)` rounds there. Checked with exact
fractions, R was right in all 354 of those cases.

These results only cover arm64 macOS. x86_64 R uses a real `long double`, so
the tests in the package should check behavior (signs, zero divisors, `NA`,
`NaN`, `Inf`, the examples above), not exact agreement with R on every
platform.

---

# Part 6: What the files look like

Each operator gets its own `src/arithmetic-{op}.c`/`.h` pair, same shape as
`src/arithmetic-divide.c`: `ffi_rray_{name}()`, `rray_{name}()`, a static
`rray_{name}_switch()`, its 12 non-error cores (16 arms, minus the 4 that error
on `cpl`), and the two scalar operations above.

Tests: `tests/testthat/test-arithmetic-modulo.R` and
`tests/testthat/test-arithmetic-integer-divide.R`, one file per operator as
usual, working through all 12 non-error type combinations in both positions,
plus `NA`, `NaN`, `Inf` and `y == 0` on both sides, plus the rounding examples
from Parts 3 and 5 (`456.09 %/% 0.01`, `-1e-20 %% 1`, `-1 %% 1e300`).
