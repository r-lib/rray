# `rray_modulo()` and `rray_integer_divide()`

Future work. This document collects what we worked out about `%%` and `%/%`
before deciding to defer them, so that work isn't lost. Nothing here is built.

---

# Part 1: Types

Same family as `rray_add()` and friends: `lgl`, `int`, `dbl`. `cpl` errors, the
same way `chr`, `raw` and `list` do for every arithmetic operator. Base R does
the same — `1i %% 1` is "unimplemented complex operation".

Promotion table, matching the shape of 2.4 in `plans/implementation.md`:

| op | lgl | int | dbl | cpl |
|---|---|---|---|---|
| `%%` `%/%` | int | int | dbl | error |

So the switch has the same 16 arm shape as `rray_add_switch()`, except the four
`cpl` arms (`logical_complex`, `integer_complex`, `double_complex`,
`complex_complex`) join the `chr`/`raw`/`list` arms calling
`stop_unsupported_arithmetic()`, the way `rray_exponentiate_switch()` already
does it. That means two scalar operations per operator, `_int_one` and
`_dbl_one`, not three — there is no `_cpl_one`.

---

# Part 2: Why the double core isn't just `fmod()`

The obvious definition is `x %% y = x - floor(x / y) * y` and
`x %/% y = floor(x / y)`. That's what R computes, but not how R computes it.
`src/main/arithmetic.c` in R's own source has two static functions, `myfmod()`
and `myfloor()`, that compute it more carefully:

- A short circuit for `fabs(x1) <= fabs(x2)`, which avoids a division (and the
  rounding error a division introduces) whenever the direct sign comparison
  already gives the answer.

- A `long double` intermediate in the general case
  (`tmp = (long double)x1 - floor(q) * (long double)x2`), which halves the
  rounding error against doing the same arithmetic purely in `double`.

- A warning ("probable complete loss of accuracy in modulus") when
  `fabs(q) * DBL_EPSILON > 1`, i.e. when the quotient is large enough that the
  double subtraction can't be trusted at all.

- `x2 == 0` returns `NaN` for `%%`, not `NA`. `myfloor()` doesn't special case
  it at all — `x1 / 0.0` is already `Inf`, `-Inf` or `NaN` under IEEE 754, which
  is exactly what R's `%/%` returns for a double `0` divisor.

Neither function is exported. They're `static` in `arithmetic.c`, not declared
in `Rmath.h`, so matching R here means porting the logic, not calling into R.

The comment directly above `myfmod()` reads "Keep myfmod() and myfloor() in
step" — they're two instances of the same idea (division with a
precision-guarded remainder), and R's own maintainers treat them as a pair that
has to be changed together. Do the same if either one is ever touched.

---

# Part 3: The int core doesn't need its own version of this

For `%%`, R's int arm is:

```c
(x1 >= 0 && x2 > 0) ? x1 % x2 : (int) myfmod((double) x1, (double) x2);
```

C's `%` truncates (sign follows the dividend), but `%%` is documented to floor
(sign follows the divisor) — the same convention Python uses. Those only agree
when both operands are non-negative, which is the fast path above. Everywhere
else, R doesn't hand-roll a second floored-mod implementation in integer
arithmetic — it reuses `myfmod()` by promoting to `double`. That round trip is
exact: R's integer range fits inside ±2^31, and a `double` mantissa holds
integers exactly up to 2^53, so nothing is lost. One canonical implementation
of "floored modulo, correctly signed, correctly handling zero" beats two copies
that have to be kept in agreement forever.

`%/%`'s int arm doesn't need any of this:

```c
(int) floor((double) x1 / (double) x2);
```

`floor()` already gives the floor regardless of sign, unlike `%`'s truncation,
so there's no fast/slow split and no call into `myfloor()`. The asymmetry is
real: `%%` needs the precision-guarded fallback for integers, `%/%` doesn't.

Both int arms return `NA_INTEGER` for `x2 == 0`, unlike the double arms, which
return `NaN`/`Inf` instead of `NA`. That's a real asymmetry in base R, not a
simplification we'd be introducing.

---

# Part 4: What the ported code would look like

Each operator gets its own `src/arithmetic-{op}.c`/`.h` pair, same shape as
`src/arithmetic-add.c`: `ffi_rray_{name}()`, `rray_{name}()`, a static
`rray_{name}_switch()`, its 12 non-error cores (16 arms, minus the 4 that error
on `cpl`), and two scalar operations.

`rray_myfmod()` would live as a `static inline` helper in
`src/arithmetic-modulo.c`, next to `rray_modulo_dbl_one()`. `rray_myfloor()`
the same way in `src/arithmetic-integer-divide.c`. Ported from
`src/main/arithmetic.c`, `myfmod()`/`myfloor()`, dropping the `warning()` call
(open question below) and writing `DBL_EPSILON` in place of R's internal
`c_eps`, since we don't need the long double PowerPC workaround R carries:

```c
static inline double rray_myfmod(double x1, double x2) {
  if (x2 == 0.0) {
    return R_NaN;
  }

  if (fabs(x2) * DBL_EPSILON > 1 && R_FINITE(x1) && fabs(x1) <= fabs(x2)) {
    if (fabs(x1) == fabs(x2)) {
      return 0;
    }
    return ((x1 < 0 && x2 > 0) || (x2 < 0 && x1 > 0)) ? x1 + x2 : x1;
  }

  const double q = x1 / x2;
  const long double tmp = (long double) x1 - floor(q) * (long double) x2;
  return (double) (tmp - floorl(tmp / x2) * x2);
}

static inline double rray_myfloor(double x1, double x2) {
  const double q = x1 / x2;

  if (x2 == 0.0 || fabs(q) * DBL_EPSILON > 1 || !R_FINITE(q)) {
    return q;
  }

  if (fabs(q) < 1) {
    if (q < 0) {
      return -1;
    }
    return ((x1 < 0 && x2 > 0) || (x1 > 0 && x2 < 0)) ? -1 : 0;
  }

  const long double tmp = (long double) x1 - floor(q) * (long double) x2;
  return (double) (floor(q) + floorl(tmp / x2));
}
```

`R_NaN` and `R_FINITE()` come from `Rmath.h`, already linked. The scalar
operations that use them:

```c
static inline int rray_modulo_int_one(int x, int y, struct r_lazy error_call) {
  if (x == r_globals.na_int || y == r_globals.na_int || y == 0) {
    return r_globals.na_int;
  }
  if (x >= 0 && y > 0) {
    return x % y;
  }
  return (int) rray_myfmod((double) x, (double) y);
}

static inline double rray_modulo_dbl_one(double x, double y, struct r_lazy error_call) {
  return rray_myfmod(x, y);
}
```

```c
static inline int rray_integer_divide_int_one(int x, int y, struct r_lazy error_call) {
  if (x == r_globals.na_int || y == r_globals.na_int || y == 0) {
    return r_globals.na_int;
  }
  return (int) floor((double) x / (double) y);
}

static inline double rray_integer_divide_dbl_one(double x, double y, struct r_lazy error_call) {
  return rray_myfloor(x, y);
}
```

Both `int` cores are shared across the `lgl_lgl`, `lgl_int`, `int_lgl` and
`int_int` type pairs, the way `rray_multiply_int_one()` is. Both `dbl` cores
are shared across every pair involving a `dbl`, the way `rray_divide_dbl_one()`
is.

Tests: `tests/testthat/test-arithmetic-modulo.R` and
`tests/testthat/test-arithmetic-integer-divide.R`, one file per operator as
usual, working through all 12 non-error type combinations in both positions,
plus `NA`, `NaN`, `Inf` and `y == 0` on both sides.

---

# Part 5: Open questions for whenever this gets picked up

- **The "loss of accuracy" warning.** `myfmod()` calls base R's `warning()`
  when the quotient is too large to trust. Porting that means a real R level
  warning coming out of `rray_modulo()`, which is more user visible surface
  than we've added for any other arithmetic operator so far. Worth deciding
  deliberately rather than silently dropping it or silently keeping it.

- **Licensing.** R is GPL (`r-svn/COPYING` is GPL-2), and rray4 is
  `MIT + file LICENSE`. `myfmod()`/`myfloor()` aren't a restatement of the
  floored-mod definition — the precision-loss short circuit, the `long double`
  intermediate, and the warning threshold are specific implementation choices,
  which is the part copyright actually protects. Porting that logic verbatim
  into an MIT file is porting GPL-covered expression into an MIT-licensed
  package, and a compiled R package links all its C sources into one shared
  object, so there's no isolating "just that file" at the licensing boundary
  that distribution creates.

  Not something to resolve by picking the option that's least annoying —
  it changes what the package can claim about its own license. Worth raising
  explicitly with Davis before writing a single line of this, rather than
  after. The realistic options are: reimplement the floored-mod arithmetic
  independently from the mathematical definition (accepting that ultra-precise
  edge cases like the loss-of-accuracy threshold may not match R bit for bit),
  or relicense/dual-license the affected files, or decide it's not worth
  either tradeoff and leave `%%`/`%/%` unimplemented.
