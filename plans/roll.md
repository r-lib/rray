# Rolling plan

## Recommendation

Add two functions:

```r
rray_roll(x, ..., n, axes)
rray_roll_each(x, ..., n, axis)
```

Both move elements along an axis. An element pushed off one end comes back on
the other.

- `rray_roll()` rolls one or more axes, with one shift per axis. Every lane
  through an axis moves by the same amount, so each output position has exactly
  one source position, and names roll with the data.

- `rray_roll_each()` rolls one axis, with one shift per lane. Lanes can move by
  different amounts, so an output position can come from different source
  positions in different lanes. Names on the rolled axis are always dropped.

The split mirrors pairs the package already has:

| Every lane moves together | Each lane moves on its own |
|---|---|
| `rray_slice_axis()` | `rray_index_axis()` |
| `rray_rep()` | `rray_rep_each()` |
| `rray_roll()` | `rray_roll_each()` |

The function decides what happens to names. The size or values of `n` never do.
A caller who wants names kept calls `rray_roll()`.

## Terms

- **Lane.** One run of elements along the rolled axis, with every other
  coordinate held fixed. For a matrix and `axis = 2`, each row is a lane. For
  dimensions `(A, B, C)` and `axis = 2`, there are `A * C` lanes, each of
  length `B`.

- **Lane dimensions.** The dimensions of `x` with the rolled axis set to 1. For
  `(A, B, C)` and `axis = 2`, they are `(A, 1, C)`. There is one lane per
  element of an array with these dimensions.

- **Shift.** The value of `n` for one lane or one axis. It can be negative,
  zero, or larger than the dimension.

## Shared rules

### Direction

A positive shift moves elements toward higher positions, as in NumPy:

```r
rray_roll(1:5, n = 2, axes = 1)
# [1] 4 5 1 2 3
```

A negative shift moves them toward lower positions:

```r
rray_roll(1:5, n = -1, axes = 1)
# [1] 2 3 4 5 1
```

### Wrapping

A shift is reduced modulo the dimension `d` of the rolled axis, into `[0, d)`:

```text
k = n mod d
out[..., j, ...] = x[..., (j - k) mod d, ...]
```

So a shift of `7` on a dimension of `5` is a shift of `2`, and a shift of `-1`
is a shift of `4`:

```r
rray_roll(1:5, n = 7, axes = 1)
# [1] 4 5 1 2 3
```

A shift of `0`, or any multiple of `d`, leaves the axis unchanged.

### Values of `n`

- Integers, and anything that casts losslessly to integer. Doubles holding
  whole numbers work and `1.5` is an error. Logicals work too, with `TRUE` as
  `1`, the same as `axes` and `times` accept them today.

- Any sign and any magnitude within the R integer range, which is
  `-2147483647` to `2147483647`. `-2147483648` is reserved for `NA`, and a
  double outside the range fails the cast.

- No missing values.

- No classed objects.

### Zero dimensions

When the rolled axis has dimension 0, there is nothing to move. Every shift is
valid, and it is still validated. The result equals `x`, with the names rule of
the function applied.

### Bare vectors

`x` goes through `arg_as_array()`, so a bare vector is a one-dimensional array
and its names move to `dimnames`. The result is always an array.

## `rray_roll()`

```r
rray_roll(x, ..., n, axes)
```

### Arguments

- `x`: an array or bare vector.

- `...`: must be empty. It forces `n` and `axes` to be named, as in
  `rray_rep()`.

- `n`: a bare integer vector of size 1 or the size of `axes`, validated the
  same way as `axes`. A size 1 `n` is used for every axis in `axes`. Otherwise
  `n[[i]]` is the shift for `axes[[i]]`. `n` can't have attributes, so names,
  dimensions, and classes are all errors. Arrays of per-lane shifts belong to
  `rray_roll_each()`.

- `axes`: a bare integer vector of axes. Required, with no default. Any order.
  An axis can't appear twice. `integer()` is allowed and rolls nothing.

The result has the type, dimensions, and dimensionality of `x`.

### Worked examples

#### One dimension

```r
rray_roll(1:5, n = 2, axes = 1)
# [1] 4 5 1 2 3

rray_roll(1:5, n = -1, axes = 1)
# [1] 2 3 4 5 1

rray_roll(1:5, n = 7, axes = 1)
# [1] 4 5 1 2 3

rray_roll(1:5, n = 0, axes = 1)
# [1] 1 2 3 4 5
```

Each result is a one-dimensional array with dimensions `5`.

#### A matrix

```r
x <- matrix(
  1:6,
  nrow = 2,
  dimnames = list(c("r1", "r2"), c("a", "b", "c"))
)

x
#    a b c
# r1 1 3 5
# r2 2 4 6
```

Roll the columns:

```r
rray_roll(x, n = 1, axes = 2)
#    c a b
# r1 5 1 3
# r2 6 2 4

rray_roll(x, n = -1, axes = 2)
#    b c a
# r1 3 5 1
# r2 4 6 2
```

Roll the rows:

```r
rray_roll(x, n = 1, axes = 1)
#    a b c
# r2 2 4 6
# r1 1 3 5
```

Roll both at once. A size 1 `n` is used for every axis:

```r
rray_roll(x, n = 1, axes = c(1, 2))
#    c a b
# r2 6 2 4
# r1 5 1 3

rray_roll(x, n = c(1, 1), axes = c(1, 2))
#    c a b
# r2 6 2 4
# r1 5 1 3
```

#### `n` pairs with `axes` in the order given

```r
x <- array(1:24, c(2, 3, 4))

out <- rray_roll(x, n = c(1, -1), axes = c(3, 1))
```

This rolls axis 3 by `1` and axis 1 by `-1`. The first sheet of the result is
the last sheet of `x` with its two rows swapped:

```r
x[, , 4]
#      [,1] [,2] [,3]
# [1,]   19   21   23
# [2,]   20   22   24

out[, , 1]
#      [,1] [,2] [,3]
# [1,]   20   22   24
# [2,]   19   21   23
```

#### Names on a bare vector

```r
rray_roll(c(a = 1L, b = 2L, c = 3L, d = 4L), n = 1, axes = 1)
# d a b c
# 4 1 2 3
```

The result is a one-dimensional array with names `c("d", "a", "b", "c")`.

#### Nothing moves

Each of these gives a new array identical to `x`, names included. They run
through the same path as every other roll, with no early exit:

```r
x <- matrix(
  1:6,
  nrow = 2,
  dimnames = list(c("r1", "r2"), c("a", "b", "c"))
)

rray_roll(x, n = 0, axes = 2)
rray_roll(x, n = 3, axes = 2)          # dimension of axis 2 is 3
rray_roll(x, n = 1, axes = integer())
rray_roll(array(integer(), c(2, 0)), n = 1, axes = 2)
```

### Names

Each rolled axis reorders its names with the same locations as its data. Every
other axis keeps its names untouched. This is the subset rule from
`plans/implementation.md` 2.3, and is exactly what `rray_slice_axis()` does for
the same locations.

### Relationship to NumPy

| NumPy | rray4 |
|---|---|
| `np.roll(x, 1, axis=0)` | `rray_roll(x, n = 1, axes = 1)` |
| `np.roll(x, (1, 2), axis=(0, 1))` | `rray_roll(x, n = c(1, 2), axes = c(1, 2))` |
| `np.roll(x, 1, axis=(0, 1))` | `rray_roll(x, n = 1, axes = c(1, 2))` |
| `np.roll(x, (1, 2), axis=(0, 0))` | Error. Write `rray_roll(x, n = 3, axes = 1)` |
| `np.roll(x, 1)` | Not supported. `axes` is required |

NumPy adds up shifts on a repeated axis. rray4 treats a repeated axis as a
mistake.

### Errors

```r
x <- matrix(1:6, nrow = 2)

rray_roll(x, n = 1, axes = c(1, 1))
# Error in `rray_roll()`:
# ! `axes` must not contain 1 more than once.

rray_roll(x, n = c(1, 2, 3), axes = c(1, 2))
# Error in `rray_roll()`:
# ! `n` must be size 1 or size 2 to match `axes`, not size 3.

rray_roll(x, n = c(1, 2), axes = 1)
# Error in `rray_roll()`:
# ! `n` must be size 1, not size 2.

rray_roll(x, n = NA, axes = 1)
# Error in `rray_roll()`:
# ! `n` must not contain missing values.

rray_roll(x, n = 1.5, axes = 1)
# Error: Can't convert from `n` <double> to <integer> due to loss of precision.
# • Locations: 1

rray_roll(x, n = matrix(1L), axes = 1)
# Error in `rray_roll()`:
# ! `n` can't have attributes.

rray_roll(x, n = 1, axes = 3)
# Error in `rray_roll()`:
# ! `axes` must contain values less than or equal to the dimensionality of 2,
#   not 3.

rray_roll(x, 1, 2)
# Error from `check_dots_empty0()`
```

The axes errors come from `arg_as_axes_unsorted()`, and the cast and
attribute errors from `arg_as_bare_integer()`. Only the two size errors and
the missing value error are new text. The exact first lines of messages
from existing helpers are whatever those helpers print today. Snapshot them
rather than retyping them.

## `rray_roll_each()`

```r
rray_roll_each(x, ..., n, axis)
```

### Arguments

- `x`: an array or bare vector.

- `...`: must be empty.

- `n`: an integer array of shifts, one per lane. It must broadcast to the lane
  dimensions of `x`. A bare vector is a one-dimensional array.

- `axis`: a single axis to roll along.

The result has the type and dimensions of `x`.

### How `n` broadcasts

`n` broadcasts to the lane dimensions with the ordinary left-aligned rules:

- On every axis, `n` has dimension 1 or the lane dimension.

- On the rolled axis, the lane dimension is 1, so `n` must be 1 there.

- `n` can have fewer axes than `x`. Missing trailing axes count as 1.

- `n` can't have more axes than `x`.

It is directional, like `rray_index_axis()` in `plans/index.md`: `n` can grow
to the lane dimensions, but it can never grow `x`.

A dimension of 1 in `n` means "the same shift for all of these". So `n` only
needs a real dimension on the axes where the shift actually changes. Axes after
the last one that changes can be left off. Axes before it are written as 1.

For `x` with dimensions `(A, B, C)` and `axis = 2`, the lane dimensions are
`(A, 1, C)`:

| `n` | Dimensions | Meaning |
|---|---|---|
| `1` | `(1)` | One shift for every lane |
| `c(...)` of length `A` | `(A)` | One shift per position on axis 1 |
| `array(..., c(1, 1, C))` | `(1, 1, C)` | One shift per position on axis 3 |
| `array(..., c(A, 1, C))` | `(A, 1, C)` | One shift per lane |
| `array(..., c(A, B, C))` | `(A, B, C)` | Error, axis 2 must be 1 |
| `array(..., c(A, 1, C, 2))` | `(A, 1, C, 2)` | Error, too many axes |

A size 1 `n` works and rolls every lane by the same amount. It runs through the
general per-lane loop. There is no special path for it, because a caller who
wants a uniform shift should use `rray_roll()`, which also keeps names.

### Worked examples

#### One shift per row

This is the question in
[Roll rows of a matrix independently](https://stackoverflow.com/questions/20360675/roll-rows-of-a-matrix-independently).

```r
x <- matrix(1:12, nrow = 3, byrow = TRUE)

x
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    3    4
# [2,]    5    6    7    8
# [3,]    9   10   11   12

rray_roll_each(x, n = c(1, 0, -1), axis = 2)
#      [,1] [,2] [,3] [,4]
# [1,]    4    1    2    3
# [2,]    5    6    7    8
# [3,]   10   11   12    9
```

The lane dimensions are `(3, 1)`. The vector `n` has dimensions `(3)`, read as
`(3, 1)`, so it fits with no reshaping.

#### One shift per column

```r
x <- matrix(1:12, nrow = 3)

x
#      [,1] [,2] [,3] [,4]
# [1,]    1    4    7   10
# [2,]    2    5    8   11
# [3,]    3    6    9   12

rray_roll_each(x, n = matrix(c(0, 1, 2, 3), nrow = 1), axis = 1)
#      [,1] [,2] [,3] [,4]
# [1,]    1    6    8   10
# [2,]    2    4    9   11
# [3,]    3    5    7   12
```

The lane dimensions are `(1, 4)`, so `n` must be a one row matrix. A plain
vector of length 4 has dimensions `(4)` and fails on axis 1:

```r
rray_roll_each(x, n = c(0, 1, 2, 3), axis = 1)
# Error in `rray_roll_each()`:
# ! Can't broadcast axis 1 of `n` from dimension 4 to 1.
```

The last column has a shift of `3` on a dimension of `3`, so it doesn't move.

#### Three dimensions

`x` has dimensions `(2, 5, 3)`. Think of it as 3 sheets, each with 2 rows and 5
columns. Roll along axis 2, the columns. The lane dimensions are `(2, 1, 3)`,
so there are 6 lanes: 2 rows on each of 3 sheets.

```r
x <- array(1:30, c(2, 5, 3))

x[, , 1]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    3    5    7    9
# [2,]    2    4    6    8   10

x[, , 2]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   11   13   15   17   19
# [2,]   12   14   16   18   20

x[, , 3]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   21   23   25   27   29
# [2,]   22   24   26   28   30
```

One shift for every lane:

```r
out <- rray_roll_each(x, n = 1, axis = 2)

out[, , 1]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    9    1    3    5    7
# [2,]   10    2    4    6    8

out[, , 2]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   19   11   13   15   17
# [2,]   20   12   14   16   18

out[, , 3]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   29   21   23   25   27
# [2,]   30   22   24   26   28
```

The values match `rray_roll(x, n = 1, axes = 2)`. Only the names rule differs.

One shift per row, the same on every sheet. `n` has dimensions `(2)`, read as
`(2, 1, 1)`:

```r
out <- rray_roll_each(x, n = c(1, 2), axis = 2)

out[, , 1]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    9    1    3    5    7
# [2,]    8   10    2    4    6

out[, , 2]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   19   11   13   15   17
# [2,]   18   20   12   14   16

out[, , 3]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   29   21   23   25   27
# [2,]   28   30   22   24   26
```

One shift per sheet, the same for both rows. `n` has dimensions `(1, 1, 3)`,
so axis 1 and axis 2 are written as 1:

```r
out <- rray_roll_each(x, n = array(c(0, 1, 2), c(1, 1, 3)), axis = 2)

out[, , 1]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    3    5    7    9
# [2,]    2    4    6    8   10

out[, , 2]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   19   11   13   15   17
# [2,]   20   12   14   16   18

out[, , 3]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   27   29   21   23   25
# [2,]   28   30   22   24   26
```

One shift per lane. `n` has the full lane dimensions `(2, 1, 3)`:

```r
n <- array(1:6, c(2, 1, 3))

n[, 1, ]
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6

out <- rray_roll_each(x, n = n, axis = 2)

out[, , 1]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    9    1    3    5    7
# [2,]    8   10    2    4    6

out[, , 2]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   15   17   19   11   13
# [2,]   14   16   18   20   12

out[, , 3]
#      [,1] [,2] [,3] [,4] [,5]
# [1,]   21   23   25   27   29
# [2,]   30   22   24   26   28
```

Row 1 of sheet 3 has a shift of `5` on a dimension of `5`, so it doesn't move.
Row 2 of sheet 3 has a shift of `6`, which wraps to `1`.

#### Shifts computed along the axis

A reducer keeps the reduced axis as a dimension of 1, so anything computed
along `axis` already has the lane dimensions:

```r
n <- rray_sum_along(counts, axes = 2)   # dimensions (2, 1, 3)
rray_roll_each(x, n = n, axis = 2)
```

#### One dimension

A one-dimensional `x` has one lane, so `n` must have size 1:

```r
rray_roll_each(1:5, n = 2, axis = 1)
# [1] 4 5 1 2 3
```

### Names

- Names on the rolled axis are always dropped, whatever the size or values of
  `n`.

- Every other axis keeps its names. Its dimension is unchanged and every
  position still means the same thing.

- Names on `n` are ignored.

This is the reduce rule from `plans/implementation.md` 2.3 applied to the one
rolled axis, and is what `rray_index_axis()` does to its selected axis.

Dropping is forced by the per-lane case. When lanes move by different amounts,
column 1 can hold data from column `c` in one row and from column `b` in
another, so no single name is correct:

```r
x <- matrix(
  1:6,
  nrow = 2,
  dimnames = list(c("r1", "r2"), c("a", "b", "c"))
)

rray_roll_each(x, n = c(1, 2), axis = 2)
#    [,1] [,2] [,3]
# r1    5    1    3
# r2    4    6    2
```

The same rule applies when every lane moves by the same amount, so changing a
value in `n` never changes the names:

```r
rray_roll_each(x, n = 1, axis = 2)
#    [,1] [,2] [,3]
# r1    5    1    3
# r2    6    2    4
```

Rolling along the rows drops the row names and keeps the column names:

```r
rray_roll_each(x, n = matrix(c(1, 0, 1), nrow = 1), axis = 1)
#      a b c
# [1,] 2 3 6
# [2,] 1 4 5
```

If no axis is left with names, the result has no `dimnames` at all.

### Errors

```r
x <- matrix(1:12, nrow = 3)

rray_roll_each(x, n = c(0, 1, 2, 3), axis = 1)
# Error in `rray_roll_each()`:
# ! Can't broadcast axis 1 of `n` from dimension 4 to 1.

rray_roll_each(x, n = array(1L, c(1, 1, 2)), axis = 1)
# Error in `rray_roll_each()`:
# ! Can't broadcast `n` from dimensionality 3 to 2. Can't decrease
#   dimensionality.

rray_roll_each(x, n = NA, axis = 1)
# Error in `rray_roll_each()`:
# ! `n` must not contain missing values.

rray_roll_each(x, n = 1.5, axis = 1)
# Error: Can't convert from `n` <double> to <integer> due to loss of
#   precision at location 1.

rray_roll_each(x, n = factor("a"), axis = 1)
# Error from `check_unclassed()`

rray_roll_each(x, n = 1, axis = c(1, 2))
# Error in `rray_roll_each()`:
# ! `axis` must be a single integer, not length 2.

rray_roll_each(x, 1, 2)
# Error from `check_dots_empty0()`
```

Only the missing value error is new text. The broadcast errors come from
`check_broadcastable()`, the cast error from `rray_cast()`, and the rest from
existing helpers.

## Equivalences

These describe the functions exactly and make good property tests.

A roll of one axis is a slice of that axis. For dimension `d` and a reduced
shift `k` in `(0, d)`:

```r
rray_roll(x, n = k, axes = a)
rray_slice_axis(x, c((d - k + 1):d, seq_len(d - k)), axis = a)
```

These are identical, names included.

Rolling several axes is rolling them one at a time, in any order:

```r
rray_roll(x, n = c(1, 2), axes = c(1, 3))
rray_roll(rray_roll(x, n = 1, axes = 1), n = 2, axes = 3)
rray_roll(rray_roll(x, n = 2, axes = 3), n = 1, axes = 1)
```

Rolling by `n` and then by `-n` gives back `x`:

```r
rray_roll(rray_roll(x, n = n, axes = a), n = -n, axes = a)  # x
```

A uniform `rray_roll_each()` is `rray_roll()` without the rolled axis names:

```r
rray_roll_each(x, n = 1, axis = a)
rray_set_axis_names(rray_roll(x, n = 1, axes = a), a, NULL)
```

Rolling each lane by `n` and then by `-n` gives back `x` without the rolled
axis names.

## Implementation design

### Files

- `R/roll.R`: both R functions and one shared documentation topic.

- `src/roll.c`, `src/roll.h`, `src/decl/roll-decl.h`: both C entry points,
  following `src/rep.c`, which holds `rray_rep()` and `rray_rep_each()`.

- `src/arg.c`, `src/arg.h`: add an `n` tag with `INIT_ARG(n)`, after `times`.

- `src/init.c`: `extern` declarations and registrations for `ffi_rray_roll()`
  and `ffi_rray_roll_each()`, each with 4 arguments.

- `tests/testthat/test-roll.R` and `tests/testthat/helper-roll.R`.

- `_pkgdown.yml`: a new `Rolling` section after `Repeating`, containing
  `rray-roll`.

### R wrappers

```r
rray_roll <- function(x, ..., n, axes) {
  check_dots_empty0(...)
  .Call(ffi_rray_roll, x, n, axes, environment())
}

rray_roll_each <- function(x, ..., n, axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_roll_each, x, n, axis, environment())
}
```

### Shared shift helper

Both functions reduce a shift into `[0, d)` with one `static inline` helper in
`src/roll.c`:

```c
static inline int rray_roll_shift(int n, int dimension) {
  if (dimension == 0) {
    return 0;
  }

  int out = n % dimension;

  if (out < 0) {
    out += dimension;
  }

  return out;
}
```

`n` is never `NA_integer_` here because missing values are rejected first, so
`n % dimension` can't overflow.

### Validating `n`

The two functions take different shapes of `n`, so each has its own static
helper in `src/roll.c`. They share the missing value check.

```text
arg_as_roll_n(n, axes_size, arg, error_call):
  n = arg_as_bare_integer(n, arg)
  check_roll_n_not_missing(n, arg)
  check that r_length(n) is 1 or axes_size
  return n

arg_as_roll_each_n(n, arg, error_call):
  check_unclassed(n, arg)
  n = arg_as_array(n, arg)
  n = rray_cast(n, integer(), arg)
  check_roll_n_not_missing(n, arg)
  return n
```

`arg_as_roll_n()` validates `n` the way `arg_as_axes_unsorted()` validates
`axes`, through `arg_as_bare_integer()`, so both arguments of `rray_roll()`
follow the same rules.

`arg_as_roll_each_n()` returns an integer array. `rray_cast()` keeps
dimensions, so a matrix of doubles becomes a matrix of integers.

The size errors in `arg_as_roll_n()` are written like `stop_times_size()` in
`src/rep.c`, in a new `stop_roll_n_size()`.

### `rray_roll()` is a slice

A roll of an axis by `k` is a slice of that axis with rotated locations.
`rray_roll()` builds one slice index per axis and calls `rray_slice()`, the
same way `rray_slice_axis()` builds its indices with
`rray_slice_axis_indices()`:

- `TRUE` for every axis not in `axes`.

- The rotated locations for every axis in `axes`, even when the shift is 0.

`rray_slice()` validates the locations again. That costs one pass over the
rolled dimensions and never fails. It also builds the result names from the
same locations, so names roll with no extra code. Every call goes through this
one path. There is no early exit, and the result is always a new array.

```text
check_unclassed(x)
x = KEEP(arg_as_array(x))
dimensionality = rray_dimensionality(x)

axes = KEEP(arg_as_axes_unsorted(axes, dimensionality, rray_args.axes))

n = KEEP(arg_as_roll_n(n, r_length(axes), rray_args.n))

indices = KEEP(list of `dimensionality` elements, each `TRUE`)

for each i in axes:
  axis      = v_axes[i]
  dimension = v_x_dimensions[axis - 1]
  shift     = rray_roll_shift(v_n[n_size == 1 ? 0 : i], dimension)

  indices[[axis]] = rray_roll_locations(dimension, shift)

return rray_slice(x, indices, x_arg, rray_args.empty, error_call)
```

`indices` needs no names. Every location is in bounds, so `rray_slice()` never
reports an error about an index.

A rolled axis always gets its full location vector, even when `x` is empty
because of a zero dimension on another axis. Its names still have to roll, and
`rray_slice()` builds them from those locations. The cost is one integer per
position on the axis, the same as the names themselves.

`rray_roll_locations()` allocates the one-based locations for one axis:

```c
static r_obj* rray_roll_locations(int dimension, int shift) {
  r_obj* out = KEEP(r_alloc_integer(dimension));
  int* v_out = r_int_begin(out);

  for (int j = 0; j < dimension; ++j) {
    v_out[j] = (j >= shift) ? j - shift + 1 : j - shift + dimension + 1;
  }

  FREE(1);
  return out;
}
```

For `dimension = 5` and `shift = 2` this gives `4 5 1 2 3`, which reads
`x[4], x[5], x[1], x[2], x[3]`. For a shift of 0 it gives `1 2 3 4 5`. For a
dimension of 0 it gives `integer()`.

### `rray_roll_each()`

#### The layout

View `x` as three blocks around `axis`:

```text
dimensions: (A, B, C, D),  axis = 3

block_size       = A * B   product of the dimensions before `axis`
axis_dimension   = C
n_groups         = D       product of the dimensions after `axis`

element (i, j, g) is at offset  i + j * block_size + g * block_size * axis_dimension
```

`block_size` and `n_groups` use the same names and meaning as in `src/rep.c`.

A lane is one `(i, g)` pair. Its position in the lane dimensions, in
column-major order, is `i + g * block_size`. That means a lane-dimension array
can be read with that flat index directly.

#### The shell

```text
check_unclassed(x)
x = KEEP(arg_as_array(x))
check_dimensionality()
check_axis(axis, dimensionality)

n = KEEP(arg_as_roll_each_n(n, rray_args.n))

int v_lane_dimensions[RRAY_MAX_DIMENSIONALITY]
copy dim(x) into v_lane_dimensions, with axis set to 1
check_broadcastable(dim(n), v_lane_dimensions, rray_args.n)

out = r_alloc_vector(r_typeof(x), size)
poke r_dim(x) onto out

if size > 0:
  compute block_size and n_groups
  plan = rray_broadcast_iterator_plan(dim(n), v_lane_dimensions)
  shifts = rray_roll_each_shifts(n, &plan, axis_dimension)
  rray_roll_each_fill(x, out, v_shifts, block_size, axis_dimension, n_groups)

names = rray_reduce_names(x, r_int(axis))
poke names onto out if not NULL
```

Notes:

- `n` is fully validated before the size check, with `check_broadcastable()`
  from `src/broadcast.h`. It checks the dimensions without allocating and gives
  the same messages as `rray_broadcast()`. So a bad `n` errors even when `x` is
  empty.

- The lane dimensions are a stack array, since nothing needs them as an R
  object.

- Nothing proportional to the lane count is allocated for an empty `x`. When
  the rolled axis has dimension 0, the lane dimensions can still be huge, for
  example `(100000, 1, 100000)` for `x` with dimensions `(100000, 0, 100000)`.
  The `shifts` table there would be ten billion integers for an empty result.
  This is the only reason for the size check.

- `n` is never broadcast into an R object. Its dimensions only drive the
  iterator plan, and `n` itself is only read. So there is no copy of `n` the
  size of the lane count, only the `shifts` table.

- `shifts` has `size / axis_dimension` elements, which is at most the size of
  `x` and usually much smaller. Reducing once per lane keeps `%` out of the main
  loop.

- `rray_reduce_names()` drops names on the given axes and keeps the rest. It
  returns `NULL` when nothing survives. Its meaning here is exactly the names
  rule for `rray_roll_each()`.

#### The shifts

`rray_roll_each_shifts()` walks the lane dimensions with the broadcast
iterator, reads each lane's value from `n`, and writes its reduced shift. It
follows the loop in `RRAY_BROADCAST_ATOMIC` in `src/broadcast.c`, including the
fixed path for a zero run stride, which reduces a shift once per run instead of
once per lane:

```c
static r_obj* rray_roll_each_shifts(
  r_obj* n,
  const struct rray_strided_iterator_plan* plan,
  int axis_dimension
) {
  const r_ssize size = rray_strided_iterator_plan_size(plan);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  const int* v_n = r_int_cbegin(n);

  r_ssize run_start = 0;
  const r_ssize run_size = rray_strided_iterator_plan_run_size(plan);

  r_ssize n_start = 0;
  const r_ssize n_run_stride = rray_strided_iterator_plan_run_stride(plan);

  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
  rray_strided_iterator_plan_point_init(plan, v_point);

  while (run_start != size) {
    const r_ssize run_end = run_start + run_size;
    r_ssize n_loc = n_start;

    if (n_run_stride == 0) {
      const int shift = rray_roll_shift(v_n[n_loc], axis_dimension);
      for (r_ssize i = run_start; i < run_end; ++i) {
        v_out[i] = shift;
      }
    } else {
      for (r_ssize i = run_start; i < run_end; ++i) {
        v_out[i] = rray_roll_shift(v_n[n_loc], axis_dimension);
        n_loc += n_run_stride;
      }
    }

    run_start = run_end;
    RRAY_STRIDED_ITERATOR_NEXT(n_start, v_point, plan);
  }

  FREE(1);
  return out;
}
```

For a size 1 `n`, the plan coalesces to one run with stride 0, so there is one
`%` for the whole table.

#### The fill

`rray_roll_each_fill()` switches on `r_typeof(x)` to seven cores, like
`rray_rep_fill_varying()`. Each core is one macro call, with
`RRAY_ROLL_EACH_FILL_ATOMIC` for logical, integer, double, complex, and raw,
and `RRAY_ROLL_EACH_FILL_BARRIER` for character and list. Undefine both after
the cores.

The loop walks `out` in column-major order, so every write is sequential:

```c
const CTYPE* v_x = CONST_DEREF(x);
CTYPE* v_out = DEREF(out);

const r_ssize group_size = block_size * axis_dimension;

r_ssize out_i = 0;

for (r_ssize group = 0; group < n_groups; ++group) {
  const CTYPE* v_x_group = v_x + group * group_size;
  const int* v_group_shifts = v_shifts + group * block_size;

  for (int j = 0; j < axis_dimension; ++j) {
    for (r_ssize i = 0; i < block_size; ++i) {
      const int shift = v_group_shifts[i];
      const int source =
        (j >= shift) ? j - shift : j - shift + axis_dimension;

      v_out[out_i] = v_x_group[(r_ssize) source * block_size + i];
      ++out_i;
    }
  }
}
```

The barrier version replaces the assignment with `POKE(out, out_i, ...)`.

Reads stay inside the current group, so they stay close in memory. When `axis`
is 1, `block_size` is 1 and each lane is contiguous. A lane by lane path with
two `memcpy()` calls would be faster there. Add it only if a benchmark against
the general loop shows a real gap.

### File order

Following the C conventions in `CLAUDE.md`, `src/roll.c` reads top down:

1. `ffi_rray_roll()`, `rray_roll()`

2. `arg_as_roll_n()`, `stop_roll_n_size()`

3. `rray_roll_locations()`

4. `ffi_rray_roll_each()`, `rray_roll_each()`

5. `arg_as_roll_each_n()`

6. `rray_roll_each_shifts()`

7. `rray_roll_each_fill()`, then its seven cores and macros

8. `check_roll_n_not_missing()` and `rray_roll_shift()`, last, since both
   halves use them. Declare `rray_roll_shift()` in the decl header as a
   `static inline`, like `axis_is_reduced()` in
   `src/decl/reduce-names-decl.h`.

`src/roll.h` declares `rray_roll()` and `rray_roll_each()`.
`src/decl/roll-decl.h` declares every static helper and core, and is included
last.

### Protection review

Do this as a separate pass over the diff before calling the C done. Points
specific to this plan:

- In both functions, keep `x` right after `arg_as_array()`. For a bare vector
  it returns a new array, and every later step allocates before `x` is last
  used.

- In `rray_roll()`, `axes` and `n` from `arg_as_axes_unsorted()` and
  `arg_as_roll_n()` can be fresh allocations from a cast. Keep both across the
  loop, which allocates.

- Keep `indices` from its allocation until `rray_slice()` returns.

- Poke each result of `rray_roll_locations()` straight into `indices`, with no
  allocation in between. Once poked, `indices` protects it.

- In `arg_as_roll_each_n()`, keep `n` after each of `arg_as_array()` and
  `rray_cast()`.

- In `rray_roll_each()`, keep `n` after `arg_as_roll_each_n()`, since the
  allocation of `out` comes before `rray_roll_each_shifts()` reads it.
  `check_broadcastable()` and `rray_broadcast_iterator_plan()` do not
  allocate.

- Keep `out` across `rray_roll_each_shifts()` and across
  `rray_reduce_names()`. Keep `shifts` until the fill returns.

- `r_int(axis)` passed to `rray_reduce_names()` must be kept, since
  `rray_reduce_names()` allocates before it reads `axes`.

### Documentation

One topic, `rray-roll`, in `R/roll.R`, shaped like `rray-rep` in `R/rep.R`:

- A description bullet for each function, saying one shift per axis versus one
  shift per lane.

- `@details` covering direction, wrapping, broadcasting of `n` in
  `rray_roll_each()`, and the names rule for each function.

- `@param n` with a paragraph per function, `@param axes`, and `@param axis`,
  with a full blank line between each `@param`.

- Examples: the one-dimensional roll, the named matrix with `rray_roll()` on
  each axis and on both, the one shift per row example, the one row matrix for
  per column shifts, and a names comparison between the two functions.

Wrap at 80 characters and re-document afterwards. Run
`pkgdown::check_pkgdown()`.

## Test plan

`tests/testthat/test-roll.R` has two `# ----` sections, `rray_roll()` then
`rray_roll_each()`. No other section headers.

`tests/testthat/helper-roll.R` holds two reference implementations written
with base R only:

- `expected_roll(x, n, axes)` builds rotated locations per axis and slices with
  `[` and `drop = FALSE`.

- `expected_roll_each(x, n, axis)` walks every output point, reads its lane's
  shift from `n` expanded to the lane dimensions, and reads the source
  element. It then removes the names on `axis`.

### `rray_roll()`

- Positive, negative, zero, and larger than dimension shifts on a
  one-dimensional array.

- Every axis of a three-dimensional array, compared with `expected_roll()`.

- Several axes at once, with a size 1 `n` and with one shift per axis.

- `n` pairs with `axes` in the order given, including unsorted `axes`.

- Rolling several axes equals rolling them one at a time.

- Rolling by `n` then `-n` gives back `x`.

- Equals `rray_slice_axis()` with rotated locations, names included.

- All seven types.

- Bare vectors, with and without names.

- Names roll on rolled axes and are untouched elsewhere, including when only
  some axes have names.

- Gives a result identical to `x` for a shift of 0, a multiple of the
  dimension, empty `axes`, and a zero dimension.

- Zero size arrays with a zero dimension on the rolled axis and on another
  axis. With the zero on another axis, names on the rolled axis still roll.

- Integer-ish doubles and logicals for `n` and `axes`.

- `n = integer()` works with `axes = integer()` and is a size error with
  nonempty `axes`.

- Shifts of `2147483647` and `-2147483647`.

- Errors, snapshotted: repeated axis, out of range axis, `n` size mismatch for
  one and several axes, missing `n`, fractional `n`, character `n`, named
  `n`, matrix `n`, classed `x`, and a positional argument in the dots.

### `rray_roll_each()`

- The one shift per row example.

- The one shift per column example with a one row matrix.

- All four three-dimensional cases from the worked examples.

- A size 1 `n` gives the same values as `rray_roll()`.

- Rolling each lane by `n` then `-n` gives back `x` without the rolled axis
  names.

- `n` with fewer axes than `x`, and `n` with every axis.

- Every axis of a three-dimensional array, compared with
  `expected_roll_each()`.

- All seven types.

- Bare vector `x` and bare vector `n`.

- Names: rolled axis names dropped for a size 1 `n` and for a per-lane `n`,
  other axes kept, no `dimnames` when only the rolled axis had names, and names
  on `n` ignored.

- A double matrix `n` is cast and keeps its dimensions.

- Zero size arrays, including a zero dimension on the rolled axis, where `n`
  is still validated.

- A zero rolled dimension with huge lane dimensions, such as `x` with
  dimensions `c(100000, 0, 100000)` and `n = 1`, returns quickly without
  allocating the `shifts` table.

- Shifts of `2147483647` and `-2147483647`.

- `x` is not modified, and `n` is not modified when it already has the lane
  dimensions.

- Errors, snapshotted: `n` not 1 on the rolled axis, `n` too large on another
  axis, `n` with too many axes, missing `n`, fractional `n`, classed `n`,
  classed `x`, `axis` not a single integer, `axis` out of range, and a
  positional argument in the dots.

### Required checks

After each C change:

1. Run the explicit protection review.

2. Run `clang-format -i src/*.c src/*.h`.

3. Run `air format .`.

4. Run `devtools::document()` after roxygen changes.

5. Run focused tests, then all tests.

6. Run `pkgdown::check_pkgdown()` and `devtools::check()` for the final pull
   request.

## Delivery order

Two stacked pull requests.

### 1. `rray_roll()`

Add the `n` argument tag, `R/roll.R` with `rray_roll()` only, `src/roll.c`
with the `rray_roll()` half and the shared helpers, the documentation topic,
the pkgdown section, and the `rray_roll()` tests.

### 2. `rray_roll_each()`

Add `rray_roll_each()` to `R/roll.R` and `src/roll.c`, extend the topic, and
add its tests.

## Research notes

### NumPy

`numpy.roll(a, shift, axis=None)` rolls one or more axes by one shift each.
`shift` and `axis` broadcast against each other, and a repeated axis adds its
shifts. With `axis=None` the array is flattened, rolled, and reshaped.
`rray_roll()` keeps the multi-axis form, requires `axes`, and rejects repeated
axes.

NumPy has no per-lane roll. The usual workaround is fancy indexing with a
computed column index, which is what `rray_roll_each()` does directly.

- [`numpy.roll()`](https://numpy.org/doc/stable/reference/generated/numpy.roll.html)

- [Roll rows of a matrix independently](https://stackoverflow.com/questions/20360675/roll-rows-of-a-matrix-independently)

### xarray

`DataArray.roll(shifts, roll_coords=False)` rolls data by one shift per
dimension. By default the coordinate labels stay in place and the data moves
past them. rray4 instead rolls names with the data in `rray_roll()`, so a
uniform roll matches `rray_slice_axis()` with the same locations.

- [`xarray.DataArray.roll()`](https://docs.xarray.dev/en/stable/generated/xarray.DataArray.roll.html)
