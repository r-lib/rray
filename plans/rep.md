# Repeat an array along an axis

## Status

This document is an implementation plan. It does not describe code that is
already present.

The target branch is `feature/rep`, based on `main`.

## Goal

Add two functions:

```r
rray_rep(x, times, axis)
rray_rep_each(x, times, axis)
```

They are the array versions of `vctrs::vec_rep()` and `vctrs::vec_rep_each()`.
vctrs repeats along the size of a vector. rray4 repeats along one axis of an
array, so both gain a required `axis`.

`rray_rep()` repeats the whole axis. `rray_rep_each()` repeats each slice of
the axis.

```r
x <- array(1:6, c(3, 2))

rray_rep(x, 2, axis = 1)      # (6, 2), rows 1,2,3,1,2,3
rray_rep_each(x, 2, axis = 1) # (6, 2), rows 1,1,2,2,3,3
```

For reference, the numpy names:

- `np.repeat(x, n, axis = k)` is `rray_rep_each()`.

- `rray_rep()` has no numpy name. The closest is
  `np.concatenate([x] * n, axis = k)`.

- `np.tile()` is a separate multi axis function. It is not part of this plan,
  see "Out of scope".

## Decision summary

- Both functions take a single `axis`. It is required and has no default, which
  matches `rray_split()`, `rray_remove_axes()`, and `rray_permute_axes()`.

- `axis` must be between 1 and the dimensionality of `x`. Out of range is an
  error. These functions never add an axis, that is what `rray_broadcast()` is
  for.

- `times` comes before `axis`, so `vec_rep()` habits carry over.

- Names repeat alongside the data, so the repeated axis can come back with
  duplicate names. Names belong to a slice, not to a position, and this is what
  `vec_rep()` does.

- Type is preserved. Nothing is cast, and there is no `ptype` argument.

- The copy needs no broadcasting and no strided iterator. It is a loop of
  contiguous block copies, see "Implementation".

## Public behavior

### `rray_rep()`

`rray_rep()` repeats the whole `axis` of `x`, `times` times. `times` is a single
non-missing whole number greater than or equal to 0.

The dimension along `axis` becomes `dimension * times`. Every other axis is
unchanged.

```text
[3, 2] with times = 2 along axis 1 -> [6, 2]
[3, 2] with times = 2 along axis 2 -> [3, 4]
```

```r
x <- array(1:6, c(3, 2))

rray_rep(x, 2, axis = 1)
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6
#> [4,]    1    4
#> [5,]    2    5
#> [6,]    3    6
```

### `rray_rep_each()`

`rray_rep_each()` repeats each slice of `axis` in place. `times` is a vector of
non-missing whole numbers greater than or equal to 0, recycled to the dimension
along `axis`, so it is either size 1 or exactly that dimension.

The dimension along `axis` becomes `sum(times)`. Every other axis is unchanged.

```r
x <- array(1:6, c(3, 2))

rray_rep_each(x, 2, axis = 1)
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    1    4
#> [3,]    2    5
#> [4,]    2    5
#> [5,]    3    6
#> [6,]    3    6

rray_rep_each(x, c(1, 2, 3), axis = 1)
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    2    5
#> [4,]    3    6
#> [5,]    3    6
#> [6,]    3    6
```

With `times` of size 1 the two functions differ only in the order of the
result. `rray_rep()` gives `1,2,3,1,2,3` and `rray_rep_each()` gives
`1,1,2,2,3,3`.

### Names

Names repeat with the data. The repeated axis keeps its names in the repeated
order, and every other axis keeps its names untouched.

```r
x <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))

rray_names(rray_rep(x, 2, axis = 1))
#> [[1]]
#> [1] "a" "b" "c" "a" "b" "c"
#>
#> [[2]]
#> [1] "x" "y"

rray_names(rray_rep_each(x, 2, axis = 1))
#> [[1]]
#> [1] "a" "a" "b" "b" "c" "c"
#>
#> [[2]]
#> [1] "x" "y"
```

Duplicate names are expected and are not deduplicated or repaired.

If `x` has no names at all, the result has none. If `x` has names but the
repeated axis has `NULL` names, that axis stays `NULL` and the other axes keep
their names.

### Type

The result has the same type as `x`. All seven vector types are supported:
logical, integer, double, complex, character, raw, and list.

Classed input is rejected by `check_unclassed()`, as everywhere else in the
package.

### Shape of the result

A bare vector is treated as a 1D array and comes back as a 1D array, which is
what the rest of the package does.

```r
rray_rep(1:3, 2, axis = 1)
#>  int [1:6(1d)] 1 2 3 1 2 3
```

### Zero and empty cases

`times = 0` is allowed and gives a dimension of 0 along `axis`.

```text
[3, 2] with times = 0 along axis 1 -> [0, 2]
```

If the dimension along `axis` is already 0, `times` must be size 0 or size 1 to
recycle, and the result keeps a dimension of 0.

For `rray_rep_each()`, individual zeros drop individual slices.

```r
rray_rep_each(array(1:3, 3), c(1, 0, 2), axis = 1)
#>  int [1:3(1d)] 1 3 3
```

### Errors

All errors are thrown with the caller's `error_call`, using the existing helpers
where they exist.

- `x` is classed, or is a scalar. From `check_unclassed()` and
  `arg_as_array()`.

- `axis` is not a single integer. From `arg_as_int()`.

- `axis` is missing, less than 1, or greater than the dimensionality. From
  `check_axis()`, which already produces `` `axis` must be less than or equal
  to the dimensionality of 2, not 3. ``

- `times` cannot be cast to integer, or has attributes. New, modelled on
  `arg_as_axes()`.

- `times` contains a missing value:
  `` `times` must not contain missing values. ``

- `times` contains a negative value:
  `` `times` must contain values greater than or equal to 0, not -1. ``

- `times` is the wrong size for `rray_rep()`:
  `` `times` must be size 1, not size 2. ``

- `times` is the wrong size for `rray_rep_each()`:
  `` `times` must be size 1 or the dimension of `axis`, 3, not size 2. ``

- The result is too large, either because the new dimension overflows an `int`
  or because the total size overflows an `r_ssize`:
  `` The result is too large. ``

## Implementation

### The copy is a block copy

Take an array with dimensions `d` and a chosen `axis` `k`. Because the layout is
column major, every element index splits into three parts:

```text
inner  = prod(d[1..k-1])
middle = d[k]
outer  = prod(d[k+1..n])

index  = i + inner * (m + middle * o)
```

So `x` is `(inner, middle, outer)` as far as this operation is concerned, and a
fixed `(m, o)` names a run of `inner` contiguous elements.

That makes both functions loops of contiguous block copies, with no
broadcasting and no strided iterator:

- `rray_rep()` copies a block of `inner * middle` elements, `times` times, for
  each of the `outer` positions.

- `rray_rep_each()` copies a block of `inner` elements, `times[m]` times, for
  each `(m, o)` pair.

In both cases the writes into `out` run strictly front to back, so the output
position is a single counter that advances by the block size. There is no
output index arithmetic at all.

### One kernel for both functions

`rray_rep()` is `rray_rep_each()` on a collapsed view. Fold `middle` into
`inner` and the loop over `m` disappears:

| | `inner` | `middle` | `times` |
|---|---|---|---|
| `rray_rep()` | `inner * middle` | `1` | the single `times`, as size 1 |
| `rray_rep_each()` | `inner` | `middle` | `times` recycled to size `middle` |

So both entry points prepare those three values and hand off to one kernel:

```c
static void rray_rep_copy(
  r_obj* x,
  r_obj* out,
  r_ssize inner,
  r_ssize middle,
  r_ssize outer,
  const int* v_times
);
```

Recycling `times` up front means the kernel can always read `v_times[m]`,
without a size 1 branch or a stride of 0. For `rray_rep()` the recycled vector
is size 1, so this costs nothing.

### Type dispatch

`rray_rep_copy()` is an exhaustive switch over the seven vector types, ending in
`r_stop_unreachable()`, matching `rray_split()`.

Two macros back it:

- `RRAY_REP_ATOMIC(CTYPE, CONST_DEREF, DEREF)` for logical, integer, double,
  complex, and raw. The inner copy is one `r_memcpy()` per block.

- `RRAY_REP_BARRIER(CONST_DEREF, POKE)` for character and list, which cannot be
  `memcpy()`d. The inner copy is an element loop using `r_chr_poke()` or
  `r_list_poke()`.

The atomic body:

```c
r_ssize out_i = 0;

for (r_ssize o = 0; o < outer; ++o) {
  for (r_ssize m = 0; m < middle; ++m) {
    const CTYPE* v_block = v_x + (o * middle + m) * inner;
    const int n = v_times[m];

    for (int j = 0; j < n; ++j) {
      r_memcpy(v_out + out_i, v_block, sizeof(CTYPE) * (size_t) inner);
      out_i += inner;
    }
  }
}
```

### Names

Names are a static helper in `rep.c`, not a separate file. There is not enough
here to justify the split that `split-names.c` and `broadcast-names.c` use.

```c
static r_obj* rray_rep_names(
  r_obj* x_names,
  int axis,
  r_ssize dimension,
  const int* v_times
);
```

If `x_names` is `r_null`, return `r_null`. Otherwise shallow duplicate the list,
and if the element at `axis` is non `NULL`, replace it with a character vector
built by the same `times` walk the data uses. Every other element is carried
over as is.

The `times` walk is shared with the kernel by construction, not by code, since
the names version always steps one name at a time while the data version steps
`inner` elements at a time. Building the names with the recycled `times` vector
keeps them in step.

### Sizes and overflow

Two checks:

- The new dimension along `axis` is `sum(times)` for `rray_rep_each()` and
  `dimension * times` for `rray_rep()`. Both must fit in an `int`, since
  dimensions are integer.

- The total size is the product of the output dimensions and must fit in an
  `r_ssize`.

`rray_size_from_dimensions()` in `size.c` multiplies without checking. Add a
checked sibling next to it:

```c
r_ssize rray_size_from_dimensions_checked(
  const int* v_dimensions,
  int dimensionality,
  struct r_lazy error_call
);
```

Note that `feature/combine` adds the same guarded multiply as a static
`rray_combine_size()` in `combine.c`. Whichever branch lands second should drop
its copy and call this one.

### Files

- `R/rep.R`, one roxygen block with `@name rep` covering both functions,
  following `R/reduce.R`.

- `src/rep.c`, `src/rep.h`, `src/decl/rep-decl.h`.

- `src/size.c` and `src/size.h` for the checked size helper.

- `src/arg.c` and `src/arg.h` to add `times` to `struct rray_args` with
  `INIT_ARG(times)`.

- `src/init.c` for the two `extern` declarations and the two table entries,
  both with 4 arguments.

- `tests/testthat/test-rep.R`.

- `_pkgdown.yml`, adding `rep` to the Manipulation section.

### C layout

`src/rep.c` reads top down:

```text
ffi_rray_rep()
rray_rep()
ffi_rray_rep_each()
rray_rep_each()
rray_rep_copy()
rray_rep_lgl() and the rest of the family
arg_as_times()
rray_rep_names()
```

The R layer is two thin wrappers:

```r
rray_rep <- function(x, times, axis) {
  .Call(ffi_rray_rep, x, times, axis, environment())
}
```

## Testing

`tests/testthat/test-rep.R`, with the two functions grouped under `# ----`
headers.

Cover, for each function:

- Every axis of a 3D array, so that `inner`, `middle`, and `outer` are each
  non trivial and each get to be the degenerate one.

- A bare vector, and confirm the 1D array result.

- All seven types, so both macros and every switch branch run.

- `times = 0`, and for `rray_rep_each()` a `times` with zeros mixed in.

- An input with a dimension of 0 along `axis`.

- `times` of size 1 recycling in `rray_rep_each()`.

- Names on the repeated axis, on another axis, on neither, and on an array
  where only one axis is named.

- Double `times` such as `2` rather than `2L`.

Errors get `expect_snapshot(error = TRUE)`, one per bullet in the "Errors"
section.

The ordering rule is worth pinning directly, since it is the only thing that
separates the two functions when `times` is size 1:

```r
test_that("`rray_rep()` repeats the axis, `rray_rep_each()` repeats slices", {
  x <- array(1:3, 3)
  expect_identical(rray_rep(x, 2, axis = 1), array(c(1L, 2L, 3L, 1L, 2L, 3L), 6))
  expect_identical(
    rray_rep_each(x, 2, axis = 1),
    array(c(1L, 1L, 2L, 2L, 3L, 3L), 6)
  )
})
```

## Out of scope

`rray_tile()`, the multi axis form, stays as sketched in
`plans/implementation.md`. It is `rray_rep()` applied to every axis at once,
and it is the only one of the family that can grow dimensionality. Revisit it
once `rray_rep()` is in and the multi axis case has actually come up.

`vec_unrep()` has no array version here. Along an axis it would have to compare
whole slices for equality, which is a different feature with its own design
questions.
