# Multi-axis repeat plan

## Recommendation

Change `rray_rep()` to repeat one or more axes, with one `times` per axis:

```r
rray_rep(x, ..., times, axes)
rray_rep_each(x, ..., times, axis)
```

`rray_rep_each()` does not change.

- `rray_rep()` repeats whole axes. `times` is size 1, used for every axis in
  `axes`, or the size of `axes`, with `times[[i]]` used for `axes[[i]]`. This
  is NumPy's `np.tile()` with explicit axes.

- `rray_rep_each()` repeats each position along one axis. `times` is size 1 or
  the dimension of `axis`.

This makes the rep pair match the roll pair in `plans/roll.md` exactly:

| Every lane together | Each position on its own |
|---|---|
| `rray_rep(x, ..., times, axes)` | `rray_rep_each(x, ..., times, axis)` |
| `rray_roll(x, ..., n, axes)` | `rray_roll_each(x, ..., n, axis)` |

The left column takes `axes` and one value per axis. The right column takes one
`axis` and one value per position or lane.

This is a breaking change to an exported function. `axis` is renamed to `axes`,
with no deprecation path. An old call such as `rray_rep(x, times = 2, axis = 1)`
now fails in `check_dots_empty0()`, since `axis` lands in the dots.

## Why multiple axes are well defined

Repeating along axis 1 and then axis 2 gives the same result as the other order.
Every output position has exactly one source position:

```text
out[i1, ..., iD] = x[i1 mod d1, ..., iD mod dD]    (zero-based)
```

So one `times` per axis has a single meaning, the names rule stays the same,
and there is no ordering question.

## `rray_rep()`

### Arguments

- `x`: an array or bare vector.

- `...`: must be empty. It forces `times` and `axes` to be named.

- `times`: a bare integer vector of values greater than or equal to 0, size 1
  or the size of `axes`. Validated with `arg_as_non_negative_bare_integer()`, as
  today.

- `axes`: a bare integer vector of axes. Required, with no default. Any order.
  An axis can't appear twice. `integer()` is allowed and repeats nothing.
  Validated with `arg_as_axes_unsorted()`, like `from` and `to` in
  `rray_move_axes()` and `axes` in `rray_roll()`.

The result has the type and dimensionality of `x`. Each axis in `axes` has its
dimension multiplied by its `times`. Every other axis keeps its dimension.

### Worked examples

```r
x <- matrix(1:4, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b")))

x
#    a b
# r1 1 3
# r2 2 4
```

One axis, as today:

```r
rray_rep(x, times = 2, axes = 2)
#    a b a b
# r1 1 3 1 3
# r2 2 4 2 4
```

Both axes with a size 1 `times`:

```r
rray_rep(x, times = 2, axes = c(1, 2))
#    a b a b
# r1 1 3 1 3
# r2 2 4 2 4
# r1 1 3 1 3
# r2 2 4 2 4
```

One `times` per axis. `times` pairs with `axes` in the order given, so these two
are identical:

```r
rray_rep(x, times = c(2, 3), axes = c(1, 2))
rray_rep(x, times = c(3, 2), axes = c(2, 1))
#    a b a b a b
# r1 1 3 1 3 1 3
# r2 2 4 2 4 2 4
# r1 1 3 1 3 1 3
# r2 2 4 2 4 2 4
```

A `times` of 0 gives a zero dimension on that axis. The other axes still
repeat:

```r
out <- rray_rep(x, times = c(2, 0), axes = c(1, 2))

dim(out)
# [1] 4 0

rray_names(out)
# [[1]]
# [1] "r1" "r2" "r1" "r2"
#
# [[2]]
# NULL
```

Three dimensions, repeating the first and last axes:

```r
y <- array(1:8, c(2, 2, 2))

out <- rray_rep(y, times = 2, axes = c(1, 3))

dim(out)
# [1] 4 2 4

out[, , 1]
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# [3,]    1    3
# [4,]    2    4

out[, , 3]
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# [3,]    1    3
# [4,]    2    4
```

Nothing repeats. Each of these gives a new array identical to `x`, names
included:

```r
rray_rep(x, times = 1, axes = c(1, 2))
rray_rep(x, times = 2, axes = integer())
rray_rep(x, times = integer(), axes = integer())
```

### Names

Each axis in `axes` repeats its own names `times` times, duplicates and all.
Every other axis keeps its names untouched. This is today's rule applied once
per axis, and is exactly what `rray_slice_axis()` does with the same locations.

As today, names on the `dimnames` list itself are not kept. `rray_slice_axis()`
drops them too, so that is a separate question for all of rray4.

### Relationship to NumPy

| NumPy | rray4 |
|---|---|
| `np.tile(x, (2, 1))` | `rray_rep(x, times = 2, axes = 1)` |
| `np.tile(x, (2, 3))` | `rray_rep(x, times = c(2, 3), axes = c(1, 2))` |
| `np.tile(x, 2)` on a matrix | `rray_rep(x, times = 2, axes = 2)` |
| `np.tile(x, (2, 1, 1))` on a matrix | Not supported. Use `rray_insert_axes()` first |

`np.tile()` lines `reps` up with the trailing axes, and adds new leading axes
when `reps` is longer than the dimensionality. rray4 names every axis
explicitly and never adds axes.

### Errors

```r
x <- matrix(1:6, nrow = 2)

rray_rep(x, times = 2, axes = c(1, 1))
# Error in `rray_rep()`:
# ! `axes` must not contain 1 more than once.

rray_rep(x, times = 2, axes = 3)
# Error in `rray_rep()`:
# ! `axes` must contain values less than or equal to the dimensionality of 2,
#   not 3.

rray_rep(x, times = c(1, 2, 3), axes = c(1, 2))
# Error in `rray_rep()`:
# ! `times` must be size 1 or size 2 to match `axes`, not size 3.

rray_rep(x, times = c(1, 2), axes = 1)
# Error in `rray_rep()`:
# ! `times` must be size 1, not size 2.

rray_rep(x, times = -1, axes = 1)
# Error in `rray_rep()`:
# ! `times` must not contain negative values.

rray_rep(x, times = .Machine$integer.max, axes = 1)
# Error in `rray_rep()`:
# ! The dimension implied by `times` is too large for R.

rray_rep(array(1L, c(1, 1)), times = 2^30, axes = c(1, 2))
# Error in `rray_rep()`:
# ! Size (1.15292e+18) computed from dimensions `(1073741824, 1073741824)` is
#   too large.

rray_rep(x, times = 2, axis = 1)
# Error from `check_dots_empty0()`
```

Only the `times` size error that mentions `axes` is new text. It is written
like the size error in `rray_roll()`, so both functions say the same thing. The
rest comes from existing helpers. Snapshot them rather than retyping them.

## `rray_rep_each()`

No change in behavior. It keeps a single `axis`, for two reasons:

- A varying `times` across several axes would need a list with one vector per
  axis. That is a different argument shape from every other `times` in the
  package.

- NumPy's `np.repeat()` also takes one axis.

Julia's `repeat(x; inner = (2, 2))` does allow a uniform count per axis. The one
common use is upsampling, like turning each pixel into a 2x2 block. Two calls to
`rray_rep_each()` do the same thing.

Its C code does change shape, since it no longer shares an implementation with
`rray_rep()`. See below.

## Equivalences

These describe the function exactly and make good property tests.

Repeating several axes is repeating them one at a time, in any order:

```r
rray_rep(x, times = c(2, 3), axes = c(1, 3))
rray_rep(rray_rep(x, times = 2, axes = 1), times = 3, axes = 3)
rray_rep(rray_rep(x, times = 3, axes = 3), times = 2, axes = 1)
```

Repeating one axis is a slice of that axis, names included:

```r
rray_rep(x, times = t, axes = a)
rray_slice_axis(x, rep(seq_len(d), times = t), axis = a)
```

## Implementation design

### Files

- `R/rep.R`: rename `axis` to `axes` in `rray_rep()`, and update the topic.

- `src/rep.c`, `src/rep.h`, `src/decl/rep-decl.h`: split the shared
  `rray_rep_impl()` into separate `rray_rep()` and `rray_rep_each()` bodies, and
  add the in-place expand.

- `src/init.c`: rename the `ffi_axis` parameter of the `ffi_rray_rep()` extern
  to `ffi_axes`. The registration keeps 4 arguments.

- `tests/testthat/test-rep.R`, `tests/testthat/helper-rep.R`, and the
  regenerated `tests/testthat/_snaps/rep.md`.

- `man/rray-rep.Rd`, regenerated.

No new files, and no new argument tags. `rray_args.axes` and `rray_args.times`
already exist.

### R wrapper

```r
rray_rep <- function(x, ..., times, axes) {
  check_dots_empty0(...)
  .Call(ffi_rray_rep, x, times, axes, environment())
}
```

### C entry points

`ffi_rray_rep()` no longer converts its axis. It passes `ffi_axes` straight
through, and `rray_rep()` validates it after it knows the dimensionality:

```c
r_obj* rray_rep(
  r_obj* x,
  r_obj* times,
  r_obj* axes,
  struct rray_arg* arg,
  struct r_lazy error_call
);
```

`rray_rep_each()` keeps its current signature, with `int axis`.

### `rray_rep()`

```text
check_unclassed(x)
x = KEEP(arg_as_array(x))
dimensions, dimensionality

axes = KEEP(arg_as_axes_unsorted(axes, dimensionality, rray_args.axes))
axes_size = r_length(axes)

times = KEEP(arg_as_rep_times(times, axes_size, rray_args.times))
times_size = r_length(times)

out_dimensions = KEEP(copy of dimensions)
for each i in axes:
  axis = v_axes[i]
  time = v_times[times_size == 1 ? 0 : i]
  v_out_dimensions[axis - 1] = rray_rep_dimension(v_dimensions[axis - 1], time)

out_size = rray_size_from_dimensions_checked(v_out_dimensions, dimensionality)

out = KEEP(r_alloc_vector(r_typeof(x), out_size))
poke out_dimensions onto out

rray_rep_fill(x, out, v_dimensions, dimensionality, v_axes, axes_size,
              v_times, times_size)

names = KEEP(rray_rep_names(r_dim_names(x), v_axes, axes_size, v_times,
                            times_size))
poke names onto out if not NULL
```

Notes:

- Everything is validated before `out` is allocated, so every error fires for
  an empty `x` too.

- `arg_as_axes_unsorted()` calls `check_dimensionality()` itself.

- The fill needs no special case for an empty `out`, the same way the fills in
  `src/rep.c` don't today. A zero `times` or a zero dimension in `x` leaves a
  block count, group count, or block size of 0, and every loop falls through.

### `arg_as_rep_times()`

```text
arg_as_rep_times(times, axes_size, arg, error_call):
  times = arg_as_non_negative_bare_integer(times, arg)
  if r_length(times) is not 1 and not axes_size:
    stop_rep_times_size(r_length(times), axes_size, arg)
  return times
```

`stop_rep_times_size()` has two branches, like `stop_times_size()` today:

- `axes_size == 1`: `` `times` must be size 1, not size 2. ``

- otherwise: `` `times` must be size 1 or size 2 to match `axes`, not size 3. ``

With `axes = integer()`, a size 0 or size 1 `times` is accepted and ignored.

### `rray_rep_dimension()`

Today's `rray_rep_dimension()` handles both a single `times` and a vector. Split
it:

- `rray_rep_dimension(int dimension, int times, error_call)` multiplies with
  the overflow check. Both functions use it.

- `rray_rep_each_dimension(axis_dimension, v_times, times_size, error_call)`
  calls `rray_rep_dimension()` for a size 1 `times`, and otherwise sums with the
  overflow check, as today.

### The fill

The fill has two steps.

#### Step 1: the first axis, read from `x`

The first axis in `axes` is repeated straight from `x` into the start of `out`
with the existing `rray_rep_fill_uniform()`, exactly as single-axis
`rray_rep()` does today:

```text
axis       = v_axes[0]
block_size = product of dimensions 1 through axis, inclusive
n_blocks   = product of dimensions after axis
times      = v_times[0]
```

When `axes` is empty there is no first axis. Use `block_size = size of x`,
`n_blocks = 1`, and `times = 1`, which is a plain copy.

After step 1, `out` holds `x` repeated along the first axis. Its current
dimensions are the dimensions of `x` with the first axis updated. Track these in
a stack array, `int v_current_dimensions[RRAY_MAX_DIMENSIONALITY]`.

For a single axis, step 1 is the whole fill. That keeps single-axis
`rray_rep()` exactly as fast as it is today.

#### Step 2: every other axis, in place

Each remaining axis is repeated in place inside `out`. The data already in
`out` is treated as an array with the current dimensions. For an axis `axis`
with `times`:

```text
block_size = product of current dimensions 1 through axis, inclusive
n_groups   = product of current dimensions after axis
group_size = block_size * times
```

Group `g` currently sits at `g * block_size` and must end up as `times` copies
starting at `g * group_size`. Walk the groups from last to first:

```c
CTYPE* v_out = DEREF(out);

for (r_ssize group = n_groups - 1; group >= 0; --group) {
  const CTYPE* v_source = v_out + group * block_size;
  CTYPE* v_group = v_out + group * group_size;

  for (int time = 0; time < times; ++time) {
    CTYPE* v_target = v_group + time * block_size;

    for (r_ssize i = 0; i < block_size; ++i) {
      v_target[i] = v_source[i];
    }
  }
}
```

Then set the current dimension of `axis` to its output dimension. Skip any axis
with `times` of 1.

Why this is safe:

- Group `g` only writes at or after `g * group_size`. Every group still to be
  read sits before `g * block_size`, which is at most that. So a write never
  hits unread data.

- For `g >= 1` and `times >= 2`, the target region starts at
  `g * block_size * times`, which is at least `(g + 1) * block_size`. So target
  and source never overlap.

- For `g == 0`, the first target is the source itself, so that copy writes
  each element onto itself, which is harmless. The rest start at `block_size`
  or later and don't overlap it.

A target that only partly overlapped its source would corrupt the copy. The two
points above rule that out.

A base R simulation of steps 1 and 2 matched base R subsetting on 3000 random
arrays of up to 4 dimensions, with random zero dimensions, random axis subsets
in random orders, and `times` from 0 to 3.

Total work is at most about twice the size of `out`, since every in-place step
at least doubles the data already written.

#### Types

`rray_rep_expand()` switches on `r_typeof(out)` to seven cores, like
`rray_rep_fill_uniform()`. Each core is one macro call:

- `RRAY_REP_EXPAND_ATOMIC(CTYPE, DEREF)` for logical, integer, double, complex,
  and raw, with the loop above.

- `RRAY_REP_EXPAND_BARRIER(CONST_DEREF, POKE)` for character and list. It reads
  from `CONST_DEREF(out)` and writes with `POKE()` at the matching offset.

Both copy element by element in a loop, like every other fill in `src/rep.c`.
No `r_memcpy()`.

Undefine both after the cores.

`rray_rep_fill()` is the driver that runs step 1 and then calls
`rray_rep_expand()` once per remaining axis.

### Names

```text
rray_rep_names(names, v_axes, axes_size, v_times, times_size):
  if names is NULL:
    return NULL

  if no axis in axes has non-NULL names:
    return names

  out = KEEP(new list with the elements of names)

  for each i in axes:
    axis_names = names[[axis]]
    if axis_names is NULL: next
    poke rray_rep_axis_names(axis_names, time) into out[[axis]]

  return out
```

`rray_rep_axis_names()` is today's non-each branch of the axis names helper: the
whole axis names, `times` times over.

`rray_rep_each_names()` and `rray_rep_each_axis_names()` are today's helpers
with the non-each branches removed and the `each` argument dropped.

### `rray_rep_each()`

Its body is today's `rray_rep_impl()` with `each` fixed to `true` and the
non-each branches removed. Its helpers are renamed so each function has its own
set:

- `arg_as_times()` becomes `arg_as_rep_each_times()`.

- `stop_times_size()` becomes `stop_rep_each_times_size()`, with the same two
  messages as today.

- `rray_rep_each_dimension()`, from the split above.

- `rray_rep_fill_uniform()` for a size 1 `times`, with
  `n_blocks = n_groups * axis_dimension`, and `rray_rep_fill_varying()`
  otherwise, as today.

- `rray_rep_each_names()` and `rray_rep_each_axis_names()`.

`rray_rep_impl()` is removed.

### File order

`src/rep.c` reads top down:

1. `ffi_rray_rep()`, `rray_rep()`

2. `arg_as_rep_times()`, `stop_rep_times_size()`

3. `rray_rep_fill()`, `rray_rep_expand()`, then its seven cores and macros

4. `rray_rep_names()`, `rray_rep_axis_names()`

5. `ffi_rray_rep_each()`, `rray_rep_each()`

6. `arg_as_rep_each_times()`, `stop_rep_each_times_size()`

7. `rray_rep_each_dimension()`

8. `rray_rep_fill_varying()`, then its seven cores and macros

9. `rray_rep_each_names()`, `rray_rep_each_axis_names()`

10. Last, since both halves use them: `rray_rep_dimension()`,
    `stop_dimension_too_large()`, and `rray_rep_fill_uniform()` with its seven
    cores and macros

`src/rep.h` declares `rray_rep()` with the new signature and `rray_rep_each()`
unchanged. `src/decl/rep-decl.h` declares every static helper and core, in the
same order, and drops `rray_rep_impl()`.

### Protection review

Do this as a separate pass over the diff before calling the C done. Points
specific to this plan:

- Keep `x` right after `arg_as_array()`. For a bare vector it returns a new
  array, and every later step allocates before `x` is last used.

- `axes` from `arg_as_axes_unsorted()` and `times` from `arg_as_rep_times()`
  can be fresh allocations from a cast. Keep both until the names are built,
  since `v_axes` and `v_times` are read there, after `out` and
  `out_dimensions` are allocated.

- Keep `out_dimensions` from its allocation until it is poked onto `out`, since
  `out` is allocated in between.

- Keep `out` across `rray_rep_names()`, which allocates.

- In `rray_rep_names()`, keep the new list across every call to
  `rray_rep_axis_names()`. Poke each result straight into the list with no
  allocation in between.

- The fill does not allocate.

### Documentation

Update the `rray-rep` topic in `R/rep.R`:

- The `rray_rep()` description bullet says it repeats whole axes, and can
  repeat several at once with one `times` per axis.

- `@param times` keeps a paragraph per function. For `rray_rep()`: integers
  greater than or equal to 0, size 1 or the size of `axes`.

- A new `@param axes` for `rray_rep()`: the axes to repeat along, in any order,
  with no axis twice.

- `@param axis` now reads "For `rray_rep_each()`, ...".

- `@returns`: the same dimensions as `x`, except along the repeated axes.

- Examples: switch the existing `rray_rep()` calls to `axes`, and add one with
  a size 1 `times` on both axes and one with a `times` per axis.

Full blank line between each `@param`, wrap at 80 characters, re-document, and
run `pkgdown::check_pkgdown()`. The topic name is unchanged, so `_pkgdown.yml`
needs no edit.

## Test plan

`tests/testthat/test-rep.R` keeps its two `# ----` sections.

`tests/testthat/helper-rep.R` splits `expected_rep(x, times, axis, each)` into
two base R references:

- `expected_rep(x, times, axes)` recycles `times` to the size of `axes`, builds
  `rep.int(seq_len(d), times)` locations for each axis in `axes`, and slices
  with `[` and `drop = FALSE`.

- `expected_rep_each(x, times, axis)` is today's `each = TRUE` branch.

### `rray_rep()`

Every existing test, switched to `axes`. The single-axis snapshots for `axis`
are replaced with the `axes` errors below. Then add:

- Several axes with a size 1 `times`, and with one `times` per axis.

- `times` pairs with `axes` in the order given, including unsorted `axes`.

- Repeating several axes equals repeating them one at a time, in both orders.

- One axis equals `rray_slice_axis()` with repeated locations, names included.

- A `times` of 1 on some axes and 2 on others, so step 2 skips an axis.

- A `times` of 0 on one axis and 2 on another.

- `axes = integer()` with `times` of size 0 and size 1 gives a result identical
  to `x`.

- A zero dimension on an axis not in `axes`, with a huge output on the others,
  such as `x` with dimensions `c(0, 100000, 100000)`, `times = 2`, and
  `axes = c(2, 3)`. It returns quickly with dimensions
  `c(0, 200000, 200000)`, which checks that the fill does no work.

- All seven types with two axes, so every expand core runs.

- Names on several repeated axes, on some of them, on an axis not in `axes`,
  and on none.

- The reference loop over the existing shapes, extended to every subset of
  axes in both orders, with `times` of size 1 and one per axis, for named and
  unnamed `x`.

- `x` is not modified.

- Errors, snapshotted: a repeated axis, an axis of 0, an axis past the
  dimensionality, a missing axis, `times` of the wrong size for one axis and
  for several, a nonempty `axes` with `times = integer()`, a dimension that is
  too large, a total size that is too large, and `axis = 1` passed by its old
  name.

### `rray_rep_each()`

Existing tests, unchanged apart from the helper rename. The snapshots must not
change. That confirms the split kept its behavior.

### Required checks

After each C change:

1. Run the explicit protection review.

2. Run `clang-format -i src/*.c src/*.h`.

3. Run `air format .`.

4. Run `devtools::document()`.

5. Run `devtools::test(filter = '^rep')`, then all tests.

6. Run `pkgdown::check_pkgdown()` and `devtools::check()` before the pull
   request.

## Delivery

One pull request, on its own branch from `main`, merged before the first roll
pull request. It is independent of roll, but landing it first makes the pairs
table in `plans/roll.md` true when roll arrives.

## Research notes

- NumPy's `np.tile(A, reps)` repeats whole axes, one count per axis. `reps`
  lines up with the trailing axes, and a `reps` longer than the dimensionality
  adds leading axes.
  [`numpy.tile()`](https://numpy.org/doc/stable/reference/generated/numpy.tile.html)

- NumPy's `np.repeat(a, repeats, axis)` repeats each position along one axis,
  like `rray_rep_each()`.
  [`numpy.repeat()`](https://numpy.org/doc/stable/reference/generated/numpy.repeat.html)

- MATLAB's `repmat(A, r1, ..., rN)` is `np.tile()` with one count per
  dimension.

- Julia's `repeat(A; inner, outer)` has both at once. `outer` is a tile with
  one count per axis. `inner` repeats each position, also with one uniform
  count per axis.
