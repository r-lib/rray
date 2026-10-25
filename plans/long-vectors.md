# Long vector tests

## Goal

Every exported function gets at least one basic test on an array with more
than `.Machine$integer.max` elements. Each test starts with
`skip_if_not_testing_long_vectors()`, so they only run when asked:

```sh
RRAY_TESTING_LONG_VECTORS=true Rscript -e "devtools::test()"
```

A few functions can't handle long vectors today. Those are fixed first, each
with its own test.

## How each test looks

```r
test_that("works with long vectors", {
  skip_if_not_testing_long_vectors()

  x <- array(as.raw(0), c(2^16, 2^15))
  x[length(x)] <- as.raw(1)

  out <- rray_reverse(x, axes = 2)

  expect_identical(dim(out), dim(x))
  expect_identical(out[2^16], as.raw(1))
})
```

- Use dimensions `c(2^16, 2^15)`. That is exactly `2^31` elements, so the last
  element sits at location `2^31`, the first location past
  `.Machine$integer.max`.

- Put one different value (the marker) at a location past
  `.Machine$integer.max`, usually the last one. Then check that the result
  has it in the right place. If any index gets cut down to an `int`, the
  marker shows up in the wrong place or not at all.

- Check a few positions with `[`. Never compare against a full base R result,
  since that doubles the memory.

- Use raw wherever the function accepts it (1 byte per element, 2 GiB). The
  numeric and logical functions don't accept raw, so they use logical (4
  bytes per element, 8 GiB).

- When the output is a different type than the input, keep the peak memory
  low by building the long output from small inputs through broadcasting.

- The test goes in `tests/testthat/test-{name}.R`, after the tests it is most
  like. Name it `"works with long vectors"`, or something more specific when a
  file has more than one.

- Build the marker array inline in each test. There is no shared helper.

- Memory below is the peak for one test. Keep every test at 16 GiB or less.
  The one exception is `rray_cumulative_prod()` at 24 GiB. It takes a single
  input and always returns a double of the same size, so it can't go lower.

## Pull requests

One stacked PR per fix, each with its own test, in the order below. Then one
PR with the rest of the checklist. The wording of each new error message is
proposed in its PR and reviewed in the snapshot.

## Fixes needed first

All but the last were confirmed by running them on a 2^31 element raw array.
Each one ends in rlang's internal error
`Result can't be represented as 'int'`.

1. Plain vectors with no `dim`. `vec_as_array()` in `src/utils.c` turns the
   length into a 1D `dim` with `r_ssize_as_integer()`. A 1D array can't have
   more than `.Machine$integer.max` elements, so this must become a clear
   error. `rray_size()` errors too, rather than falling back to `length()`,
   so every function treats input the same way. It affects every function, through `arg_as_array()`, and also the
   coordinate arrays of `rray_index()`, which go through `vec_as_array()`
   directly.

   ```r
   rray_size(raw(2^31))
   #> Error: Result can't be represented as `int`.
   #> This is an internal error that was detected in the rray package.
   ```

2. `rray_extract()` selecting more than `.Machine$integer.max` values. The
   result is a 1D array, so this can't work. The fix is a clear error, raised
   before the result is allocated. The `r_ssize_as_integer()` call is in
   `rray_extract()` in `src/extract.c`. The test helper
   `rray_subscript_as_list()` in `src/subscript.c` has the same call. A
   subscript that large is valid, so it reports the size with `r_len()`
   instead.

   ```r
   rray_extract(x, TRUE)
   ```

3. `rray_extract_assign()` with a selection of more than
   `.Machine$integer.max` values. This one should work. The size only goes
   through `int` so that it can be passed to `check_broadcastable()`, in
   `rray_extract_assign()` in `src/extract-assign.c`. A selection that large
   can only take a `value` of size 1, so check that directly when the size
   doesn't fit in an `int`.

   ```r
   rray_extract_assign(x, TRUE, as.raw(2))
   ```

4. `rray_slice()` and `rray_slice_assign()` with an index that selects more
   than `.Machine$integer.max` positions on one axis. That axis can't hold
   them, so this must become a clear error. Both functions call
   `r_ssize_as_integer()` on each axis subscript size (`src/slice.c` and
   `src/slice-assign.c`). The `rray_slice_*()` helpers, `rray_rep()`,
   `rray_rep_each()`, `rray_roll()`, and `rray_reverse()` all go through
   `rray_slice()`, but they build their own indices, which always fit.

   ```r
   rray_slice(array(as.raw(0), c(1, 1)), rep(1L, 2^31), 1L)
   ```

5. `rray_locate_max()` and `rray_locate_min()` along a first axis of exactly
   `.Machine$integer.max`. In the `RRAY_LOCATE` loop in `src/locate.c`, the
   `int position` counter steps once past the last element, which overflows
   to `.Machine$integer.max + 1`. Signed overflow is undefined behavior in C,
   even though the value is never used. Make `position` an `r_ssize` and cast
   it when storing. Found by reading the code, not by running it, since the
   result most likely comes out right anyway.

## Checklist

Already done:

- [x] `rray_sum()` in `test-reduce-sum.R` and `rray_mean()` in
  `test-reduce-mean.R` already have long vector tests.

Fixes first, in the order above:

- [x] Fix 1, dimensionless long vectors. Test in `test-size.R`:

  ```r
  expect_snapshot(rray_size(raw(2^31)), error = TRUE)
  ```

  2 GiB.

- [x] Fix 2, `rray_extract()` past the limit. Test in `test-extract.R`:

  ```r
  x <- array(as.raw(0), c(2^16, 2^15))
  expect_snapshot(rray_extract(x, TRUE), error = TRUE)
  ```

  2 GiB.

- [ ] Fix 3, `rray_extract_assign()` past the limit. Test in
  `test-extract.R`:

  ```r
  x <- array(as.raw(0), c(2^16, 2^15))
  out <- rray_extract_assign(x, TRUE, as.raw(2))
  expect_identical(out[c(1, length(out))], as.raw(c(2, 2)))
  ```

  4 GiB.

- [ ] Fix 4, `rray_slice()` and `rray_slice_assign()` with a long index. Test
  in `test-slice.R`:

  ```r
  x <- array(as.raw(0), c(1, 1))
  i <- rep(1L, 2^31)
  expect_snapshot(rray_slice(x, i, 1L), error = TRUE)
  expect_snapshot(rray_slice_assign(x, i, 1L, value = as.raw(1)), error = TRUE)
  ```

  8 GiB, all of it the index.

- [ ] Fix 5, `rray_locate_max()` and `rray_locate_min()` on an
  `.Machine$integer.max` axis. Test in `test-locate.R`:

  ```r
  x <- array(FALSE, c(.Machine$integer.max, 1))
  x[length(x)] <- TRUE
  expect_identical(
    as.vector(rray_locate_max(x, axis = 1)),
    .Machine$integer.max
  )
  ```

  8 GiB. This is one element short of a long vector, but it is the only way
  to reach the overflow.

Then one test per group. Unless a bullet says otherwise, `x` is the raw
marker array from "How each test looks", and `l` is the logical version:

```r
l <- array(FALSE, c(2^16, 2^15))
l[length(l)] <- TRUE
```

- [ ] Size, dimensions, dimensionality: `rray_size()`, `rray_dimensions()`,
  `rray_set_dimensions()`, `rray_dimensions_common()`,
  `rray_dimensionality()`. Tests in `test-size.R`, `test-dimensions.R`,
  `test-dimensionality.R`:

  ```r
  expect_identical(rray_size(x), 2^31)
  expect_identical(rray_dimensions(x), c(65536L, 32768L))
  expect_identical(rray_dimensionality(x), 2L)

  out <- rray_set_dimensions(x, c(2^15, 2^16))
  expect_identical(dim(out), c(32768L, 65536L))
  expect_identical(out[length(out)], as.raw(1))

  y <- array(as.raw(0), c(1, 2^15))
  expect_identical(rray_dimensions_common(x, y), c(65536L, 32768L))
  ```

  2 GiB. No fixes.

- [ ] Names: `rray_names()`, `rray_set_names()`, `rray_axis_names()`,
  `rray_set_axis_names()`, `rray_row_names()`, `rray_set_row_names()`,
  `rray_column_names()`, `rray_set_column_names()`. Test in `test-names.R`.
  Set names with each setter, read them back with each getter, and check that
  `out[length(out)]` is still the marker. The setters return views, so this
  stays at 2 GiB. No fixes.

- [ ] Titles: `rray_titles()`, `rray_set_titles()`, `rray_axis_title()`,
  `rray_set_axis_title()`. Test in `test-titles.R`, same shape as names.
  2 GiB. No fixes.

- [ ] Insert and remove axes: `rray_insert_axes()`, `rray_remove_axes()`.
  Tests in `test-insert-axes.R` and `test-remove-axes.R`:

  ```r
  out <- rray_insert_axes(x, axes = 1)
  expect_identical(dim(out), c(1L, 65536L, 32768L))
  expect_identical(out[length(out)], as.raw(1))
  ```

  And `rray_remove_axes(out, axes = 1)` back to `dim(x)`. Both are views.
  2 GiB. No fixes.

- [ ] Permute and move axes: `rray_permute_axes()`, `rray_move_axes()`.
  Tests in `test-permute-axes.R` and `test-move-axes.R`:

  ```r
  out <- rray_permute_axes(x, c(2, 1))
  expect_identical(dim(out), c(32768L, 65536L))
  expect_identical(out[length(out)], as.raw(1))
  ```

  `rray_move_axes(x, from = 1, to = 2)` gives the same result. 4 GiB. No
  fixes.

- [ ] Broadcast: `rray_broadcast()`, `rray_broadcast_common()`. Test in
  `test-broadcast.R`. A small input broadcast up to a long output:

  ```r
  y <- array(as.raw(0), c(2^16, 1))
  y[2^16] <- as.raw(1)

  out <- rray_broadcast(y, c(2^16, 2^15))
  expect_identical(out[c(1, length(out))], as.raw(c(0, 1)))
  ```

  For `rray_broadcast_common()`, pair `y` with an `array(as.raw(0), c(1,
  2^15))` and check the last element of the first result. 4 GiB. No fixes.

- [ ] Add, subtract, multiply: `rray_add()`, `rray_subtract()`,
  `rray_multiply()`. One test in each `test-arithmetic-{name}.R`, with a long
  input and a scalar:

  ```r
  out <- rray_add(l, 1L)
  expect_identical(out[c(1, length(out))], c(1L, 2L))
  ```

  Then `rray_subtract(l, 1L)` gives `c(-1L, 0L)` and `rray_multiply(l, 2L)`
  gives `c(0L, 2L)`. 16 GiB. No fixes.

- [ ] Divide, exponentiate: `rray_divide()`, `rray_exponentiate()`. Tests in
  `test-arithmetic-divide.R` and `test-arithmetic-exponentiate.R`. The output
  is double, so build it from small inputs to stay at 16 GiB:

  ```r
  a <- array(1, c(2^16, 1))
  b <- array(c(rep(1, 2^15 - 1), 2), c(1, 2^15))

  out <- rray_divide(a, b)
  expect_identical(out[c(1, length(out))], c(1, 0.5))

  out <- rray_exponentiate(b, a)
  expect_identical(out[c(1, length(out))], c(1, 2))
  ```

  Long input reads go through the same loop as `rray_add()`, which covers
  them. No fixes.

- [ ] Comparison: `rray_greater_than()`, `rray_greater_than_or_equal()`,
  `rray_less_than()`, `rray_less_than_or_equal()`. Test in `test-compare.R`:

  ```r
  out <- rray_greater_than(l, FALSE)
  expect_identical(out[c(1, length(out))], c(FALSE, TRUE))
  ```

  One `expect_identical()` per function. 16 GiB. No fixes.

- [ ] Equality: `rray_equal()`, `rray_not_equal()`. Test in `test-equal.R`,
  same shape as comparison with `TRUE` as `y`. 16 GiB. No fixes.

- [ ] Logical: `rray_and()`, `rray_or()`, `rray_xor()`. Test in
  `test-logical.R`, same shape as comparison. 16 GiB. No fixes.

- [ ] Parallel extremum: `rray_pmax()`, `rray_pmin()`. Test in
  `test-extremum.R`:

  ```r
  out <- rray_pmax(l, 0L)
  expect_identical(out[c(1, length(out))], c(0L, 1L))
  ```

  And `rray_pmin(l, 1L)` gives the same. 16 GiB. No fixes.

- [ ] If else: `rray_if_else()`. Test in `test-if-else.R`, with a long
  condition and raw scalars:

  ```r
  out <- rray_if_else(l, as.raw(1), as.raw(0))
  expect_identical(out[c(1, length(out))], as.raw(c(0, 1)))
  ```

  10 GiB. No fixes.

- [ ] Reduce: `rray_prod()`, `rray_max()`, `rray_min()`, `rray_all()`,
  `rray_any()`. Tests in `test-reduce-prod.R`, `test-reduce-extremum.R`,
  `test-reduce-logical.R`. Reduce over one axis, so the marker lands in the
  last output element, and over both axes, so the count passes
  `.Machine$integer.max`:

  ```r
  out <- rray_any(l, axes = 1)
  expect_identical(as.vector(out)[c(1, 2^15)], c(FALSE, TRUE))
  expect_identical(as.vector(rray_any(l, axes = 1:2)), TRUE)
  ```

  `rray_max()` works the same on `l`. `rray_prod()`, `rray_min()`, and
  `rray_all()` use `!l`, so the marker is the only `FALSE`. 8 GiB. No fixes.

- [ ] Cumulative: `rray_cumulative_sum()`, `rray_cumulative_prod()`,
  `rray_cumulative_max()`, `rray_cumulative_min()`, `rray_cumulative_all()`,
  `rray_cumulative_any()`. One test in each `test-cumulative-{name}.R`, along
  axis 2 so each run goes past `.Machine$integer.max`:

  ```r
  x <- array(TRUE, c(2^16, 2^15))
  out <- rray_cumulative_sum(x, axis = 2)
  expect_identical(out[c(1, length(out))], c(1L, 32768L))
  ```

  The other five use the marker array `l` (or `!l`) and check the last
  element. 16 GiB, except `rray_cumulative_prod()` at 24 GiB, because its
  output is double. No fixes.

- [ ] Locate: `rray_locate_max()`, `rray_locate_min()`. Test in
  `test-locate.R`:

  ```r
  out <- rray_locate_max(l, axis = 2)
  expect_identical(as.vector(out)[c(1, 2^16)], c(1L, 32768L))
  ```

  And `rray_locate_min(!l, axis = 2)`. 8 GiB. Needs fix 5 for the separate
  `.Machine$integer.max` axis test.

- [ ] Index: `rray_index()`. Test in `test-index.R`:

  ```r
  expect_identical(as.vector(rray_index(x, 65536L, 32768L)), as.raw(1))
  ```

  2 GiB. Needs fix 1 only for its error path.

- [ ] Extract: `rray_extract()`, `rray_extract_assign()`. Test in
  `test-extract.R`, with a double location and a point:

  ```r
  expect_identical(as.vector(rray_extract(x, 2^31)), as.raw(1))
  expect_identical(as.vector(rray_extract(x, cbind(65536L, 32768L))), as.raw(1))

  out <- rray_extract_assign(x, 2^31, as.raw(2))
  expect_identical(out[length(out)], as.raw(2))
  ```

  4 GiB. Needs fixes 2 and 3.

- [ ] Slice: `rray_slice()`, `rray_slice_axis()`, `rray_slice_rows()`,
  `rray_slice_columns()`. Test in `test-slice.R`:

  ```r
  expect_identical(as.vector(rray_slice(x, 65536L, 32768L)), as.raw(1))
  expect_identical(rray_slice_axis(x, 32768L, axis = 2)[2^16], as.raw(1))
  expect_identical(rray_slice_rows(x, 65536L)[2^15], as.raw(1))
  expect_identical(rray_slice_columns(x, 32768L)[2^16], as.raw(1))
  ```

  2 GiB. Needs fix 4.

- [ ] Slice assign: `rray_slice_assign()`, `rray_slice_assign_axis()`,
  `rray_slice_assign_rows()`, `rray_slice_assign_columns()`. Test in
  `test-slice.R`. Assign `as.raw(2)` at row `2^16`, column `2^15` with each
  one, and check `out[length(out)]`. 4 GiB. Needs fix 4.

- [ ] Combine and stack: `rray_combine()`, `rray_stack()`. Tests in
  `test-combine.R` and `test-stack.R`. Two half size inputs:

  ```r
  a <- array(as.raw(0), c(2^16, 2^14))
  b <- a
  b[length(b)] <- as.raw(1)

  out <- rray_combine(a, b, .axis = 2)
  expect_identical(dim(out), c(65536L, 32768L))
  expect_identical(out[length(out)], as.raw(1))
  ```

  `rray_stack(a, b, .axis = 3)` with dimensions `c(65536L, 16384L, 2L)`.
  4 GiB. No fixes.

- [ ] Split and unstack: `rray_split()`, `rray_unstack()`. Tests in
  `test-split.R` and `test-unstack.R`. Use one extra column, so the second
  piece starts past `.Machine$integer.max`:

  ```r
  x <- array(as.raw(0), c(2^16, 2^15 + 1))
  x[length(x)] <- as.raw(1)

  out <- rray_split(x, axis = 2, dimensions = c(2^15, 1))
  expect_identical(out[[2]][2^16], as.raw(1))
  ```

  For `rray_unstack()`, use the usual `x` along axis 2 and check
  `out[[2^15]][2^16]`. 4 GiB. No fixes.

- [ ] Rep: `rray_rep()`, `rray_rep_each()`. Test in `test-rep.R`. A small
  input repeated up to a long output:

  ```r
  y <- array(as.raw(0), c(2^16, 1))
  y[2^16] <- as.raw(1)

  out <- rray_rep(y, times = 2^15, axes = 2)
  expect_identical(out[c(1, length(out))], as.raw(c(0, 1)))
  ```

  `rray_rep_each(y, times = 2^15, axis = 2)` gives the same. 2 GiB. No
  fixes.

- [ ] Roll and reverse: `rray_roll()`, `rray_roll_each()`, `rray_reverse()`.
  Tests in `test-roll.R` and `test-reverse.R`. Each moves the last column to
  the first, so the marker ends up at `out[2^16]`:

  ```r
  expect_identical(rray_roll(x, n = 1, axes = 2)[2^16], as.raw(1))
  expect_identical(rray_roll_each(x, n = 1, axis = 2)[2^16], as.raw(1))
  expect_identical(rray_reverse(x, axes = 2)[2^16], as.raw(1))
  ```

  4 GiB. `rray_roll_each()` has its own fill loop, separate from
  `rray_slice()`. I read it and it uses `r_ssize` everywhere it needs to. No
  fixes.

## Not in scope

- Exported functions only. `rray_cast()`, `rray_ptype()`, and the other
  internal helpers are covered by the exported functions that call them.

- Running these on CI. They run locally only. GitHub's standard runners have
  16 GiB of memory, which isn't enough for most of these tests.
