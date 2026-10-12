# Require axes by name

## Goal

Move `axis` and `axes` behind `...` wherever a bare number could be mistaken
for data:

```r
# Before
rray_sum(x, 2)

# After
rray_sum(x, axes = 2)
```

`rray_add(x, 2)` and `rray_sum(x, 2)` look the same today but mean very
different things. Naming the axis removes that doubt.

## The rule

Require a name when a bare value could be mistaken for something else.

- `rray_sum(x, 2)`: the `2` could be data, so `axes` must be named.

- `rray_axis_names(x, 2)`: the function name says "axis", so no name is needed.

- `rray_rep(x, ..., times, axes)`: two numbers side by side, so both are named.
  This is already the case.

- `rray_slice_axis(x, i, ..., axis)`: `i` and `axis` are both numbers, so
  `axis` is named even though "axis" is in the function name. This is already
  the case.

## What changes

| Function | Before | After |
|---|---|---|
| `rray_sum()`, `rray_prod()`, `rray_mean()`, `rray_all()`, `rray_any()`, `rray_max()`, `rray_min()` | `(x, axes, ..., na_rm = FALSE)` | `(x, ..., axes, na_rm = FALSE)` |
| `rray_locate_max()`, `rray_locate_min()` | `(x, axis, ..., na_rm = FALSE)` | `(x, ..., axis, na_rm = FALSE)` |
| `rray_cumulative_all()`, `rray_cumulative_any()`, `rray_cumulative_max()`, `rray_cumulative_min()`, `rray_cumulative_prod()`, `rray_cumulative_sum()` | `(x, axis)` | `(x, ..., axis)` |
| `rray_reverse()` | `(x, axes)` | `(x, ..., axes)` |
| `rray_unstack()` | `(x, axis)` | `(x, ..., axis)` |

## What stays the same

- Functions with "axis" or "axes" in the name and only one number:
  `rray_axis_names()`, `rray_axis_title()`, `rray_set_axis_names()`,
  `rray_set_axis_title()`, `rray_insert_axes()`, `rray_remove_axes()`,
  `rray_permute_axes()`.

- Functions that already follow the pattern: `rray_move_axes()`, `rray_rep()`,
  `rray_rep_each()`, `rray_roll()`, `rray_roll_each()`, `rray_split()`,
  `rray_slice_axis()`, `rray_slice_assign_axis()`, `rray_combine()`,
  `rray_stack()`.

- Functions without an axis: setters like `rray_set_names()`, subscripts like
  `i` in `rray_extract()` and `rray_slice_rows()`, `dimensions` in
  `rray_broadcast()`, and every binary op like `rray_add(x, y)`.

- Old plans in `plans/`. They record past decisions and are not rewritten.

## Error

Use rlang's `check_dots_empty0()`, like `rray_rep()` and `rray_roll()` already
do. A positional axis then gives:

```
`...` must be empty.
✖ Problematic argument:
• ..1 = 2
ℹ Did you forget to name an argument?
```

No custom error.

## How to implement

Make one commit per function, in the order of the checklist below. Every
commit must pass `devtools::test()` and `devtools::check()` on its own, so a
commit changes the function and every caller of it at once.

For each function:

1. Move `axis` or `axes` behind `...` in the signature. Add
   `check_dots_empty0(...)` as the first line if the function doesn't already
   have it.

2. Document `...` with `@inheritParams rlang::args_dots_empty`, like
   `R/rep.R` and `R/roll.R` do. Put it above every `@param`, with a blank
   line on either side:

   ```r
   #' @inheritParams rlang::args_dots_empty
   #'
   #' @param x An array.
   #'
   #' @param axis A single integer representing the axis to unstack along.
   ```

   `R/reduce.R` and `R/locate.R` already have it in the right place, so
   reduce and locate functions skip this step.

3. Name the axis at every call site of the function:

   - Roxygen examples and prose in `R/`, including other functions' docs.
     For example, `R/cumulative.R` mentions `rray_sum(x, 1L)`.

   - Tests in `tests/testthat/`.

   - `README.Rmd` and `vignettes/`.

   - Test helpers that mirror the signature, such as
     `rray_sum_forced_fallback()` in `tests/testthat/helper-reduce-sum.R` and
     `rray_mean_forced_fallback()` in `tests/testthat/helper-reduce-mean.R`.
     Change their signature in the same commit as the function they mirror.

4. Add a test next to the function's other argument tests:

   ```r
   test_that("`axes` must be named", {
     expect_snapshot(rray_sum(1:3, 1), error = TRUE)
   })
   ```

   Use `axis` instead of `axes` in the test name where that is the argument.

5. Run `Rscript -e "devtools::document()"`, then
   `Rscript -e "devtools::build_readme()"` if `README.Rmd` changed.

6. Run `Rscript -e "testthat::snapshot_accept()"` only after reviewing the
   snapshot diff. Existing snapshots record the calls, so they change too.

7. Run `air format .` and `Rscript -e "devtools::test()"`.

8. Tick the function off in the checklist below in the same commit.

9. Commit with a one sentence message, such as
   ``Require `axes` to be named in `rray_sum()` ``.

All commits land in one pull request from `feature/dots`.

## Checklist

### Reduce

- [x] `rray_sum()`

- [x] `rray_prod()`

- [x] `rray_mean()`

- [x] `rray_all()`

- [x] `rray_any()`

- [x] `rray_max()`

- [ ] `rray_min()`

### Locate

- [ ] `rray_locate_max()`

- [ ] `rray_locate_min()`

### Cumulative

- [ ] `rray_cumulative_all()`

- [ ] `rray_cumulative_any()`

- [ ] `rray_cumulative_max()`

- [ ] `rray_cumulative_min()`

- [ ] `rray_cumulative_prod()`

- [ ] `rray_cumulative_sum()`

### Other

- [ ] `rray_reverse()`

- [ ] `rray_unstack()`
