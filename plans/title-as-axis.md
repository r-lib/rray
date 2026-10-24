# Titles as axes

## Goal

Let every argument that names an existing axis of `x` take a title as well as
a position.

```r
x <- array(
  1:24,
  dim = c(2, 3, 4),
  dimnames = list(row = NULL, col = NULL, page = NULL)
)

rray_sum(x, axes = "row")
# Same as
rray_sum(x, axes = 1)

rray_sum(x, axes = c("page", "row"))
# Same as
rray_sum(x, axes = c(1, 3))
```

It should feel like a native part of the package: same rules, same error
messages, and the same docs wording everywhere it applies.

## Decisions

- A title matches the first axis with that title, like `match()`. Duplicate
  titles on `x` are never an error.

- `""` and `NA` never match an axis. They are errors.

- Before any title work, `axes` arguments drop the "strictly increasing" rule
  for integers. Any order is fine, duplicates are not. The helper hands back
  the axes sorted, so callers still see increasing axes. This lands first, on
  its own.

- Arguments that name a position in the output stay integer only:
  `rray_stack(.axis)`, `rray_insert_axes(axes)`, and both `from` and `to` of
  `rray_move_axes()`.

- `rray_combine(.axis)` takes a title, resolved against the common titles of
  the inputs.

- `rray_axis_names()`, `rray_set_axis_names()`, and `rray_set_axis_title()`
  take a title. `rray_axis_title()` does not, since the lookup would be
  circular.

- Docs: every `@param axis` / `@param axes` says it takes a title, and links to
  one new FAQ topic that explains positions and titles with an example.

## How a title resolves

The rules are the same for `axis` and `axes`. `axis` is the one element case.

- A numeric `axis` or `axes` behaves exactly as it does today (after Part 1).

- A character `axis` or `axes` is looked up in `rray_titles(x)`. Each string
  becomes the position of the first axis with that title.

- Numbers and titles can't mix, since R can't hold both in one atomic vector.

- After lookup, the positions go through the same checks as integers. So
  `axes = c("row", "row")` is a duplicate error, just like `axes = c(1, 1)`.

- The result is sorted, the same as for integers. `axes = c("page", "row")`
  comes back as `c(1L, 3L)`.

- A character vector with attributes is an error, the same as a numeric one
  today. This also rules out factors.

### Errors

Proposed wording. All are thrown with the caller's `error_call`.

```r
rray_sum(x, axes = "nope")
# Error in `rray_sum()`:
# ! Can't find an axis of `x` titled "nope".

rray_sum(matrix(1:4, 2), axes = "row")
# Error in `rray_sum()`:
# ! Can't use a title in `axes` because `x` has no titles.

rray_sum(x, axes = c("row", NA))
# Error in `rray_sum()`:
# ! `axes` must not contain missing values.

rray_sum(x, axes = "")
# Error in `rray_sum()`:
# ! `axes` can't contain empty strings.

rray_sum(x, axes = c("row", "row"))
# Error in `rray_sum()`:
# ! `axes` must not contain "row" more than once.

rray_sum(x, axes = list(1))
# Error in `rray_sum()`:
# ! `axes` must be an integer, double, or character vector, not a list.

rray_cumulative_sum(x, axis = c("row", "col"))
# Error in `rray_cumulative_sum()`:
# ! `axis` must be a single integer or string, not length 2.
```

Integer only arguments keep the error they give today:

```r
rray_stack(x, x, .axis = "row")
# Error in `rray_stack()`:
# ! `.axis` must be an integer or double vector, not a string.
```

## Where resolution happens

Today the FFI wrapper turns `axis` into an `int` with `arg_as_int()`, and the
internal function checks the range with `check_axis()` once it knows the
dimensionality. A title needs `x`, so it can't be resolved in the wrapper.

The rule: resolve where `check_axis()` or `arg_as_axes()` runs today.

- Internal functions that take `int axis` switch to `r_obj* axis`.

- FFI wrappers stop calling `arg_as_int()` and pass `ffi_axis` through.

- Internal C callers that already hold a valid `int` wrap it with `r_int()`.
  These are `rray_stack()` calling `rray_combine()` and
  `rray_set_axis_names()`, and `rray_unstack()` calling `rray_split()` and
  `rray_axis_names()`.

This matches the `axes` functions, which already pass `r_obj* axes` down and
resolve inside.

One side effect: a bad `axis` type used to error before a bad `x`. After this,
`x` errors first. Some snapshots will move. That is expected and fine.

---

# Part 1: Relax integer `axes` ordering

## Shared helpers

File: `src/axes.c`, `src/axes.h`.

- Replace `check_axes()` with `axes_normalize()`, which checks and returns
  sorted axes. Any order in, no duplicates, no missing values, in range.
  Increasing order out.

  ```c
  r_obj* axes_normalize(
    r_obj* axes,
    int dimensionality,
    struct rray_arg* arg,
    struct r_lazy error_call
  );
  ```

- Sort with the seen mask the duplicate check already needs: fill
  `bool v_seen[RRAY_MAX_DIMENSIONALITY]`, then emit every seen axis in order.
  No `qsort()`. If the input is already increasing, return it as is and skip
  the allocation.

- `arg_as_axes()` becomes `arg_as_bare_integer()` plus `axes_normalize()`.

- `arg_as_axes_unsorted()` stays, for callers that pair `axes` with another
  argument by position. Those are `rray_move_axes()` (`from` with `to`),
  `rray_rep()` (`axes` with `times`), and `rray_roll()` (`axes` with `n`).

- Use one duplicate message across `axes_normalize()`,
  `arg_as_axes_unsorted()`, and `arg_as_axes_permutation()`: `` `axes` must
  not contain axis 2 more than once. `` Today two of them leave out the word
  "axis".

## Reduce

Functions: `rray_sum()`, `rray_prod()`, `rray_mean()`, `rray_all()`,
`rray_any()`, `rray_max()`, `rray_min()`.

Files: `src/reduce.c`, `src/reduce-names.c`.

Changes: none. `arg_as_axes()` hands back sorted axes.

Tests (`test-reduce-sum.R` as the main one, one case each in the others):

- `axes = c(2, 1)` gives the same result as `axes = c(1, 2)`, names and
  titles included.

- Snapshot the duplicate error, `axes = c(1, 1)`.

- Remove the "strictly increasing" snapshots.

## Rep and roll

Functions: `rray_rep()`, `rray_roll()`.

Files: `src/rep.c`, `src/roll.c`.

Changes: `times` and `n` pair with `axes` by position, so sorting `axes` alone
would break the pairing. Switch both to `arg_as_axes_unsorted()`. Neither
function needs sorted axes, since each axis is handled on its own.

Tests (`test-rep.R`, `test-roll.R`):

- `rray_rep(x, times = c(2, 3), axes = c(2, 1))` equals
  `rray_rep(x, times = c(3, 2), axes = c(1, 2))`.

- Same pattern for `rray_roll()` with `n`.

## Reverse

Function: `rray_reverse()`. File: `src/reverse.c`.

Changes: none.

Tests (`test-reverse.R`): `axes = c(2, 1)` equals `axes = c(1, 2)`.

## Remove axes

Function: `rray_remove_axes()`. File: `src/remove-axes.c`.

Changes: none. The loop walks `axes` in step with the axes of `x`, and
`arg_as_axes()` hands them back sorted.

Tests (`test-remove-axes.R`): `axes = c(3, 1)` equals `axes = c(1, 3)`, names
and titles included.

## Insert axes

Function: `rray_insert_axes()`. File: `src/insert-axes.c`.

Changes: swap `check_axes()` for `axes = KEEP(axes_normalize(...))`, since it
now returns a new vector. The loop is unchanged. `axes` here are output
positions, so they stay integer only in Part 2.

Tests (`test-insert-axes.R`): `axes = c(3, 1)` equals `axes = c(1, 3)`.

## Docs

Remove "in increasing order" (or similar) from each `@param axes`. No example
is added.

---

# Part 2: Titles

## Shared helpers

Files: `src/axes.c`, `src/axes.h`, `src/decl/axes-decl.h`.

New and changed functions:

```c
int arg_as_axis(
  r_obj* axis,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

r_obj* arg_as_axes(
  r_obj* axes,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

r_obj* arg_as_axes_unsorted(
  r_obj* axes,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);

r_obj* arg_as_axes_unsorted_integer(
  r_obj* axes,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
);

r_obj* arg_as_axes_permutation(
  r_obj* axes,
  r_obj* titles,
  int dimensionality,
  struct rray_arg* arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
);
```

`rray_move_axes()` must still give today's type error for a character `from`
or `to`, not "`x` has no titles". So the numeric only path stays reachable on
its own: `arg_as_axes_unsorted_integer()` is public in `src/axes.h` and
`rray_move_axes()` calls it.

- `titles` is `rray_titles(x)`, which may be `r_null`.

- `x_arg` names `x` in the "can't find" and "no titles" errors.

- For a single `axis`, `NA` and `""` use scalar wording: `` `axis` must not be
  missing. `` and `` `axis` can't be an empty string. ``

- Until PR 4, reverse, remove axes, rep, roll, and permute axes pass `r_null`
  titles. A character `axes` there errors with "`x` has no titles".

- A numeric input goes through the existing path. `arg_as_axis()` is
  `arg_as_int()` plus `check_axis()`.

- A character input goes through one shared lookup,
  `axes_from_titles()`, which returns an integer vector of positions. Then the
  existing checks run on that vector: `axes_normalize()` for `arg_as_axes()`,
  so titles come back sorted too, and the unsorted or permutation checks for
  the other two.

- The duplicate error names the string when the input was a title, and the
  position when it was a number. The lookup keeps the input around for this.

- `check_axis()` stays for integer only callers.

Tests: there is no R entry point for these helpers, so they are tested through
`rray_sum()` (for `axes`) and `rray_cumulative_sum()` (for `axis`). Every error
above gets a snapshot there once. Other groups only test the happy path, plus
one snapshot to prove the argument name is right.

## Reduce

Functions: `rray_sum()`, `rray_prod()`, `rray_mean()`, `rray_all()`,
`rray_any()`, `rray_max()`, `rray_min()`.

File: `src/reduce.c`. `rray_reduce()` passes `rray_titles(x)` to
`arg_as_axes()`.

Tests (`test-reduce-sum.R` holds the full error set):

- `axes = "row"` equals `axes = 1`.

- `axes = c("page", "row")` equals `axes = c(3, 1)`.

- Duplicate titles on `x`: the first match wins.

- Every error in the "Errors" section.

- One happy path test in each other reduce test file.

## Locate

Functions: `rray_locate_max()`, `rray_locate_min()`. File: `src/locate.c`.

`rray_locate()` takes `r_obj* axis` and resolves with `arg_as_axis()`.

Tests (`test-locate.R`): `axis = "col"` equals `axis = 2`, and the not found
snapshot.

## Cumulative

Functions: `rray_cumulative_sum()`, `rray_cumulative_prod()`,
`rray_cumulative_max()`, `rray_cumulative_min()`, `rray_cumulative_all()`,
`rray_cumulative_any()`.

Files: `src/cumulative.c` and the four `src/cumulative-*.c` wrappers.

`rray_cumulative()` takes `r_obj* axis` and resolves with `arg_as_axis()`. The
six wrappers stop calling `arg_as_int()`.

Tests (`test-cumulative-sum.R` holds the `axis` error set):

- `axis = "col"` equals `axis = 2`.

- The `axis` errors: not found, no titles, `NA`, `""`, length 2, wrong type.

- One happy path test in each other cumulative test file.

## Rep and roll

Functions: `rray_rep()`, `rray_rep_each()`, `rray_roll()`, `rray_roll_each()`.

Files: `src/rep.c`, `src/rep-each.c`, `src/roll.c`, `src/roll-each.c`.

- `rray_rep()` and `rray_roll()` pass titles to `arg_as_axes_unsorted()`.

- `rray_rep_each()` and `rray_roll_each()` take `r_obj* axis` and use
  `arg_as_axis()`.

Tests (`test-rep.R`, `test-roll.R`):

- `rray_rep(x, times = c(2, 3), axes = c("col", "row"))` pairs `times` with
  the titles in the order given.

- Same for `rray_roll()` with `n`.

- `axis = "row"` for the `_each()` variants.

## Reverse

Function: `rray_reverse()`. File: `src/reverse.c`.

Tests (`test-reverse.R`): `axes = "col"` equals `axes = 2`.

## Remove axes

Function: `rray_remove_axes()`. File: `src/remove-axes.c`.

Tests (`test-remove-axes.R`):

- `axes = "page"` equals `axes = 3`.

- The "dimension is not 1" error still reports the position. Snapshot it with
  a title input to confirm the message reads well.

## Permute axes

Function: `rray_permute_axes()`. File: `src/permute-axes.c`.

`axes` lists existing axes of `x` in their new order, so titles fit.
`arg_as_axes_permutation()` gets `titles`.

Tests (`test-permute-axes.R`):

- `axes = c("col", "row")` transposes.

- A title permutation with a missing title errors with the length message, the
  same as integers.

## Slice axis

Functions: `rray_slice_axis()`, `rray_slice_assign_axis()`.

File: `src/slice-axis.c`. `rray_slice_rows()` and friends pass a fixed
position, so they wrap it with `r_int()` or call a lower level that keeps
`int`. Pick whichever keeps the file simplest when doing the work.

Tests (`test-slice.R`):

- `rray_slice_axis(x, 1, axis = "col")` equals `axis = 2`.

- Same for `rray_slice_assign_axis()`.

- `i` as names and `axis` as a title at once, to show they don't mix up.

## Split and unstack

Functions: `rray_split()`, `rray_unstack()`.

Files: `src/split.c`, `src/unstack.c`.

Both take `r_obj* axis`. `rray_unstack()` resolves once, then passes
`r_int(axis)` to `rray_split()` and `rray_axis_names()`.

Tests (`test-split.R`, `test-unstack.R`): `axis = "col"` equals `axis = 2`.

## Combine

Function: `rray_combine()`. File: `src/combine.c`, plus
`src/broadcast-names.c`.

`.axis` can be one past the dimensionality of some inputs, and titles must
agree across inputs. A title resolves against the common titles of `xs`.

- Add `rray_titles_common(xs, dimensionality, arg, error_call)` to
  `src/broadcast-names.c`. It shares the title merge loop with
  `rray_broadcast_names_merge()`, so the incompatible titles error stays the
  same. Split that loop into its own helper rather than copy it.

- `rray_combine()` takes `r_obj* axis`. After `list_max_dimensionality()`, a
  character `.axis` computes the common titles and resolves against them. A
  numeric `.axis` skips this, so the integer path does no extra work.

- `rray_stack()` passes `r_int(axis)`.

Tests (`test-combine.R`):

- `.axis = "col"` equals `.axis = 2`.

- Inputs where only the second one has titles still resolve.

- Inputs with clashing titles error with the existing incompatible titles
  message, not "can't find".

- `.axis = "nope"` snapshot.

## Names

Functions: `rray_axis_names()`, `rray_set_axis_names()`. File: `src/names.c`.

`rray_row_names()`, `rray_column_names()`, and their setters use fixed
positions and don't change.

Tests (`test-names.R`): `axis = "col"` for both, and the not found snapshot.

## Titles

Function: `rray_set_axis_title()`. File: `src/titles.c`.

`rray_set_axis_title(x, "row", "Row")` renames a title.
`rray_set_axis_title(x, "row", NULL)` clears it.

`rray_axis_title()` stays integer only.

Tests (`test-titles.R`):

- Rename by title.

- Clear by title.

- `rray_axis_title(x, "row")` errors with today's integer type message.

## Integer only

Functions: `rray_stack()`, `rray_insert_axes()`, `rray_move_axes()`,
`rray_axis_title()`.

No code changes. One snapshot each in their test files showing a character
input errors, so a later change can't add title support by accident.

## Documentation

- New FAQ topic, `faq-axes`, "How do I refer to an axis?", in `R/faq.R` with
  `man/faq/axes.Rmd`. It shows a position, a title, and an output position
  argument side by side. Add it to the FAQ section of `_pkgdown.yml`.

- Every `@param axis` that takes a title reads: "The axis to `<verb>` along,
  given [by position or by title][faq-axes]."

- Every `@param axes` that takes titles reads: "The axes to `<verb>` along,
  given [by position or by title][faq-axes]."

- Integer only params read: "The axis to `<verb>`, given
  [by position in the output][faq-axes]." `from` in `rray_move_axes()` names
  an axis of `x`, so it reads "given [by position][faq-axes]" instead.

- One title example per help topic, next to the integer one.

---

# Checklist

Each top level item is one PR. Each PR runs `clang-format`, `air format .`,
`devtools::document()`, `devtools::test()`, and the protection reading pass.

- [x] PR 1: Relax integer `axes` ordering.

  - [x] `axes_normalize()` in place of `check_axes()`, returning sorted axes.
    Build `arg_as_axes()` on it. Unify the duplicate message.

  - [x] `rray_insert_axes()` keeps the result of `axes_normalize()`.

  - [x] `rray_rep()` and `rray_roll()` switch to `arg_as_axes_unsorted()`.

  - [x] Tests for reduce, rep, roll, reverse, remove axes, insert axes, move
    axes.

  - [x] Remove ordering wording from `@param axes` docs, if any.

- [x] PR 2: Title lookup helpers, used by reduce and cumulative.

  - [x] `arg_as_axis()`, `axes_from_titles()`, title aware `arg_as_axes()`,
    `arg_as_axes_unsorted()`, and `arg_as_axes_permutation()`. Point
    `rray_move_axes()` at the integer only variant. Add
    `src/decl/axes-decl.h`.

  - [x] Reduce: pass titles in `rray_reduce()`. Full `axes` error snapshots in
    `test-reduce-sum.R`.

  - [x] Cumulative: `r_obj* axis` in `rray_cumulative()`, drop `arg_as_int()`
    from the six wrappers. Full `axis` error snapshots in
    `test-cumulative-sum.R`.

  - [x] Add `faq-axes` and update the reduce and cumulative docs.

- [x] PR 3: Titles for one axis functions.

  - [x] Locate.

  - [x] Rep each and roll each.

  - [x] Slice axis and slice assign axis.

  - [x] Split and unstack.

  - [x] Names: `rray_axis_names()`, `rray_set_axis_names()`.

  - [x] Titles: `rray_set_axis_title()`.

  - [x] Docs for each.

- [x] PR 4: Titles for multi axis functions.

  - [x] Swap the `r_null` titles from PR 2 for `rray_titles(x)`.

  - [x] Rep, roll, reverse.

  - [x] Remove axes.

  - [x] Permute axes.

  - [x] Docs for each.

- [ ] PR 5: Titles for `rray_combine()`.

  - [ ] Split the title merge loop out of `rray_broadcast_names_merge()`. Add
    `rray_titles_common()`.

  - [ ] `r_obj* axis` in `rray_combine()`, `r_int()` in `rray_stack()`.

  - [ ] Tests and docs.

- [ ] PR 6: Integer only guards and wrap up.

  - [ ] Character input snapshots for `rray_stack()`, `rray_insert_axes()`,
    `rray_move_axes()`, `rray_axis_title()`.

  - [ ] Integer only param docs link `faq-axes`.

  - [ ] `pkgdown::check_pkgdown()`.
