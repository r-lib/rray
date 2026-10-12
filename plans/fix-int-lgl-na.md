# Logical `NA` checked against integer `NA`

## Summary

Several places pass logical data to a helper that checks for missing values
with `r_globals.na_int`. The answer is right only because R happens to store
`NA_LOGICAL` and `NA_INTEGER` as the same value (`INT_MIN`). Code that relies
on this is hard to read and easy to break. A logical value should always be
checked against `r_globals.na_lgl`, and turned into `r_globals.na_int`
explicitly when it becomes an integer.

Do a full review of the package for this pattern and fix every case in one PR.

## Known cases

- `rray_cumulative_sum()` on logical input (`src/cumulative-sum.c`) passes
  logical data straight to `rray_add_int_one()`, which checks both operands
  with `rray_int_is_missing()`.

  Fix: add `rray_add_lgl_one(int out, int x, struct r_lazy error_call)` to
  `src/one-add.h`. It checks the running total `out` with
  `rray_int_is_missing()` and the new value `x` with `rray_lgl_is_missing()`,
  then returns `r_globals.na_int` for a missing result. Use it in
  `rray_cumulative_sum_lgl()`.

- `rray_cast_lgl_to_int_one()` in `src/cast.h` returns `x` unchanged, so a
  logical `NA` passes through as is and is later read as an integer `NA`.
  Every logical input to an integer helper in a binary operation goes
  through this cast. It is used 27 times:

  - `src/arithmetic-add.c`, `src/arithmetic-subtract.c`, and
    `src/arithmetic-multiply.c` (4 each)

  - `src/compare.c` (6)

  - `src/equal.c` and `src/extremum.c` (4 each)

  - `src/cast.c` (1), through `rray_cast_lgl_to_int()`, which backs
    `rray_cast()`

  Fix: map the missing value explicitly, the way
  `rray_cast_lgl_to_dbl_one()` already does:

  ```c
  static inline int rray_cast_lgl_to_int_one(int x) {
    return rray_lgl_is_missing(x) ? r_globals.na_int : x;
  }
  ```

  This one change fixes every binary operation at once.

## Already correct

These show the pattern to follow:

- `rray_sum_lgl()` and `rray_mean_lgl()` pass `r_globals.na_lgl` as the
  missing value to their shared logical and integer loops.

- `rray_cast_lgl_to_dbl_one()` checks `r_globals.na_lgl`.

- `rray_prod_lgl_one()` and the logical helpers in `src/one-extremum.h` and
  `src/one-logical.h` use `rray_lgl_is_missing()` and `r_globals.na_lgl`.

- `src/slice-subscript.c` checks a logical subscript against
  `r_globals.na_lgl` and writes `r_globals.na_int`.

## Full review

The list above came from a quick search, so it is a starting point, not the
whole set. For the review:

- Find every read of logical data (`r_lgl_cbegin()`, `r_lgl_begin()`,
  `r_lgl_get()`) and follow each value to the point where it is checked for
  missingness. Flag any check that uses `r_globals.na_int`, `NA_INTEGER`, or
  `rray_int_is_missing()`.

- Look at every helper with `int` in its name that is ever handed logical
  data, including through a macro such as `RRAY_BINARY()`,
  `RRAY_CUMULATIVE()`, `RRAY_REDUCE()`, or `RRAY_CAST()`.

- Check the reverse too: integer data checked with `r_globals.na_lgl` or
  `rray_lgl_is_missing()`.

- Check places that write a missing value into a logical output, and make
  sure they write `r_globals.na_lgl`.

## Performance

`r_globals.na_lgl` and `r_globals.na_int` are set at run time, so the
compiler can't tell they are equal and won't remove the new select. Each fix
adds a compare and a select per element. This should be cheap, since it does
not depend on the running result, but measure it:

- `rray_cumulative_sum()` on logical input, from `bench/cumulative.R`.

- `rray_add()`, `rray_greater_than()`, `rray_equal()`, and `rray_pmax()` with
  a logical input, from `bench/binary.R`. Its operations only use integer and
  double data today, so add logical.

- `rray_cast()` from logical to integer.

If a hot path gets slower, say so in the PR before keeping the change.

## Tests

Results should not change. Existing tests for logical `NA` in arithmetic,
comparison, equality, extrema, casting, and cumulative sum cover the
behavior. Add a test wherever the review finds a logical path with no `NA`
test. Run the full suite.
