# Reuse common dimensions in `rray_combine()`

## Status

This document is an implementation plan. The implementation should be completed
in full, tested, committed, and pushed to the current `feature/combine` branch.

Do not change the R API. This is an internal C refactor that removes the common
dimension loop duplicated in `src/combine.c`.

## Goal

`rray_combine()` finds its output dimensions in two parts:

1. Find common broadcast dimensions on every axis except `.axis`.
2. Sum the input dimensions on `.axis`.

The first part is the same operation as `rray_dimensions_common()` if the
combine axis is ignored and returned with dimension `1`. Reuse the dimensions
common implementation for that part, then keep the checked sum in
`rray_combine()`.

For example, combining `[2, 1, 3]` and `[4, 5]` on axis 1 should work as if the
dimensions common calculation saw `[1, 1, 3]` and `[1, 5, 1]`:

```text
common dimensions with axis 1 ignored: [1, 5, 3]
sum on axis 1:                       2 + 4 = 6
output dimensions:                    [6, 5, 3]
```

Returning `1` on ignored axes is intentional. One is the identity dimension
for broadcasting. The ignored axes remain present and still contribute to the
dimensionality of the result.

## C API changes

Add `arg` to the internal `rray_dimensions_common()` signature:

```c
r_obj* rray_dimensions_common(
  r_obj* xs,
  r_obj* dimensions,
  struct rray_arg* arg,
  struct r_lazy error_call
);
```

Use `arg` as the parent passed to both `new_subscript_arg()` calls inside the
common dimension calculation. This preserves the argument context when the
calculation is reused by `rray_combine()`.

The FFI wrapper should pass `rray_args.empty` for `arg`. Update every other
caller of `rray_dimensions_common()` to pass the correct argument. In
particular, the existing broadcast common path should pass `rray_args.empty`.

Do not add an R argument to `rray_dimensions_common()`. Its R signature and
`.Call` registration stay unchanged.

## Internal helper with ignored axes

Move the calculated common dimension path into an internal helper. A suitable
shape is:

```c
r_obj* rray_dimensions_common_opts(
  r_obj* xs,
  r_obj* ignore,
  struct rray_arg* arg,
  struct r_lazy error_call
);
```

The exact helper name may change if another name reads better, but it should be
declared in `src/dimensions.h` because `src/combine.c` calls it.

The contracts are:

- `ignore` is either `r_null` or an integer vector of one-based axes.
- This is an internal interface. Its callers must pass validated axes.
- `r_null` means no axes are ignored.
- Every ignored axis has dimension `1` in the result.
- Ignoring an axis does not remove it and does not lower the result
  dimensionality.
- Non-ignored axes use the existing broadcast common rules and existing error
  messages.
- The input list must still be nonempty.

The ordinary `rray_dimensions_common()` entry point should keep its current
`.dimensions` behavior:

- If `dimensions` is not `r_null`, validate and return that override.
- Otherwise call the internal helper with `ignore = r_null`.

This keeps ignored axes out of the `.dimensions` override path. There is no
useful meaning for combining an explicit dimensions override with ignored axes
in the current API.

Within the helper, initialize output dimensions to `1` as today. Convert
`ignore` into a small stack allocated lookup, or use another simple approach
that makes the merge loop clear. When an axis is ignored, skip its
`rray_dimension2()` call so its output dimension remains `1`. Still update the
common dimensionality from every input before processing its axes.

Do not validate `ignore` through an R-facing axis parser. The normal entry
point always passes `r_null`, and `rray_combine()` has already validated its
axis with `check_axis()`.

## Refactor `rray_combine()`

Keep the existing order through type calculation, casting, maximum
dimensionality, and `check_axis()`. In particular, validate `.axis` before the
common dimension calculation. This preserves the current error precedence for
an invalid axis.

After `check_axis()`:

1. Allocate a protected integer scalar containing `axis`.
2. Call the internal common dimension helper with the cast `xs`, that scalar
   as `ignore`, the existing `arg`, and `error_call`.
3. Protect the returned dimensions.
4. Make a small pass over `xs` to sum the dimension on `axis`.
5. Treat a missing trailing axis as an implicit dimension of `1`, matching the
   current combine behavior.
6. Retain the current `INT_MAX` overflow check and its exact error message.
7. Store the checked sum into the ignored axis of the returned dimensions.

The extra pass over the input list is acceptable. It only reads dimensions,
and the array filling work dominates it. Do not complicate the dimensions
common helper with a sum output or a callback to avoid this pass.

Remove the duplicated common dimension state and merge code from
`src/combine.c`, including its local output argument tracking. The dimensions
helper now owns incompatible dimension errors and uses the parent `arg` from
combine, so named dots and nested arguments must continue to format correctly.

Do not change the name handling, stride plans, fill kernels, output type, or
the layout of combined values.

## Important cases

The refactor must preserve all of these behaviors:

- Different dimensions on the combine axis are accepted and summed.
- Incompatible dimensions on any other axis are rejected.
- A dimension of `1` broadcasts on non-combine axes.
- A lower-dimensional input has implicit trailing dimensions of `1`.
- A zero dimension on the combine axis contributes zero to the sum.
- A zero dimension on another axis follows the existing broadcasting rule.
- One input returns the same dimensions and values after normalization and
  casting.
- The combined axis sum errors before exceeding `INT_MAX`.
- Names on the combine axis are concatenated as before.
- Names on other axes continue to follow broadcast name rules.

## Files

At minimum, inspect and update:

- `src/dimensions.c`
- `src/dimensions.h`
- `src/combine.c`
- `src/broadcast.c`
- `src/decl/dimensions-decl.h` if the helper declarations change
- `tests/testthat/test-dimensions.R`
- `tests/testthat/test-combine.R`
- snapshot files changed by intentional error output changes

`src/init.c` should not need a registration change because no FFI signature is
changing. Confirm that this remains true.

## Tests

Keep all existing tests passing. Add focused coverage where the current tests
do not make the new contract obvious:

- Public `rray_dimensions_common()` behavior is unchanged with positional and
  named inputs.
- Combine accepts conflicting dimensions on the ignored combine axis.
- The same dimensions fail when that axis is not the combine axis.
- Combine still broadcasts singleton dimensions on the other axes.
- Combine on an implicit trailing axis still treats the missing dimension as
  `1`.
- Zero dimensions on and off the combine axis still work.
- Named inputs still appear correctly in incompatible dimension errors.

There is no R-facing test for `ignore` because it is intentionally not exposed.
Exercise it through `rray_combine()`.

Run at least:

```sh
Rscript -e "devtools::test(filter = '^(dimensions|combine|broadcast)')"
Rscript -e "devtools::test()"
```

No documentation should change because there is no R API change. If any
roxygen text is changed for an independent reason, run `devtools::document()`
and `pkgdown::check_pkgdown()` as required by the repository instructions.

## Required review passes

Do not add source comments. Preserve existing comments exactly.

After the code is complete:

1. Run `clang-format -i src/*.c src/*.h` over every C source and header.
2. Run `air format .`.
3. Review the diff for unrelated formatting or user changes.
4. Perform the required protection pass separately from the general review.

For the protection pass, list every new or touched `r_obj*` in the diff and
identify the next function that reads it. Check whether that function can
allocate before protecting or consuming the value. Pay particular attention to
the scalar `ignore` object, the common dimensions result, input names, and the
argument shelters constructed in the dimensions helper.

Do not use `gctorture()` or `gctorture2()`.

## Commit and push

Before committing, confirm that the current branch is `feature/combine` and
that the diff contains only this implementation plus any pre-existing user
changes that must remain uncommitted. Do not discard or overwrite unrelated
work.

Use this commit message:

```text
Reuse common dimensions when combining arrays
```

The commit message must be exactly that one sentence, with no trailer and no
trailing period.

Push the completed commit to the current remote branch:

```sh
git push origin feature/combine
```

Report the commit hash, pushed branch, tests run, and their results.
