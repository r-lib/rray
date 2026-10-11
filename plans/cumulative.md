# Cumulative functions

## Scope and decisions

Add `rray_cumulative_sum()`, `rray_cumulative_prod()`,
`rray_cumulative_min()`, `rray_cumulative_max()`,
`rray_cumulative_any()`, and `rray_cumulative_all()`. Each takes one
required, one-based `axis`. There is no `na_rm` argument. Do not add a
cumulative mean. The current mean reducer uses a corrected second pass for
doubles and exact wide integer sums. A cumulative mean would need a separate
numerical rule for every prefix.

For an array `x`, the value at position `i` on the selected axis summarizes
positions `1:i` on that axis. All other coordinates stay fixed. Each such
line starts with fresh state. The output has the same dimensions and axis
names as `x`; only the element type may change. A vector is treated as a
one-dimensional array, following the rest of rray.

```r
x <- matrix(1:6, nrow = 2)
rray_cumulative_sum(x, axis = 1L)
rray_cumulative_sum(x, axis = 2L)
```

The first call gives rows `(1, 3, 5)` and `(3, 7, 11)`. The second gives
rows `(1, 4, 9)` and `(2, 6, 12)`.

One axis keeps prefix order clear. NumPy also accepts one axis for
[`cumsum()`](https://numpy.org/doc/stable/reference/generated/numpy.cumsum.html);
its `axis = None` scans a flattened array. rray will require an axis and
retain the input dimensions. A scan over several axes would need a separate
rule for their order and for which intermediate prefixes become output.

These are array scans: the selected axis does not collapse. Use the current
reducer type rules and the existing one-element operations where they fit.
Each section below states any difference from a reducer or base R.

## Public API and file layout

- Put all six documented and exported R functions in `R/cumulative.R`.
  Use one `rray-cumulative` documentation topic, a required `axis`, and
  `.Call(ffi_rray_cumulative_..., x, axis, environment())`. Do not add
  `na_rm` or an unused `...` argument.
- Add `src/cumulative.c`, `src/cumulative.h`, and
  `src/decl/cumulative-decl.h` for input validation, shape handling,
  dispatch, and `RRAY_CUMULATIVE()`. Add operation files
  `src/cumulative-sum.c/.h`, `src/cumulative-prod.c/.h`,
  `src/cumulative-extremum.c/.h`, and `src/cumulative-logical.c/.h`,
  each with its own `src/decl/*-decl.h`.
  Extremum holds min and max; logical holds any and all, as the reducers do.
- Add the six `extern` FFI declarations and registrations to `src/init.c`.
  Keep FFI declarations out of feature headers. Put thin FFI wrappers first
  in each operation `.c`, then the internal entry points, type switches,
  typed loops, and helpers in use order. Include the decl header last.
- Add `test-cumulative-sum.R`, `test-cumulative-prod.R`,
  `test-cumulative-extremum.R`, and `test-cumulative-logical.R`.
  Add the topic to `_pkgdown.yml` under a Cumulative section, then
  document and run `pkgdown::check_pkgdown()`.

The common C entry point should accept the validated integer axis and a
typed function switch, for example
`rray_cumulative(x, axis, fn_switch, arg, error_call)`. A typed function
receives `x`, `stride`, `axis_dimension`, `outer`, and `error_call`, and
returns a freshly allocated output vector. Each public internal function
passes its switch to the common entry point. Convert the R axis in the
FFI wrapper with `arg_as_int(ffi_axis, rray_args.axis, error_call)` and call
`check_axis()` after finding dimensionality. Use `check_unclassed()` and
`arg_as_array()` as `rray_reduce()` does. Switch on `rray_typeof(x)` and
reject unsupported types through a cumulative-specific error naming the
operation. For any and all, use `check_logical()` as their reducers do.

The common entry point protects the converted `x` and the typed output. It
attaches `r_dim(x)` and, when present, `r_dim_names(x)`. Both are borrowed
from the protected input. The typed function or macro allocates and fills
the output. Do not copy class or other attributes. Preserve all axis names,
including the scanned axis, since every output position still belongs to
the same input position. A named vector becomes a one-dimensional array
with its names on that axis through `arg_as_array()`.

## Traversal and `RRAY_CUMULATIVE()`

For a one-based axis, define `stride` as the product of dimensions before
that axis, `axis_dimension` as its dimension, and `outer` as the product of
dimensions after it. These use `r_ssize`. For each outer block, the source
index for axis position `k` and inner lane `j` is
`block * axis_dimension * stride + k * stride + j`.

```text
dimensions c(2, 3, 4), axis 2:
stride = 2, axis_dimension = 3, outer = 4
one line uses offsets 0, 2, 4; another uses 1, 3, 5
the next block begins at offset 6
```

Make `RRAY_CUMULATIVE()` analogous to `RRAY_REDUCE()`: typed input and output
accessors, output type, initial state, and one-element operation. The macro
allocates one output vector, obtains input and output pointers once, and
writes every prefix.

Process each block in axis order and each `k` across adjacent `j` lanes.
At `k = 0`, apply the operation to its identity and the first input. At
later positions, read the preceding result at `index - stride` and combine
it with `x[index]`. This avoids a scratch array for sum, product, extrema,
and logical scans. The output never aliases the input. The `k` loop must
remain sequential; the lane loop may vectorize. Axis 1 is a single
contiguous line per outer block. Do not pass this scan through the reduction
iterator, whose zero strides intentionally merge all positions of a reduced
axis into one result.

For zero-sized arrays, allocate a zero-sized output of the right type and
attach the input shape and names. Do not divide by an axis dimension or
read a first element. An axis of dimension 1 still applies type conversion.
Use `r_ssize` for size, stride, and outer counts. Base their products on the
validated dimensions of `x`, and check multiplication if a product can be
formed before encountering a zero dimension. An error in any line aborts
the whole call. Line state does not leak into the next line.

## `rray_cumulative_sum()`

Accepted types: logical, integer, double, and complex. Logical input yields
integer; every other accepted input keeps its type, like `rray_sum()` and
base [`cumsum()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/cumsum.html).
Use zero as the starting value, separately for each line.

Reuse `rray_add_int_one()` from `src/one-add.h` for both integer and logical
input. Logical values already use `0`, `1`, and the same missing sentinel as
integers. This checks each returned prefix against `[-INT_MAX, INT_MAX]`.
The current `rray_sum()` instead sums in `int64_t`, or its `int128_t`
fallback for very long reductions, and checks only the final result. A
cumulative sum must error as soon as a returned prefix overflows, even if
a later value or `NA` would make the final reduction valid. Once a line is
missing, `rray_add_int_one()` keeps it missing and does not report a later
overflow. Base `cumsum()` warns and fills the suffix with `NA` on overflow.

```r
x <- c(.Machine$integer.max, 1L, -1L)
rray_sum(x, 1L)
rray_cumulative_sum(x, 1L)
```

The reducer returns `.Machine$integer.max`; the scan errors at its second
prefix.

For double input, reuse `rray_add_dbl_one()`. For complex input, reuse
`rray_add_cpl_one()`, which adds real and imaginary parts separately. These
are ordinary double additions in prefix order, like the current double and
complex sum reducers. Do not add a separate missing-value rule. When `NA`
and `NaN` meet, the result can be either one, depending on the platform and
operand order. Test `is.na()` for those mixed cases. A single `NA` or `NaN`
persists in its line, and opposing infinities produce `NaN`. Base `cumsum()`
revisits missing prefixes to prefer `NA`; it also uses `long double` when
available for real doubles. Compare with base only for cases whose semantics
agree.

Test both scan axes and a third axis, types, resets between lines, upper
and lower integer overflow, cancellation after overflow, a missing value
before overflow, both `NA` and `NaN` orders, opposing infinities, and
complex values with a missing real or imaginary part.

## `rray_cumulative_prod()`

Accepted types: logical, integer, double, and complex. Logical and integer
input yield double, matching `rray_prod()` and base `cumprod()`. Double and
complex input keep their type. Start every line at one, with a zero
imaginary part for complex numbers.

Reuse `rray_prod_lgl_one()`, `rray_prod_int_one()`, `rray_prod_dbl_one()`,
and `rray_prod_cpl_one()` from `src/one-multiply.h`. Integer multiplication
is done in double, so it has no integer overflow error. A double product may
become `Inf` or underflow to zero; `0 * Inf` is `NaN`. As with the current
product reducer, do not promise whether `NA` or `NaN` wins when both occur.
For complex multiplication, keep rray's C99 operation and its infinity
recovery behavior. Its special values can differ from base R's complex
`cumprod()`. Base real `cumprod()` also uses `long double` when available
and revisits missing prefixes to prefer `NA`. State these differences in
documentation.

Test type promotion, line resets, signed and zero products, `0 * Inf`,
both `NA` and `NaN` orders, overflow to infinity, underflow, and finite
complex products. Compare simple lines to base `cumprod()`.

## `rray_cumulative_min()`

Accepted types: logical, integer, and double. Keep the input type, as
`rray_min()` does. Base `cummin()` returns integer for logical input, so
that is an intentional type difference. Complex is unsupported.

Start each line at `TRUE`, `INT_MAX`, or `Inf`, respectively. Reuse the
missing-aware operations in `src/one-extremum.h` where their behavior
matches prefix semantics. A logical or integer `NA` persists through the
rest of its line. A double `NaN` persists until a later `NA`, which then
persists. Later finite values cannot clear either missing state. Compare
ordinary values by taking the smaller one. Check ties and signed zero
explicitly: the existing one-element helper keeps the earlier operand on
a tie, while base R's double `cummin()` may keep the later one. Keep the
rray helper rule and avoid a bit-level promise for signed zero.

Test logical, integer, and double types; rising and falling values;
repeated extrema; both missing orders; infinity; signed zero; empty
lines; and independent lines on each axis.

## `rray_cumulative_max()`

Accepted types and output types match cumulative min. Start each line at
`FALSE`, `-INT_MAX`, or `-Inf`, respectively. Use the matching
missing-aware maximum operations in `src/one-extremum.h`. An `NA` or
`NaN` has the same persistent rule as cumulative min. Logical maximum is
an ordered maximum over `FALSE < TRUE` and does not have the logical
short-circuit behavior of cumulative any. In particular,
`rray_cumulative_max(c(NA, TRUE), 1L)` remains missing, while cumulative
any becomes `TRUE` at the second position.

Test the same shape, type, missing, infinity, signed-zero, and line-reset
cases as min, with both increasing and decreasing values. Compare ordinary
numeric lines to base `cummax()`.

## `rray_cumulative_any()`

Accept only logical input and return logical. Use the three-valued OR
operation in `src/one-logical.h`, starting each line at `FALSE`.
`FALSE, NA, TRUE` yields `FALSE, NA, TRUE`: a later `TRUE` resolves an
earlier unknown result. `TRUE, NA` remains `TRUE, TRUE`. This is the
prefix form of `rray_any()`, and matches dplyr's `cumany()` for logical
vectors. dplyr coerces other inputs to logical; rray should keep the
reducer's strict logical input rule.

Test every combination of `FALSE`, `TRUE`, and `NA` in short lines,
especially unknown followed by decisive `TRUE`; test axis resets,
empty arrays, names, and rejection of nonlogical input.

## `rray_cumulative_all()`

Accept only logical input and return logical. Use the three-valued AND
operation in `src/one-logical.h`, starting each line at `TRUE`.
`TRUE, NA, FALSE` yields `TRUE, NA, FALSE`: a later `FALSE` resolves an
earlier unknown result. `FALSE, NA` remains `FALSE, FALSE`. This matches
dplyr's `cumall()` for logical vectors, with the same strict-input
difference as cumulative any.

Test every combination of `FALSE`, `TRUE`, and `NA` in short lines,
especially unknown followed by decisive `FALSE`; test axis resets,
empty arrays, names, and rejection of nonlogical input.

## Cross-cutting checks and completion

Use a 3D example with distinct values on all axes to check indexing.
Check that `axis` rejects length zero, length greater than one, `NA`, zero,
negative, fractional, out-of-range, and attributed values. Check that a
whole-valued double axis is accepted, as `arg_as_int()` allows. Check
unclassed input rules and unsupported input types. Verify dimensions and
dimnames with absent, partial, and full names, including names on a plain
vector. An array with any zero dimension returns a zero-size result of the
correct type and shape. No state is read when size is zero. For ordinary
nonmissing lines, compare the last prefix with the matching reducer over
that axis.

Add only tests whose expected values exercise behavior, including snapshots
for errors thrown by this package. Do not snapshot base R errors. Use
`devtools::test(filter = '^cumulative')` and then the full test suite.
After generating C, run `clang-format -i src/*.c src/*.h`; after generating
R, run `air format .`. Run `devtools::document()` and
`pkgdown::check_pkgdown()` after the roxygen work.

Before calling the C work done, make a separate pass through every new or
touched `r_obj*`. For each one, name its next callee or read and check whether
that callee can allocate before it protects or consumes the value. Protect
the converted input, output, and any allocated dimensions or names through
later allocations. Do not use `gctorture()` or `gctorture2()`. Do not add
source comments; the details belong in this plan and the public roxygen
documentation.

Useful source references: base R's
[`src/main/cum.c`](https://github.com/wch/r-source/blob/trunk/src/main/cum.c),
the base [cumulative function manual](https://stat.ethz.ch/R-manual/R-devel/library/base/html/cumsum.html),
dplyr's [cumulative helper manual](https://dplyr.tidyverse.org/reference/cumall.html),
and NumPy's [`cumsum()` manual](https://numpy.org/doc/stable/reference/generated/numpy.cumsum.html).
