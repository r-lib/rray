# Full broadcasting for `rray_if_else()`

## Status

This is an implementation plan. The current function uses the dimensions of
`condition` as the output dimensions. It broadcasts only `true`, `false`, and
`missing` to that shape. This plan changes that rule; it does not describe code
that is already present.

## Public API

```r
rray_if_else <- function(
  condition,
  true,
  false,
  ...,
  missing = NULL,
  dimensions = NULL
)
```

`...` must remain empty. Put `dimensions` after `missing`, so existing calls
keep working.

When `dimensions` is `NULL`, find the common broadcast dimensions of
`condition`, `true`, `false`, and `missing` when supplied. The result has those
dimensions. Every input must be compatible with them, even if the condition
never selects that branch. This matches the shape rule of `where()` in other
array libraries.

When `dimensions` is supplied, validate it as a dimensions vector and use it
as the exact output dimensions. Check that every supplied input broadcasts to
it. In particular, callers can restore the current shape guarantee with:

```r
rray_if_else(condition, true, false, dimensions = rray_dimensions(condition))
```

This call errors if a branch needs a larger dimension than `condition` has.
`dimensions` is an output shape, not a value to combine with the input shapes.
It may also specify a larger compatible shape when that is useful.

All result names are dropped, including names on `condition` and all branches.
The result is still an array, and all supplied branches still determine its
common type. `NULL` for `missing` continues to mean that an `NA` condition
produces a missing value of the result type.

## Examples

A one-column condition makes one choice per row. The chosen branch supplies
the entire row across the output columns:

```r
condition <- array(c(TRUE, FALSE, NA), c(3L, 1L))
true <- array(1:12, c(3L, 4L))
false <- array(101:112, c(3L, 4L))
missing <- array(201:212, c(3L, 4L))

out <- rray_if_else(condition, true, false, missing = missing)

expected <- true
expected[2L, ] <- false[2L, ]
expected[3L, ] <- missing[3L, ]

identical(out, expected)
```

The same shape rule applies without an explicit `missing` branch:

```r
condition <- array(c(TRUE, FALSE), c(2L, 1L))
true <- array(1:6, c(2L, 3L))
false <- array(11:16, c(2L, 3L))

rray_if_else(condition, true, false)
```

The first result row is `true[1L, ]`; the second is `false[2L, ]`.
Both examples should appear in the function documentation and tests.

## C changes

1. Add `dimensions` to the R wrapper, FFI wrapper, internal function, header,
   and `src/init.c` registration.
2. Normalize the inputs as now. With `dimensions = NULL`, compute common
   dimensions across `condition` and all supplied branches. Otherwise,
   validate the requested dimensions. Check every input against the final
   dimensions before filling the result.
3. Compute broadcast strides for `condition` as well as for each branch. The
   current fill loop reads `condition[i]`; it must instead read the condition
   at its broadcast location for each output element. Give `condition` an
   explicit stride-zero path in the inner loop: read its logical value once
   per run when the condition location is fixed, then use that value across
   the run. This matters for a one-column condition selecting whole rows.
4. Allocate the output from the size of the final dimensions rather than
   `r_length(condition)`. Attach only those dimensions. Do not attach names.
5. Keep the current type promotion and missing-value behavior. Use the
   existing run iterator with three input locations when `missing` is `NULL`
   and four when it is supplied.

## Tests and checks

- Add the two row-selection examples above, including the result dimensions.
- Cover both stride-zero and advancing condition locations in the tests.
- Test a condition that broadcasts across more than one axis.
- Test `dimensions = rray_dimensions(condition)` when branches fit and when a
  branch needs a larger dimension.
- Test an explicit larger compatible `dimensions` value and an incompatible
  value. All inputs must be checked, including an unselected branch.
- Test that names are absent even when every input has names.
- Update existing tests that assume `condition` always sets the output shape.
  In particular, a branch with an extra singleton axis can now increase the
  output dimensionality.
- Keep tests for type promotion, `NA` conditions, and zero-size dimensions.

After changing R or C code, run `air format .` and
`clang-format -i src/*.c src/*.h`. Re-document the package, run the
`if-else` tests, and run `pkgdown::check_pkgdown()`. Before calling the C work
done, make a separate pass over every new or touched `r_obj*` to check
protection across calls that may allocate.
