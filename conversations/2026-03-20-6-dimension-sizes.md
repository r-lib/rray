# Implement `rray_dimension_sizes()`

## Summary

Implemented `rray_dimension_sizes()` which returns the dimension sizes of an array as an integer vector. For plain vectors (no `dim` attribute), returns the length as a single integer.

## Files created/modified

- `src/dimension-sizes.c` — C implementation with FFI wrapper and internal function
- `src/dimension-sizes.h` — Header declaring `rray_dimension_sizes()`
- `src/init.c` — Registered the new FFI function
- `R/dimension-sizes.R` — R wrapper with roxygen2 docs
- `tests/testthat/test-dimension-sizes.R` — Tests for vectors, arrays, matrices, empty vectors, NULL, and non-vector types

## Design decisions

- For plain vectors without `dim`, returns `length(x)` as a single integer (1D)
- For arrays, returns the `dim` attribute directly
- Same type checking as `rray_dimensionality()` — supports logical, integer, double, complex, character, raw, and list types
- Internal C function returns `r_obj*` (unlike `rray_dimensionality()` which returns `r_ssize`) since dimension sizes is a vector, not a scalar

## Refactoring: `check_array()`

Extracted the duplicated type-checking switch into `check_array()` in `src/utils.c` / `src/utils.h`. Both `rray_dimensionality()` and `rray_dimension_sizes()` now call `check_array(x)` instead of inlining the switch.
