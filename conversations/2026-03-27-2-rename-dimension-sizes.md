# Rename dimension_sizes to dimensions

**Running cost: $0.50**

## Prompt

> We need to rework the package to use the new terminology outlined in the Terminology section of CLAUDE.md. Lets start by changing rray_dimension_sizes to rray_dimensions everywhere, including all local variables, file names, tests, and documentation

## Changes

Renamed `dimension_sizes` to `dimensions` across the entire codebase:

### File renames
- `src/dimension-sizes.c` -> `src/dimensions.c`
- `src/dimension-sizes.h` -> `src/dimensions.h`
- `R/dimension-sizes.R` -> `R/dimensions.R`
- `tests/testthat/test-dimension-sizes.R` -> `tests/testthat/test-dimensions.R`
- `tests/testthat/_snaps/dimension-sizes.md` -> `tests/testthat/_snaps/dimensions.md`
- Deleted `man/rray_dimension_sizes.Rd` (regenerated as `man/rray_dimensions.Rd`)

### Function/variable renames
- `rray_dimension_sizes()` -> `rray_dimensions()`
- `ffi_rray_dimension_sizes()` -> `ffi_rray_dimensions()`
- `rray_dimension_sizes_are_equal()` -> `rray_dimensions_are_equal()`
- `arg_as_dimension_sizes()` -> `arg_as_dimensions()`
- `rray_dimensionality_from_dimension_sizes()` -> `rray_dimensionality_from_dimensions()`
- `rray_reduce_dimension_sizes()` -> `rray_reduce_dimensions()`
- `rray_split_dimension_sizes()` -> `rray_split_dimensions()`
- `dimension_sizes_chr` -> `dimensions_chr`
- All local variables like `v_dimension_sizes`, `v_x_dimension_sizes`, `x_dimension_size`, `dimension_size`, etc.
- Iterator struct fields: `v_point_dimension_sizes` -> `v_point_dimensions`, `v_location_dimension_sizes` -> `v_location_dimensions`

### Files updated
- C: dimensions.c/h, broadcast.c/h, broadcast-template.h, broadcast-iterator.h, dimensionality.c/h, init.c, iterator.h, reduce.c/h, reduction-iterator.h, reshape.c/h, split-template.h, sum-template.h, utils.c/h, decl/broadcast-template-decl.h, decl/split-template-decl.h
- R: dimensions.R, broadcast.R, reshape.R
- Tests: test-dimensions.R, test-broadcast.R, test-reshape.R, test-sum.R
- Snapshots: dimensions.md, broadcast.md, reshape.md
- NAMESPACE (regenerated)
- Man pages (regenerated)

### Error message changes
- `"dimension sizes"` -> `"dimensions"` in error messages (e.g., reshape error, arg validation errors)

All 372 tests pass.
