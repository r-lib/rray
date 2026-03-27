# Rework rray_reshape as rray_set_dimensions

**Running cost: $0.20**

## Prompt

> Let's rework rray_reshape as rray_set_dimensions. It should end up in dimensions.R alongside rray_dimensions, same for the c code. The R code should still have its own help file though.

## Work

Renamed `rray_reshape` -> `rray_set_dimensions` everywhere:

- **R code**: Kept in `R/reshape.R` (own help file via roxygen2), updated function name and docs
- **C code**: Moved from `src/reshape.c` into `src/dimensions.c`, deleted `src/reshape.c` and `src/reshape.h`, updated `src/dimensions.h` with new declaration
- **init.c**: Updated extern and registration table
- **Tests**: Renamed `test-reshape.R` -> `test-set-dimensions.R`, updated all references
- **Wrapper tests**: Updated `rray_reshape` -> `rray_set_dimensions` in `test-wrapper.R`
- **Snapshots**: Deleted old `_snaps/reshape.md`, regenerated as `_snaps/set-dimensions.md`
- **Error message**: Changed from "Can't reshape to these dimensions" to "Can't set these dimensions"

All 372 tests pass.
