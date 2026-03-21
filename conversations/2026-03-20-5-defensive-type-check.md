# Defensive type checking in C functions

Date: 2026-03-20

## Summary

Added a type switch to `rray_dimensionality()` so it only accepts vector types and errors on everything else via `r_abort_lazy_call()`.

## Changes

- `src/dimensionality.c`: `rray_dimensionality()` now switches on `r_typeof(x)` and only allows `logical`, `integer`, `double`, `complex`, `character`, `raw`, and `list` types. All other types (including `NULL`) hit `r_abort_lazy_call()` with `r_obj_type_friendly()` for nice error messages. Also includes `cnd.h` for the abort helpers.
- `R/dimensionality.R`: User updated `@param x` from "An object" to "An array".
- `tests/testthat/test-dimensionality.R`: Changed NULL test from expecting `1L` to `expect_snapshot(error = TRUE)`. Added snapshot error tests for closure, symbol, and environment inputs.
- Snapshot files generated and accepted.

## Decisions

- User removed `R_TYPE_null` and `R_TYPE_expression` from allowed types (I had originally included them).
- User preferred `r_abort_lazy_call()` with `r_obj_type_friendly()` over plain `r_abort()` with `r_type_as_c_string()`.
- User removed the static `rray_dimensionality_impl()` helper, using a `break` in the switch instead.
