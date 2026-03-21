# 2026-03-20-3 - Implementing rray_dimensionality()

## Goal
Set up C infrastructure for `rray_dimensionality()`: C implementation in `src/dimensionality.{c,h}`, FFI function, init.c registration, and R wrapper.

## Progress
- Created `src/dimensionality.h` - declares `rray_dimensionality()` returning `R_xlen_t`
- Created `src/dimensionality.c` - `ffi_rray_dimensionality()` wraps as SEXP, `rray_dimensionality()` checks `R_DimSymbol` attribute
- Created `src/init.c` - uses `extern` declarations (not includes), registers FFI functions
- Created `R/rray4-package.R` with `@useDynLib rray4, .registration = TRUE`
- Updated `R/dimensionality.R` with roxygen docs and `.Call(ffi_rray_dimensionality, x)`
- Added tests in `tests/testthat/test-dimensionality.R` — 10 tests, all passing
- Documented C coding conventions in CLAUDE.md based on user's preferred style

## Key conventions learned
- `init.c` uses `extern` declarations, not `#include` of feature headers
- Feature headers only expose internal C functions, not FFI wrappers
- Use `R_xlen_t` over `int`, `Rf_xlength()` over `Rf_length()`
- Use `Rf_getAttrib()` + `R_NilValue` check rather than `Rf_isArray()`
- FFI functions go above internal functions in the `.c` file
