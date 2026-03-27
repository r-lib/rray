# Dimensions common infrastructure

**Running cost: $0.40**

## Prompt

> We now need to add infrastructure to support rray_dimensions_common(..., .dimensions = NULL) which returns the common dimensions among an arbitrary number of array inputs. It is very much like vec_size_common() and should follow a similar implementation. It should live in dimensions.R
> If you are uncertain about anything while implementing, ask rather than making an educated guess.

## Questions asked

- C or R implementation? → C for performance
- `.absent` argument? → Leave it out, error on 0 inputs
- Drop NULL inputs? → Yes
- Pairwise rules: match or one is 1? → Yes
- `.dimensions` overrides completely? → Yes

## Implementation

Added `rray_dimensions_common(..., .dimensions = NULL)`:

- **R/dimensions.R**: R wrapper using `.Call()` with `list2(...)`
- **src/dimensions.c**:
  - `ffi_rray_dimensions_common()` — FFI wrapper
  - `rray_dimensions_common()` — reduces over list of inputs, skips NULLs, errors on 0 non-NULL inputs, `.dimensions` overrides
  - `rray_dimensions2()` — pairwise common dimensions (max dimensionality, broadcast rule per axis)
- **src/dimensions.h**: Declares `rray_dimensions_common()`, `rray_dimensions2()`
- **src/init.c**: Registered FFI entry
- **tests/testthat/test-dimensions.R**: 10 new tests covering identical, broadcasting, dimensionality extension, NULL dropping, single input, 3+ inputs, `.dimensions` override, incompatible error, zero-input error

All 386 tests pass.
