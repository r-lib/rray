# Dimensions common infrastructure

**Running cost: $0.55**

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
  - `rray_dimensions_common()` — reduces over list of inputs, skips NULLs, errors on 0 non-NULL inputs, `.dimensions` overrides. Uses stack-allocated `int[RRAY_MAX_DIMENSIONALITY]` accumulator initialized to 1s.
  - `rray_dimensions2()` — `static inline void` pairwise accumulator, mutates output dimensions and dimensionality in place
- **src/decl/dimensions-decl.h**: Forward declaration of `rray_dimensions2()` as `static inline void`
- **src/dimensions.h**: Declares `rray_dimensions_common()` (not `rray_dimensions2`)
- **src/init.c**: Registered FFI entry
- **tests/testthat/test-dimensions.R**: 10 new tests

> dont expose rray_dimensions2 in dimensions.h, use a decl header

Done.

> Review the changes I've made to the common dimensions implementation

User reworked the implementation:
- Stack-allocated accumulator instead of per-iteration R allocations
- `rray_dimensions2` changed to `static inline void` mutating in place
- Added `check_max_dimensionality()` guard
- Single `r_alloc_integer` + `memcpy` at the end
- No-op branches made explicit with commented-out assignments

All 28 dimensions tests pass.
