Cost: $1.50

## Prompt

> Okay, we are going to do something more complicated now. In rray_reshape() we really should not have to do a full deep clone of x just to alter some attributes on it. Instead, we can create a shallow "wrapper" around x that gets its own attribute pairlist we can modify. This is very similar to the ALTREP wrapper that is returned by R_tryWrap() in the R sources, but we are going to have our own version of this since it isn't meant to be exposed. Let's implement this just for integers, doubles, and character vectors right now, and we can fill in the rest in a bit. There are some ALTREP classes in vctrs that you can use to get an idea of how to do this. The key feature is that we should be able to get a read only pointer to the underlying data without making a full duplication of it, but if a write access pointer is requested then we need to perform the duplication.

## Implementation

Created ALTREP wrapper classes for all 7 vector types (logical, integer, double, complex, raw, character, list).

### Naming iterations

- `rray_wrapper_real_class` → `rray_wrapper_double_class`
- `rray_wrapper_string_class` → `rray_wrapper_character_class`
- All method names from PascalCase to snake_case
- `wrapper_make` → `new_wrapper`
- `rray_init_wrapper` forward declared in init.c, not wrapper.h

### Key design

- `data1` = wrapped vector, `data2` = `r_false` (not owned) or `r_true` (owned)
- Read-only access delegates to wrapped vector
- Write access clones data and takes ownership
- Wrapping a wrapper creates a new wrapper sharing same underlying data (avoids nesting)
- Serialization returns NULL → round-trips as plain vector
- FFI test helpers (`ffi_test_r_wrap`, `ffi_test_r_is_wrapper`, `ffi_test_wrapper_is_owned`) live in wrapper.c before initializers

### Bug fix

Fixed infinite recursion: `r_wrap(wrapper)` → `r_clone()` → `wrapper_duplicate(deep=FALSE)` → `r_wrap()` → loop. Fixed by going directly to `R_new_altrep()` in both `r_wrap` and `wrapper_duplicate` for the wrapper-of-wrapper / shallow-dup cases.

### Tests

131 tests in `test-wrapper.R` covering:
- Wrapping all 7 types
- Type/length preservation
- Attribute independence
- Read access for all types
- Ownership transitions on write for all types
- Wrapping a wrapper (no nesting, independent attributes)
- Shallow duplication
- Serialization round-trips
- Zero-length vectors
- NA handling
- Subsetting
- Integration with rray_reshape

All 262 tests pass (131 original + 131 new).
