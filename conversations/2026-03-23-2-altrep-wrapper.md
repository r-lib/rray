Cost: $0.60

## Prompt

> Okay, we are going to do something more complicated now. In rray_reshape() we really should not have to do a full deep clone of x just to alter some attributes on it. Instead, we can create a shallow "wrapper" around x that gets its own attribute pairlist we can modify. This is very similar to the ALTREP wrapper that is returned by R_tryWrap() in the R sources, but we are going to have our own version of this since it isn't meant to be exposed. Let's implement this just for integers, doubles, and character vectors right now, and we can fill in the rest in a bit. There are some ALTREP classes in vctrs that you can use to get an idea of how to do this. The key feature is that we should be able to get a read only pointer to the underlying data without making a full duplication of it, but if a write access pointer is requested then we need to perform the duplication.

## Plan

Create `src/wrapper.c` and `src/wrapper.h` implementing ALTREP wrapper classes for integer, double, and character vectors. Key design:

- `data1` = wrapped vector, no `data2` needed (we don't need sortedness metadata)
- Read-only data pointer delegates to wrapped vector
- Write data pointer duplicates the wrapped vector first (copy-on-write)
- Duplicate: deep copy duplicates data, shallow copy makes new wrapper sharing data
- Serialization: return NULL so it serializes as a plain vector
- Update `reshape.c` to use wrapper instead of `r_clone()` for supported types
- Init ALTREP classes from `R_init_rray4`

## Implementation

Created:
- `src/wrapper.h` - declares `rray_wrap()` and `rray_init_wrapper()`
- `src/wrapper.c` - ALTREP wrapper classes for integer, double, and character

Updated:
- `src/reshape.c` - uses `rray_wrap()` for int/double/character, falls back to `r_clone()` for other types
- `src/init.c` - calls `rray_init_wrapper(dll)` during package init

All 131 tests pass.
