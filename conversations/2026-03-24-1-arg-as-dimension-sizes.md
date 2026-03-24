# Cost: $0.70

## Prompt

> I'd like check_dimension_sizes to instead call vec_cast(), casting to an integer(), so that it is more flexible. It should be renamed to arg_as_dimension_sizes(), like arg_as_array(). You should call the R api of vctrs::vec_cast() from C, there are many examples of doing something similar in vctrs and rlang itself. You probably need a rray_init_dimension_sizes() initializer that we call in our package initializer in init.c to initialize some global call objects to vctrs vec_cast so we dont have to look it up every time

## Changes

- Renamed `check_dimension_sizes()` to `arg_as_dimension_sizes()` in `dimension-sizes.c/h`
- Now returns `r_obj*` (the cast result) instead of `void`
- Added `rray_init_dimension_sizes()` to cache `vctrs::vec_cast` function, `integer()` ptype, and call object as globals
- Called from `ffi_rray4_init_library()` in `init.c`
- Non-integer inputs are cast via `vctrs::vec_cast(x, integer())` before validation
- Updated callers in `broadcast-template.h` and `reshape.c` to `KEEP()` the result and adjust `FREE()` counts
- Updated tests: doubles now coerce (e.g., `rray_broadcast(1, 1)` works), non-coercible types error via vctrs
- Regenerated snapshots for broadcast and reshape
