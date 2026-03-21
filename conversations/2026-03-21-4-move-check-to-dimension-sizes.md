Cost: ~$0.60

# Move check_broadcast_dimension_sizes and broadcast template refactor

## check_broadcast_dimension_sizes -> dimension-sizes.c

Moved `check_broadcast_dimension_sizes` (now `check_dimension_sizes`) from `broadcast.c` to `dimension-sizes.c` and exposed it via `dimension-sizes.h`.

## Broadcast template pattern (Verstable-style)

Refactored `broadcast_impl` into a Verstable-style template pattern:

- **`broadcast-template.h`** — no include guard, included multiple times. Defines the generic broadcast function body. Parameterized by:
  - `BROADCAST_FN` — function name
  - `BROADCAST_R_TYPE` — R type enum
  - `BROADCAST_SETUP` — pointer declarations (empty for chr/list)
  - `BROADCAST_COPY(i, loc)` — element copy expression
  All macros are `#undef`'d at end of each inclusion.

- **`broadcast.c`** — includes the template 7 times (lgl, int, dbl, cpl, raw, chr, list). `broadcast_switch` dispatches by `r_typeof(x)`.

All 76 tests pass.
