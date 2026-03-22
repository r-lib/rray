Cost: $1.00

# Broadcast dimension names

## Discussion

Planning how to layer dimension names broadcasting into `rray_broadcast()`. Before doing that, reworked `rray_dimension_names()` to return `NULL` when there are no dimension names at all, rather than a list of NULLs. This simplifies downstream consumers like broadcast.

## Changes

### `rray_dimension_names()` rework
- Now returns `NULL` for unnamed vectors and unnamed arrays
- Still returns a list (possibly with NULL elements) when any dimension has names
- Updated C implementation, R docs, and tests
- Removed unused `dimensionality.h` include from `dimension-names.c`

### `rray_broadcast()` dimension names support
- Added `rray_broadcast_dimension_names()` in `dimension-names.c/h`
  - Returns `NULL` early if input has no names
  - Keeps names for dimensions whose size is unchanged
  - Drops names for broadcast dimensions (size 1 -> N)
  - New dimensions get `NULL`
  - Returns `NULL` if all names end up dropped
- Called from `broadcast-template.h` after the broadcast loop
- Only sets `dimnames` attribute when non-NULL
- 7 new tests covering: preserved names, dropped names, all dropped, named vectors, unnamed vectors, new dimensions, partial names
