Cost: $3.19

# Broadcast iterator

Designing and implementing a reusable, stack-allocated broadcast iterator in C. The iterator creates a "view" over original data with new compatible dimension sizes, stepping through one index at a time and providing the flat location into the original array at each step. All functions are `static inline` in a header-only file (`src/broadcast-iterator.h`), with a max dimensionality of 64 to avoid heap allocations.
