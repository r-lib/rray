# Inline mutable strided traversal state

## Status

Not implemented.

This plan removes `struct rray_strided_iterator` and
`struct rray_strided_iterator2`. Their mutable fields move into local variables
inside each typed worker. The immutable plan structs remain.

The goal is to simplify the traversal model and give Clang the clearest
possible view of hot loop state. This should also make repeated inner
traversals cheaper when reductions such as `rray_mean_along()` are added.

## Conclusion from the current work

The experiments in `plans/no-macros.md` support separating fixed plan data from
mutable traversal state. They do not show that mutable traversal state needs to
be stored in a struct.

The current one-location iterator contains only:

```c
struct rray_strided_iterator {
  r_ssize run_start;
  r_ssize location;
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
};
```

The two-location iterator adds a second location. Every current caller creates
the iterator inside a typed worker, walks it to completion, and discards it.
No caller stores it, transfers ownership, pauses it, or resumes it from another
function.

The struct therefore gives a name to local loop state, but it does not enforce
a useful boundary. Its constructor and accessors add concepts without hiding
meaningful behavior.

The more precise conclusion from the existing benchmarks is:

1. Keep dimensions, strides, size, and dimensionality in an immutable plan.
2. Keep changing traversal state local to the typed worker.
3. Do not require that local state to be one aggregate object.

## What stays

Keep both immutable plan types:

```c
struct rray_strided_iterator_plan;
struct rray_strided_iterator2_plan;
```

Keep their current responsibilities:

- Validate dimensionality.
- Calculate size.
- Calculate broadcast strides.
- Coalesce compatible axes.
- Store dimensions and strides for one traversal.

Keep the current hot loop properties:

- Walk the coalesced first axis as one run.
- Carry later point coordinates between runs.
- Keep zero first-axis stride paths outside the element loop.
- Keep type, operation, and missing-value choices outside the element loop.
- Treat each mapped location as the beginning of the current run.
- Advance through the terminal point carry so the point and locations return
  to zero after a complete traversal.

Do not return to element-at-a-time iteration or callback-based element work.

## What goes

Remove these types:

```c
struct rray_strided_iterator;
struct rray_strided_iterator2;
```

Remove their constructors:

```c
rray_strided_iterator();
rray_strided_iterator2();
```

Remove the state accessors and state-based control functions:

```c
rray_strided_iterator_finished();
rray_strided_iterator_run_start();
rray_strided_iterator_location();
rray_strided_iterator_next();

rray_strided_iterator2_finished();
rray_strided_iterator2_run_start();
rray_strided_iterator2_location1();
rray_strided_iterator2_location2();
rray_strided_iterator2_next();
```

The plan run-size and run-stride accessors can remain initially. They are
hoisted before traversal and are not part of mutable state. A later cleanup can
decide whether direct plan field access is clearer.

## Target caller shape

A one-location worker should have this general shape:

```c
const r_ssize run_size = rray_strided_iterator_plan_run_size(plan);
const r_ssize run_stride = rray_strided_iterator_plan_run_stride(plan);

r_ssize run_start = 0;
r_ssize location = 0;
r_ssize v_point[RRAY_MAX_DIMENSIONALITY];

rray_strided_point_init(v_point, plan->dimensionality);

while (run_start != plan->size) {
  const r_ssize run_end = run_start + run_size;
  r_ssize loc = location;

  if (run_stride == 0) {
    for (r_ssize i = run_start; i < run_end; ++i) {
      v_out[i] = v_x[loc];
    }
  } else {
    for (r_ssize i = run_start; i < run_end; ++i) {
      v_out[i] = v_x[loc];
      loc += run_stride;
    }
  }

  run_start = run_end;
  location = rray_strided_next_location(location, v_point, plan);
}
```

The two-location form should use independent scalar locations:

```c
r_ssize run_start = 0;
r_ssize location1 = 0;
r_ssize location2 = 0;
r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
```

The inner loop must continue using local copies such as `x_loc` and `y_loc`.
The persistent locations still mean the beginning of the current run.

## Point initialization

Axis zero is processed by the inner run and is never read by the point carry.
Only later active axes need initialization.

The first implementation should use a small inline helper with behavior
equivalent to:

```c
static inline void rray_strided_point_init(
  r_ssize* v_point,
  int dimensionality
) {
  if (dimensionality > 1) {
    memset(
      v_point + 1,
      0,
      sizeof(*v_point) * (r_ssize) (dimensionality - 1)
    );
  }
}
```

Do not clear all `RRAY_MAX_DIMENSIONALITY` entries without checking the
generated code. The current iterator constructor clears the entire point array.
In the existing `rray4.so`, `rray_add_dbl_dbl()` calls `bzero` for 536 bytes at
worker entry.

That cost is small when paid once for a large operation. It is a poor default
for a future nested reduction that may restart an inner traversal once or more
for every output location. A one-dimensional inner plan should initialize no
point coordinates.

After a complete traversal, the terminal carry resets every active later point
coordinate and each persistent location to zero. A caller that immediately
reuses the same traversal storage can reset only `run_start`. An early exit does
not have this property and must explicitly reinitialize its state.

Do not add early-exit reuse until there is a caller that needs it.

## Centralizing the point carry

The point carry is easy to get subtly wrong. It should have one implementation
if that implementation preserves good generated code.

Start with a one-location inline helper that takes the current location by
value, mutates `v_point`, and returns the next location:

```c
static inline r_ssize rray_strided_next_location(
  r_ssize location,
  r_ssize* v_point,
  const struct rray_strided_iterator_plan* plan
);
```

Its body should preserve the current `rray_strided_iterator_next()` carry
logic, except that the caller updates `run_start`.

Start the two-location implementation with one inline helper that mutates the
two scalar locations through pointers:

```c
static inline void rray_strided_next_locations2(
  r_ssize* p_location1,
  r_ssize* p_location2,
  r_ssize* v_point,
  const struct rray_strided_iterator2_plan* plan
);
```

This keeps one shared point carry for the two mappings. Because it takes the
addresses of the location locals, inspect the generated assembly before
keeping it.

If either helper causes persistent locations to be stored to the stack between
runs, inline the carry directly in the traversal-generating definitions. There
are currently 11 explicit run loops across eight files. Repetition is less
harmful than a memory dependency in every carry-heavy loop.

Do not solve this with a general function pointer, an element callback, or a
new large state aggregate.

## Why this matters for `rray_mean_along()`

A mean is likely to use two immutable plans:

- An outer plan walks retained axes and produces base locations in `x`.
- An inner plan walks reduced axes and produces offsets from each base.

The inner traversal may run more than once for each output location. Real mean
needs a main sum, may need a scaled sum after overflow, and needs a correction
pass. Missing-value removal also needs a count.

The useful abstraction is the reusable inner plan. Its mutable state can be
plain locals near the numerical accumulators:

```c
r_ssize inner_run_start = 0;
r_ssize inner_location = 0;
r_ssize v_inner_point[RRAY_MAX_DIMENSIONALITY];
```

This makes reset cost visible. It also avoids returning or clearing a large
iterator value inside the outer loop.

Do not add `rray_mean_along()` as part of this change. Add a mean-shaped
benchmark that repeatedly walks a small inner plan so this design is tested
against its intended next use.

## Files to change

The iterator definitions and helpers are in:

- `src/strided-iterator.h`
- `src/decl/strided-iterator-decl.h`

Current run loops are in:

- `src/arithmetic.h`
- `src/broadcast.c`
- `src/compare.c`
- `src/equal.c`
- `src/extremum.c`
- `src/permute-axes.c`
- `src/reduce.h`
- `src/split.c`

Update `plans/no-macros.md` after the implementation is measured. Preserve its
benchmark record, but revise statements that say the mutable iterator struct
must remain. The measured requirement is separation of fixed and mutable data.

## Implementation order

1. Record the current Git revision and benchmark the unchanged branch.
2. Add point initialization and one-location advance helpers.
3. Convert one-location loops in broadcast, permutation, and reduction.
4. Run tests and inspect one generated worker before continuing.
5. Remove the one-location mutable iterator type and its functions.
6. Add the two-location advance helper.
7. Convert arithmetic, comparison, equality, extrema, and split loops.
8. Run tests and inspect arithmetic, extrema, and split workers.
9. Remove the two-location mutable iterator type and its functions.
10. Add or restore a focused mean-shaped restart benchmark.
11. Run the full benchmark matrix against the recorded baseline.
12. Revise `plans/no-macros.md` with the result.

Keep each intermediate state buildable. Do not mix this work with operation,
type, casting, or missing-value behavior changes.

## Correctness checks

Run the full package tests:

```sh
Rscript -e "devtools::test()"
```

Pay particular attention to shapes that require later-axis carries:

- Alternating singleton axes.
- Broadcasts that remain three-dimensional after coalescing.
- Reductions over middle axes.
- Matrix transposition.
- Both split traversal directions.
- Zero-size arrays.
- Maximum supported dimensionality.

The terminal carry must leave point coordinates and persistent locations at
zero. Add a focused internal test only if this invariant is exposed through a
testable helper or a bug is found. Existing operation tests should remain the
main behavior coverage.

## Performance checks

Run the same broad suites used by the macro-free work for both baseline and
candidate:

```sh
RRAY_BENCH_ITERATIONS=50 Rscript bench/stride-zero.R
RRAY_BENCH_ITERATIONS=50 Rscript bench/extremum.R
RRAY_BENCH_ITERATIONS=50 Rscript bench/reduce-logical.R
RRAY_BENCH_ITERATIONS=50 Rscript bench/matrix-stats.R
Rscript bench/iterator.R
Rscript bench/broadcast.R
```

Use separate R processes, warm each case, alternate process order, and compare
medians. Rerun any difference that changes direction or is close to allocation
and garbage collection noise.

The mean-shaped benchmark should include:

- One-dimensional inner plans with no point carry.
- Two-dimensional inner plans with short first-axis runs.
- Many output locations with a small reduced slice.
- Few output locations with a large reduced slice.
- Two and three full passes over the same inner plan.
- Sparse and dense missing values.

The candidate should remain within normal benchmark noise for existing
operations. A repeatable regression above about 5 percent needs an assembly
explanation or a revised implementation.

## Assembly checks

Inspect representative typed workers after building the package:

- `rray_add_dbl_dbl`
- A carry-heavy broadcast worker.
- A double sum or product worker with missing-value calls.
- An extrema worker.
- Both split loop layouts.

Check for:

- Elimination or reduction of the full-size `bzero` call.
- Persistent run and location values held in registers.
- No location stores between ordinary point carries.
- No new spills around `R_IsNA()` calls.
- No element-level point carry checks.
- No operation or missing-value branches inside the element loop.
- Unchanged fixed zero-stride paths.
- Similar or smaller text section sizes.

Direct locals are not automatically faster. The change is successful if they
make the state model simpler without losing the generated loop properties.

## Formatting and review

After changing C code, run:

```sh
clang-format -i src/*.c src/*.h
air format .
```

Do the required protection pass over every touched `r_obj*`. This work should
not change any protection lifetime, but each touched worker still needs an
explicit check.

Do not add source comments. Existing comments stay unchanged unless removing
the iterator types makes them false. Explain performance decisions in the plan
and final response.

## Completion criteria

The work is complete when:

- Both mutable iterator structs and their APIs are gone.
- Every caller uses local mutable traversal state.
- Plan construction and axis coalescing are unchanged.
- The full test suite passes.
- C and R formatting have been run.
- The protection review is complete.
- Existing benchmarks show no meaningful regression.
- The mean-shaped restart benchmark shows no fixed 64-axis clearing cost for a
  one-dimensional inner traversal.
- Representative assembly keeps run counters and locations in registers.
- `plans/no-macros.md` records the final result.

A suitable commit message is:

```text
Inline mutable strided traversal state
```
