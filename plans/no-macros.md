# Replace strided iteration macros with a run cursor

Not built. This document records a working experiment in this branch and the
rules needed to turn it into a mergeable feature.

The experiment removes `RRAY_STRIDED_ITERATOR_FOR_EACH()` and
`RRAY_STRIDED_ITERATOR2_FOR_EACH()` from `src/strided-iterator.h`. It replaces
them with a plan plus cursor API. All existing callers were ported and the test
suite passes. The current working tree is deliberately left uncommitted so a
future agent can inspect, revise, or discard it.

The central constraint is performance. A natural element-at-a-time iterator
regresses because it performs row-boundary work for every element. The API must
therefore expose first-axis runs. Callers own the outer loop and retain a small,
simple inner loop.

## Goal

Replace the strided iteration macros with explicit state and functions that
allow callers to control traversal. Callers should be able to stop early,
choose their own inner loop, nest iteration, and make the iteration state clear
at the call site.

The API must retain the existing properties:

- coalesce compatible axes before traversing them;
- process the coalesced first axis as one run;
- keep point carry work outside the element loop;
- keep zero first-axis strides visible as fixed paths in caller code;
- support one and two mapped location spaces;
- stay within normal benchmark noise for the present macro implementation.

## The API

An iterator is an immutable traversal plan. A cursor is the mutable state for
one pass over that plan. Separating them is important. A caller commonly
receives a pointer to a plan, then creates a stack-local cursor. Clang can keep
the cursor's changing fields in registers much more readily than fields stored
through the plan pointer.

```c
struct rray_strided_iterator {
  r_ssize size;
  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  int dimensionality;
  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];
};

struct rray_strided_iterator_cursor {
  const struct rray_strided_iterator* iterator;
  r_ssize index;
  r_ssize location;
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
};

struct rray_strided_iterator2 {
  r_ssize size;
  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  int dimensionality;
  r_ssize v_strides1[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_strides2[RRAY_MAX_DIMENSIONALITY];
};

struct rray_strided_iterator2_cursor {
  const struct rray_strided_iterator2* iterator;
  r_ssize index;
  r_ssize location1;
  r_ssize location2;
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
};
```

Keep the existing constructors and coalescing logic:

```c
struct rray_strided_iterator rray_strided_iterator(...);
struct rray_strided_iterator rray_broadcast_iterator(...);

struct rray_strided_iterator2 rray_strided_iterator2(...);
struct rray_strided_iterator2 rray_broadcast_iterator2(...);
```

Add these `static inline` functions for one mapped location:

```c
struct rray_strided_iterator_cursor rray_strided_iterator_begin(
  const struct rray_strided_iterator* iterator
);

bool rray_strided_iterator_finished(
  const struct rray_strided_iterator_cursor* cursor
);

r_ssize rray_strided_iterator_index(
  const struct rray_strided_iterator_cursor* cursor
);

r_ssize rray_strided_iterator_location(
  const struct rray_strided_iterator_cursor* cursor
);

r_ssize rray_strided_iterator_run_size(
  const struct rray_strided_iterator_cursor* cursor
);

r_ssize rray_strided_iterator_run_stride(
  const struct rray_strided_iterator_cursor* cursor
);

void rray_strided_iterator_next(
  struct rray_strided_iterator_cursor* cursor
);
```

The two-location form has the same shape:

```c
struct rray_strided_iterator2_cursor rray_strided_iterator2_begin(
  const struct rray_strided_iterator2* iterator
);

bool rray_strided_iterator2_finished(
  const struct rray_strided_iterator2_cursor* cursor
);

r_ssize rray_strided_iterator2_index(
  const struct rray_strided_iterator2_cursor* cursor
);

r_ssize rray_strided_iterator2_location1(
  const struct rray_strided_iterator2_cursor* cursor
);

r_ssize rray_strided_iterator2_location2(
  const struct rray_strided_iterator2_cursor* cursor
);

r_ssize rray_strided_iterator2_run_size(
  const struct rray_strided_iterator2_cursor* cursor
);

r_ssize rray_strided_iterator2_run_stride1(
  const struct rray_strided_iterator2_cursor* cursor
);

r_ssize rray_strided_iterator2_run_stride2(
  const struct rray_strided_iterator2_cursor* cursor
);

void rray_strided_iterator2_next(
  struct rray_strided_iterator2_cursor* cursor
);
```

`index()` and each `location()` report the first element of the current run.
`run_size()` is the coalesced first-axis dimension. `next()` advances through
the entire current run and positions the cursor at the next one. It does not
step one element.

This is intentional. An element-wise `next()` would have to test for the end
of a run and possibly carry into later axes on every element. The existing
macro avoids exactly that work.

## Cursor behaviour

`begin()` sets `index` and every location to zero, clears `v_point`, and stores
the plan pointer. `finished()` is true when `index == iterator->size`.

`next()` must do this in order:

1. Add `run_size()` to `cursor->index`.
2. Return if the new index equals the plan size.
3. Starting at axis 1, increment the point coordinate.
4. If the coordinate is still within that axis dimension, add that axis stride
   to each location and return.
5. Otherwise reset the coordinate to zero, subtract the completed extent times
   each stride, and continue to the next axis.

Do not subtract `run_size() * run_stride()` inside `next()`. The cursor stores
the beginning of the current run. The caller increments a local location in
its inner loop, so the cursor location has not moved along that run.

The first version of this experiment did subtract that amount. It produced
wrong results for broadcasts after the first run. The full test suite caught
the error.

## Required caller shape

For a one-location atomic operation, use this shape:

```c
struct rray_strided_iterator_cursor cursor =
  rray_strided_iterator_begin(iterator);

for (
  ; !rray_strided_iterator_finished(&cursor)
  ; rray_strided_iterator_next(&cursor)
) {
  const r_ssize index = rray_strided_iterator_index(&cursor);
  const r_ssize end = index + rray_strided_iterator_run_size(&cursor);
  const r_ssize location = rray_strided_iterator_location(&cursor);
  const r_ssize stride = rray_strided_iterator_run_stride(&cursor);

  if (stride == 0) {
    for (r_ssize i = index; i < end; ++i) {
      v_out[i] = v_x[location];
    }
  } else {
    for (
      r_ssize i = index, location_ = location
      ; i < end
      ; ++i, location_ += stride
    ) {
      v_out[i] = v_x[location_];
    }
  }
}
```

For two locations, branch before the inner loop on the four stride pairs:

```c
if (stride1 == 0) {
  if (stride2 == 0) {
    // Both locations fixed.
  } else {
    // Location 1 fixed, location 2 advances.
  }
} else if (stride2 == 0) {
  // Location 1 advances, location 2 fixed.
} else {
  // Both locations advance.
}
```

The bodies look repetitive. Do not reduce that repetition by moving a
stride-dependent branch into the inner loop. The exact loop body is part of
the performance contract.

## `rray_mean_along()` motivates nested reduction traversal

The current reduction plan walks `x` in its physical order and maps every
element to an output location. That is a good fit for `sum()` and `product()`,
but a mean needs more state. R accumulates means in `LDOUBLE`, uses a scaled
sum when the first sum overflows, and makes a correction pass for rounding
error.

`RRAY_REDUCE()` cannot express that state because its R output vector is also
its accumulator. A custom mean could use the current iterator macro with
`long double` buffers indexed by output location. That works, but it needs a
sum buffer, a count buffer for `na_rm`, and state for the correction pass.

A better reduction-specific traversal visits one complete reduced slice for
each output location. It needs two immutable strided plans:

| Plan | Axes | Location |
|---|---|---|
| Outer | Retained axes, in their original order | The base location in `x` |
| Inner | Reduced axes, in their original order | An offset from that base |

Both plans use the physical strides of `x`. This is a virtual axis permutation,
not a copy or a physical permutation of `x`. The outer cursor index is the
flat output location because collapsed axes have dimension 1.

The cursors still advance by runs. A mean must therefore walk every output
location in the current outer run before advancing the outer cursor:

```c
for (
  ; !rray_strided_iterator_finished(&outer)
  ; rray_strided_iterator_next(&outer)
) {
  r_ssize out_loc = rray_strided_iterator_index(&outer);
  const r_ssize out_end =
    out_loc + rray_strided_iterator_run_size(&outer);
  r_ssize x_base = rray_strided_iterator_location(&outer);
  const r_ssize x_stride = rray_strided_iterator_run_stride(&outer);

  for (; out_loc < out_end; ++out_loc, x_base += x_stride) {
    struct rray_strided_iterator_cursor inner =
      rray_strided_iterator_begin(&iterator.inner);
  }
}
```

For each output location, real mean can use scalar `long double` values for
the sum and correction, plus an `r_ssize` count when removing missing values.
It restarts the immutable inner plan for each numerical pass. The usual path
uses one sum pass and one correction pass. A first sum that is not finite uses
a scaled sum pass, then a correction pass when that scaled mean is finite.
Complex mean uses separate real and imaginary accumulators.

This removes the need for per-output `long double` state. An empty retained
axis set represents one output location. An empty reduced axis set represents
one input value per output location. A zero-size reduced slice produces `NaN`.
The grouped order may read a later or middle axis with a stride, so benchmark it
against the existing input-major reduction before using it for other reducers.

## Why zero strides need explicit paths

After coalescing, a zero first-axis stride means one input or output location
is fixed over the whole inner run. This occurs when broadcasting along the
contiguous direction and when reducing into a fixed output location.

If the stride remains a runtime value in the inner loop, Clang cannot prove
that the load or store is fixed. On Apple Silicon it can select a scalar path
instead of vectorizing. A visible `if (stride == 0)` lets each loop body omit
the location increment and lets fixed loads hoist.

For iterator2, preserve all four paths. The old macro had three dispatch arms,
but an explicit fourth both-zero path is clearer and keeps both locations fixed
in source.

Relevant shapes include:

- scalar to array broadcasting;
- row broadcasting, such as `[1, n]` into `[m, n]`;
- higher-dimensional broadcasts that coalesce to `[rows, runs]` with a zero
  first stride;
- reductions over the first axis;
- `rray_split()` layouts where either output-list or output-element location is
  fixed across a run.

## Operation dispatch must stay outside every inner loop

This is the most important lesson from the experiment.

`equal.c` originally chose equal versus not-equal before traversal. `extremum.c`
originally chose missing-value propagation versus removal before traversal.
The first port used conditional expressions inside every element loop to avoid
duplicating the four stride paths. That was wrong.

The dedicated benchmark results from that version were:

- equality and inequality: 34 to 35% slower;
- extrema: median 36% slower across 96 cases;
- some extrema cases: 2.8 to 9.1 times slower.

The fixed port restores an outer `if` and calls a type-generation macro for
one selected operation. Its generated inner loops have no `op` or `na_rm`
check. A final implementation can duplicate source instead if that better
fits the no-macro goal, but it must preserve this generated control flow.

The same rule applies to arithmetic operation dispatch, type conversion,
missing-value handling, output representation, and any other value that stays
constant for an operation call.

## Current migration coverage

The experiment ports these files:

- `src/arithmetic.h`
- `src/broadcast.c`
- `src/compare.c`
- `src/equal.c`
- `src/extremum.c`
- `src/permute-axes.c`
- `src/reduce.h`
- `src/split.c`
- `src/strided-iterator.h`

No `RRAY_STRIDED_ITERATOR_FOR_EACH()` or
`RRAY_STRIDED_ITERATOR2_FOR_EACH()` call remains. Existing type-generation
macros remain. They are separate from the strided iteration API.

## Benchmark record

All benchmark comparisons below use the main checkout as baseline. They were
run as separate R processes on the same host. Allocation and garbage collection
make individual minima noisy, so compare medians and rerun suspicious cases.

### Tests

```r
devtools::test()
```

Result: 1,512 passing tests.

### Dedicated stride-zero suite

Command for each checkout:

```sh
RRAY_BENCH_ITERATIONS=50 Rscript bench/stride-zero.R
```

This covers arithmetic, comparison, equality, extrema, direct broadcast,
numeric and logical reductions, and both `rray_split()` iterator2 layouts.

With the corrected equality and extrema ports, 39 case medians gave:

| Summary | Candidate / baseline |
|---|---:|
| Minimum | 0.57x |
| First quartile | 0.88x |
| Median | 0.92x |
| Third quartile | 0.98x |
| Maximum | 1.05x |

The slowest cases were:

| Case | Candidate / baseline |
|---|---:|
| iterator2 split with first location fixed | 1.052x |
| alternating inner broadcast, right | 1.048x |
| alternating inner broadcast, left | 1.043x |
| alternating outer broadcast | 1.022x |

The numeric and logical reduction cases were at or within 0.5% of baseline.
Equality and inequality were about 0.92x baseline after moving dispatch out of
the hot loop.

### Exhaustive extrema suite

Command for each checkout:

```sh
RRAY_BENCH_ITERATIONS=20 Rscript bench/extremum.R
```

This is 96 cases: `pmax` and `pmin`, integer and double, both `na_rm` values,
three missing-value layouts, and four traversal layouts.

With the corrected port:

| Summary | Candidate / baseline |
|---|---:|
| Minimum | 0.41x |
| First quartile | 0.96x |
| Median | 1.01x |
| Third quartile | 1.06x |
| Maximum | 3.26x |

The maximum comes from a short noisy case and needs a focused rerun before it
is treated as a real regression. The 75th percentile is the useful signal from
this first pass. Any future implementation should rerun the slowest cases at
50 or 100 iterations with a warm-up and inspect their distributions.

## Benchmark work still needed

Before merging a final implementation, run the following for both the baseline
and candidate, saving RDS output for comparison:

```sh
RRAY_BENCH_ITERATIONS=50 Rscript bench/stride-zero.R
RRAY_BENCH_ITERATIONS=50 Rscript bench/extremum.R
RRAY_BENCH_ITERATIONS=50 Rscript bench/reduce-logical.R
RRAY_BENCH_ITERATIONS=50 Rscript bench/matrix-stats.R
Rscript bench/iterator.R
Rscript bench/broadcast.R
```

Pay particular attention to:

- scalar and row broadcasting;
- alternating singleton axes in six dimensions;
- matching leading singleton axes that coalesce;
- short first-axis runs, including `[2, 500000]`;
- contiguous controls;
- reductions with fixed output locations;
- logical reductions with sparse and dense missing values;
- integer and double extrema with both missing-value policies;
- both iterator2 split directions;
- tiny arrays, where cursor setup can dominate;
- large arrays, where vectorization matters most.

For any result beyond a small regression, inspect generated assembly or reduce
the benchmark to a standalone C harness. Check that the selected inner loop
has no runtime operation branch, no element-level point carry, and no unknown
zero stride.

## Implementation checklist

1. Start from the current experimental diff or reimplement the plan and cursor
   types in `src/strided-iterator.h`.
2. Keep constructors responsible only for plan construction and axis
   coalescing.
3. Make `begin()`, accessors, and `next()` `static inline`.
4. Keep cursor state local to each caller function.
5. Port one-location callers with explicit zero and non-zero run paths.
6. Port two-location callers with four fixed stride paths.
7. Hoist every operation and missing-value decision outside traversal.
8. Run `clang-format -i src/*.c src/*.h`.
9. Do the required protection review. The iterator change itself should not add
   allocating `r_obj*` uses, but each touched caller still needs review.
10. Run `devtools::test()` and the benchmark matrix above.
11. Do not commit experimental implementation files unless the benchmark
    thresholds are agreed and met.

## Decisions still open

- Whether the API should be named `iterator` plus `cursor`, as here, or use a
  single public state object. The split is better for optimization and makes
  restartable plans explicit.
- Whether callers should use direct fields or accessors. Keep accessors in the
  first implementation because they document run semantics. Only expose fields
  if assembly shows a material cost after inlining.
- Whether to retain code-generation macros such as `RRAY_EQUALITY_IMPL()`.
  They are not strided iteration macros, but they avoid hand-writing each type
  combination. Do not use a generic callback or function pointer for element
  work.
- Whether the extrema outliers are benchmark noise or a real cursor cost. Rerun
  those cases before deciding.

## Do not do these things

- Do not implement one element per `next()` as the main API.
- Do not hide the body behind a callback or function pointer.
- Do not read an unknown zero stride inside a vectorization-sensitive loop.
- Do not put `op`, `na_rm`, or type dispatch inside the inner loop.
- Do not mutate the immutable plan while walking it.
- Do not make `next()` undo local location increments that happened only in the
  caller's loop.
- Do not judge performance from one median or one benchmark family.
