# Optimizing `rray_index()`

## Status

This document is an implementation plan. It does not describe code that is
already present. Every number in it comes from a throwaway proof of concept
that was measured and then discarded, so the code sketches below are known to
compile, pass the full test suite, and produce the stated timings.

The work is split into four steps. Steps 1 and 2 are measured. Steps 3 and 4
are not, and are written as proposals with an expected direction rather than a
promised number.

## Why

`rray_index()` reads one value from `x` for every point in the common
dimensions of the coordinate arrays. The inner loop lives in `src/index.c` and
is built from three pieces:

- `rray_index_plan()` at `src/index.c:199`, which fills a stride table.

- `rray_index_plan_location()` at `src/index.c:256`, which turns one set of
  coordinates into one location in `x`.

- `rray_index_plan_next()` at `src/index.c:275`, which advances the point and
  every coordinate array location.

This is the same algorithm numpy uses for the equivalent operation. numpy's
version is `mapiter_get()` in
`numpy/_core/src/multiarray/lowlevel_strided_loops.c.src`, reached from
`array_subscript()` in `numpy/_core/src/multiarray/mapping.c`. There are no
gather intrinsics, no blocking, and no parallelism on either side. The gap
between us is entirely in how much the compiler is allowed to specialize.

Measured on the same machine, source arrays small enough to stay in cache so
that gather latency does not dominate:

| Case | numpy | rray today |
|---|---|---|
| 1-d source, 1 coordinate array | 0.94 ns/elt | 3.62 ns/elt |
| 2-d source, 2 coordinate arrays | 3.38 ns/elt | 4.51 ns/elt |
| 3-d source, 3 coordinate arrays | 4.22 ns/elt | 6.22 ns/elt |

numpy's indices are 8 byte `intp` and ours are 4 byte `int`, so we read half
the index bytes and are still behind.

## Measurement harness

Rebuild this before changing anything. Every case produces about 1048576
output elements, and the source array is deliberately small in most rows so
that the loop cost is visible rather than the memory latency.

```r
n <- 1048576L
a <- sample(10L, n, replace = TRUE)
b <- sample(10L, n, replace = TRUE)
square <- c(1024L, 1024L)
a2 <- array(a, square)
b2 <- array(b, square)

x2_small <- array(1:100, c(10L, 10L))

rray_index(x2_small, a2, b2)
```

The cases that matter are, in order: a 1-d source with one coordinate array, a
2-d source with two square coordinate arrays, a Cartesian broadcast of
`[1024, 1]` against `[1, 1024]`, 3-d and 4-d sources with matching coordinate
counts, and the same 2-d case with coordinate arrays reshaped to 10 and 20
axes.

Two things to know before trusting any number from it.

Run to run noise on the `ns/elt` rows is within 3%. Noise on the small call
rows is up to 16%, so do not read anything into a small call change under 20%.

Passing bare integer vectors as coordinates is measurably slower than passing
1-d arrays holding identical data, 0.91 ns/elt against 0.57 ns/elt for the
validation pass alone. A pre-wrapped ALTREP array is exactly as fast as a plain
array, which rules out ALTREP element access and points at the per-call
`vec_as_array()` wrapper. The cause is not confirmed and GC pressure is the
leading hypothesis. It is not part of this plan, but benchmarks will disagree
with each other if some pass vectors and some pass arrays, so pick one.

## What the proof of concept measured

Five builds. `specialize` is step 1 limited to 1 and 2 coordinate arrays,
`specialize 1-4` extends it to 3 and 4. All values are ns/elt except the last
two rows, which are us/call.

| Case | baseline | coalesce | specialize | both | both, 1-4 |
|---|---|---|---|---|---|
| 1d/1 large src | 4.51 | 4.56 | 3.78 | 3.56 | 3.69 |
| 1d/1 small src | 3.62 | 3.47 | 3.06 | 3.05 | 2.95 |
| 2d/2 large src | 4.65 | 4.64 | 3.28 | 3.32 | 3.31 |
| 2d/2 small src | 4.51 | 4.39 | 3.29 | 3.24 | 3.25 |
| 2d/2 flat coords | 5.30 | 5.15 | 4.13 | 3.86 | 4.01 |
| 2d/2 cartesian | 3.33 | 3.19 | 2.13 | 2.11 | 2.12 |
| 3d/3 small src | 6.22 | 5.94 | 6.19 | 5.91 | 3.88 |
| 4d/4 small src | 7.35 | 7.06 | 7.37 | 7.04 | 4.48 |
| 2d/2 coords 10 axes | 5.25 | 4.42 | 3.06 | 3.27 | 3.30 |
| 2d/2 coords 20 axes | 6.77 | 4.40 | 3.46 | 3.29 | 3.24 |
| small call | 1.52 | 1.60 | 1.63 | 1.44 | 1.50 |
| small call 2d | 1.78 | 1.78 | 1.79 | 1.73 | 1.81 |

Four conclusions come out of this.

Specialization is the whole story. It is worth 20% to 36% on every case it
covers, and the cases it does not cover are exactly the ones where the switch
falls through to the generic path.

Coalescing on its own only helps deep point spaces. It is worth 16% at 10 axes
and 35% at 20 axes, and nothing at all anywhere else. That matches what it
does, which is shorten the carry chain.

Once specialization is in, coalescing adds nothing measurable. The 10 and 20
axis rows land at about 3.3 ns/elt either way. The carry was expensive because
its inner loop over the coordinate arrays could not unroll, not because of the
axis walking itself.

Specializing 3 and 4 coordinate arrays is worth as much as 1 and 2. The 3-d
case drops 34% and the 4-d case drops 36%, and neither moves at all until they
are covered.

Neither change touches small call overhead, which is a separate problem
described at the end.

## Step 1: specialize on the coordinate array count

This is the whole win and it does not depend on any other step. Do it first.

`rray_index_plan_location()` and `rray_index_plan_next()` both loop over
`plan->x_dimensionality`, which is a runtime value, so the compiler cannot
unroll either one. numpy solves this by generating `mapiter_get()` twice, once
with the operand count replaced by a literal `1` and once with the runtime
value.

Take the count as a parameter instead of reading it from the plan:

```c
static inline r_ssize rray_index_plan_location(
  const struct rray_index_plan* plan,
  const r_ssize* v_index_locations,
  const int x_dimensionality
);

static inline void rray_index_plan_next(
  const struct rray_index_plan* plan,
  int* v_point,
  r_ssize* v_index_locations,
  const int x_dimensionality
);
```

Then split the body of each iteration macro out so the loop can be emitted once
per literal:

```c
#define RRAY_INDEX_ATOMIC_LOOP(MISSING, X_DIMENSIONALITY)                      \
  for (r_ssize i = 0; i < plan->size; ++i) {                                   \
    const r_ssize location =                                                   \
      rray_index_plan_location(plan, v_index_locations, X_DIMENSIONALITY);     \
    v_out[i] = location == -1 ? MISSING : v_x[location];                       \
    rray_index_plan_next(plan, v_point, v_index_locations, X_DIMENSIONALITY);  \
  }
```

And dispatch on the real count inside `RRAY_INDEX_ATOMIC()` at
`src/index.c:299`, with the same shape for `RRAY_INDEX_BARRIER()` at
`src/index.c:316`:

```c
  switch (plan->x_dimensionality) {                                            \
  case 1:                                                                      \
    RRAY_INDEX_ATOMIC_LOOP(MISSING, 1);                                        \
    break;                                                                     \
  case 2:                                                                      \
    RRAY_INDEX_ATOMIC_LOOP(MISSING, 2);                                        \
    break;                                                                     \
  case 3:                                                                      \
    RRAY_INDEX_ATOMIC_LOOP(MISSING, 3);                                        \
    break;                                                                     \
  case 4:                                                                      \
    RRAY_INDEX_ATOMIC_LOOP(MISSING, 4);                                        \
    break;                                                                     \
  default:                                                                     \
    RRAY_INDEX_ATOMIC_LOOP(MISSING, plan->x_dimensionality);                   \
    break;                                                                     \
  }                                                                            \
```

Stop at 4. Each case is a full copy of the loop body for all seven storage
types across both macros, so the object file grows quickly, and the
proof of concept shows the returns are already flat by 4.

## Step 2: coalesce adjacent axes n ways

Do this second, and be honest in the pull request that the measured benefit
after step 1 is confined to point spaces with many small axes.

The rule is already factored out as
`rray__strided_iterator_axes_coalescible()` in `src/strided-iterator.h`, and
`rray__strided_iterator_axes_coalesce2()` is the two operand driver. Index needs
the same driver with a runtime operand count, so add a third sibling next to
them rather than writing the rule a third time:

```c
static inline int rray__strided_iterator_axes_coalescen(
  r_ssize* v_dimensions,
  r_ssize (*v_strides)[RRAY_MAX_DIMENSIONALITY],
  int dimensionality,
  int count
) {
  int out_axis = 0;

  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize left_dimension = v_dimensions[out_axis];
    const r_ssize right_dimension = v_dimensions[axis];

    bool coalescible = true;

    for (int i = 0; i < count; ++i) {
      if (!rray__strided_iterator_axes_coalescible(
            left_dimension,
            v_strides[out_axis][i],
            right_dimension,
            v_strides[axis][i]
          )) {
        coalescible = false;
        break;
      }
    }

    if (coalescible) {
      if (left_dimension == 1) {
        for (int i = 0; i < count; ++i) {
          v_strides[out_axis][i] = v_strides[axis][i];
        }
      }
      v_dimensions[out_axis] = left_dimension * right_dimension;
    } else {
      ++out_axis;
      v_dimensions[out_axis] = right_dimension;
      for (int i = 0; i < count; ++i) {
        v_strides[out_axis][i] = v_strides[axis][i];
      }
    }
  }

  return out_axis + 1;
}
```

The call site is one statement at the end of `rray_index_plan()`:

```c
  plan.dimensionality = rray__strided_iterator_axes_coalescen(
    plan.v_dimensions,
    plan.v_index_strides,
    dimensionality,
    x_dimensionality
  );
```

Three things happen to already be right, which is why this drops in cleanly.

`v_index_strides[axis][i]` is indexed point axis first, so each axis is a
contiguous row. Comparing two axes is a row scan and merging is a row copy. No
layout change is needed.

`plan.v_dimensions` is already `r_ssize` rather than `int`, so merged
dimensions cannot overflow.

`plan.dimensionality` is only ever read by `rray_index_plan_next()`. The result
dimensions come from the separate `dimensions` object poked on at
`src/index.c:91`, and the output is written contiguously at `i`. So the plan's
dimensionality is already a pure iteration concept and shrinking it is
invisible to callers.

The output itself never needs checking. It is contiguous over the point space
by construction, so it always coalesces.

One limit is permanent and worth stating in the pull request. numpy runs
`npyiter_find_best_axis_ordering()` before coalescing, sorting axes by stride
so more pairs become adjacent and mergeable. We cannot. numpy allocates the
result through its iterator and is free to pick any point order, while we write
into a contiguous result whose dimensions are fixed by the common coordinate
dimensions. We merge adjacent axes only and lose the non-adjacent cases.

## Step 3: hoist the missing value check

Not measured. Expected to help most on the 1-d case, which is where the largest
remaining gap to numpy sits.

`rray_index_plan_location()` returns `-1` when any coordinate is missing, so
every element carries a data dependent branch per coordinate array plus a
ternary on the result. numpy has no missing value concept and no equivalent
cost.

`rray_as_index_array()` already walks every coordinate array to validate it, so
it can record whether that array contained any `NA` for free. When no
coordinate array contains one, run a variant of the loop with the check and the
sentinel removed entirely.

This composes with step 1 rather than replacing it. The specialized cases
become straight line code with no branches at all.

## Step 4: flat run loop for a single coalesced axis

Not measured. This is what closes the rest of the distance to
`mapiter_trivial_get()`, which is the path that gives numpy its 0.94 ns/elt.

After step 2, the common case of identically shaped coordinate arrays collapses
to one axis with every `v_index_strides[0][i]` equal to 1. At that point the
point vector is dead weight: the location of coordinate array `i` is just the
output index. A dedicated loop for that case drops `v_point` and
`v_index_locations` and reads `plan->v_indices[k][i]` directly.

Combined with steps 1 and 3, the one coordinate array case reduces to numpy's
trivial loop with no iterator state at all:

```c
for (r_ssize i = 0; i < plan->size; ++i) {
  v_out[i] = v_x[v_index[i] - 1];
}
```

Check the unit stride condition once when building the plan and store it as a
flag, rather than testing strides inside the loop.

## Out of scope

Small call overhead is a separate problem and neither step above moves it.
`rray_index()` on a 10 element coordinate array costs about 1.5 us against
numpy's 0.10 us. About 0.5 us of that is the floor for any rray R function,
since `rray_dimensions()` alone costs 0.49 us per call, and
`rray_dimensions_common()` accounts for roughly another 0.5 us. The loop is not
involved. If this matters it deserves its own plan.

The bare vector against 1-d array difference described in the harness section
is also out of scope, but should be understood before anyone benchmarks this
code again.

## Testing

Every step here must leave results bit identical, so the existing
`tests/testthat/test-index.R` is the primary check and the full suite must pass
unchanged. The proof of concept was verified this way, including a digest of
seven differently shaped results compared across all five builds.

Add these alongside the existing index array tests.

Coalescing needs an equivalence test that pins the reshape invariant directly.
Identical coordinate data shaped `[1048576]` and `[2 x 20]` must produce
identical values, because that pair is exactly what coalescing collapses.

Both steps need coverage at the edges of the dispatch. Cover 1, 2, 3, 4, and 5
coordinate arrays so that every specialized case and the generic fallback are
exercised, and keep the existing 64 axis maximum tests.

Zero dimensions need to stay covered for coalescing. A dimension of 0 makes the
size 0 and the loop never runs, but the merge rule still executes, so the
existing zero dimension tests should be checked rather than assumed.
