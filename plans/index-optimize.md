# Optimizing `rray_index()`

## Status

This document is an implementation plan. It does not describe code that is
already present. Every number in it comes from a throwaway proof of concept
that was measured and then discarded, so the code sketches below are known to
compile, pass the full test suite, and produce the stated timings.

The work is split into four steps. Steps 1, 2 and 3 are measured. Step 4 is
not, and is written as a proposal with an expected direction rather than a
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
that gather latency does not dominate. The last column is what the proof of
concept reached with steps 1 through 3 applied:

| Case | numpy | rray today | after steps 1-3 |
|---|---|---|---|
| 1-d source, 1 coordinate array | 0.94 ns/elt | 3.62 ns/elt | 1.85 ns/elt |
| 2-d source, 2 coordinate arrays | 3.38 ns/elt | 4.51 ns/elt | 2.10 ns/elt |
| 3-d source, 3 coordinate arrays | 4.22 ns/elt | 6.22 ns/elt | 2.87 ns/elt |
| 2-d Cartesian broadcast | 2.79 ns/elt | 3.33 ns/elt | 0.94 ns/elt |

numpy's indices are 8 byte `intp` and ours are 4 byte `int`, so we read half
the index bytes. After these steps we are ahead of numpy everywhere except the
single coordinate array case, where their dedicated `mapiter_trivial_get()`
path still wins by 2x.

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

Include a leading unit axis case where every coordinate array is shaped `[1, N]`
or `[1, 1, N]`. That is the only shape coalescing helps, and a grid without it
will make step 3 look pointless. The first version of this plan made that
mistake. Include the take-along-axis shapes too, since they are the ones a
reader will assume benefit and do not.

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

Seven builds, collapsed here to the five that matter. `specialize` is step 1
limited to 1 and 2 coordinate arrays. `spec 1-4 + coalesce` is steps 1 and 3
together. `+ run loop` adds step 2, and the last column removes coalescing
again to isolate what step 3 is actually contributing. All values are ns/elt
except the last two rows, which are us/call.

| Case | baseline | specialize | spec 1-4 + coalesce | + run loop | run loop, no coalesce |
|---|---|---|---|---|---|
| 1d/1 large src | 4.51 | 3.78 | 3.69 | 2.57 | 2.59 |
| 1d/1 small src | 3.62 | 3.06 | 2.95 | 1.85 | 1.69 |
| 2d/2 large src | 4.65 | 3.28 | 3.31 | 2.12 | 2.16 |
| 2d/2 small src | 4.51 | 3.29 | 3.25 | 2.10 | 2.07 |
| 2d/2 flat coords | 5.30 | 4.13 | 4.01 | 3.00 | 2.92 |
| 2d/2 cartesian | 3.33 | 2.13 | 2.12 | 0.94 | 0.96 |
| 3d/3 small src | 6.22 | 6.19 | 3.88 | 2.87 | 2.87 |
| 4d/4 small src | 7.35 | 7.37 | 4.48 | 3.76 | 3.76 |
| 2d/2 coords 10 axes | 5.25 | 3.06 | 3.30 | 2.10 | 2.66 |
| 2d/2 coords 20 axes | 6.77 | 3.46 | 3.24 | 2.10 | 3.26 |
| small call | 1.52 | 1.63 | 1.50 | 1.54 | 1.45 |
| small call 2d | 1.78 | 1.79 | 1.81 | 1.76 | 1.73 |

Five conclusions come out of this.

Specialization is worth 20% to 36% on every case it covers, and the cases it
does not cover are exactly the ones where the switch falls through to the
generic path. Specializing 3 and 4 coordinate arrays is worth as much as 1 and
2, since the 3-d case drops 34% and the 4-d case drops 36%, and neither moves
at all until they are covered.

The run loop is the single largest win, a further 24% to 56% on top of
specialization. It is also what finally puts us ahead of numpy on everything
but the one coordinate array case.

Coalescing does not speed up ordinary shapes. It removes a pathology. Compare
the last two columns of the table above. They are identical within noise
everywhere except the 10 and 20 axis rows. For coordinate arrays shaped
`[1024, 1024]` the first axis is already 1024 long, so the run loop gets a long
inner run either way and the carry touches 0.1% of elements. Coalescing has
nothing left to give.

The pathology it removes is a short or unit first axis, which makes every inner
run one element long and turns the run loop back into the per element carry it
was meant to replace. A separate grid covers it, again with the step 2 run loop
in place:

| Coordinate array shape | coalesce | no coalesce | coalesce wins |
|---|---|---|---|
| `[N]` | 2.17 | 2.18 | 0% |
| `[1, N]` | 2.19 | 3.53 | 38% |
| `[1, 1, N]` | 2.19 | 4.05 | 46% |
| `[1, 1024, 1024]` | 2.18 | 3.48 | 37% |
| `[2, N/2]` | 2.18 | 2.78 | 22% |

Read the first column. Every shape lands at 2.17 to 2.19 ns/elt, identical to
the flat case. That is the real property coalescing buys: the operation costs
the same regardless of how the coordinate arrays happen to be shaped. Without
it, writing coordinates as a row vector rather than a plain vector costs 60%.

This matches the original coalescing work exactly. Its plan, `plan/coalesce.md`
as of commit `f3f2063`, measured 3.03x to 5.20x and every case it reported had
a first axis of 1. It closed with the warning not to claim that coalescing
makes every operation faster, and recorded that cases which could not gain a
longer first axis run were unchanged. The 3-5x in the `src/strided-iterator.h`
header comes from those leading unit axis cases, not from general shapes.

The condition is strict, and narrower than the table above suggests on its own.
Every coordinate array must have dimension 1 on the leading axis, which is the
same as saying the result's leading dimension is 1. The rule keys off the point
dimension, and the point dimensions are the common dimensions of the coordinate
arrays, so one array with a leading dimension above 1 is enough to stop the
merge for all of them:

| Case | coalesce | no coalesce | coalesce wins |
|---|---|---|---|
| all coordinate arrays `[1, N]` | 2.23 | 3.55 | 37% |
| mixed `[2, N/2]` and `[1, N/2]` | 2.53 | 2.49 | 0% |
| take along axis 2, `[1024, 1]` and `[1, 1024]` | 0.97 | 0.99 | 0% |
| take along first axis, J = 2 | 1.82 | 1.84 | 0% |
| take along first axis, J = 8 | 1.17 | 1.20 | 0% |

The take rows matter because `rray_index_axis()` is a planned function and it
will not benefit. The equivalence in `plans/index.md` spells out its coordinate
arrays for source dimensions `(A, B, C)` and `axis = 2`:

```r
axis1 <- array(seq_len(A), c(A, 1L, 1L))
axis2 <- i
axis3 <- array(seq_len(C), c(1L, 1L, C))
```

`axis3` does have a leading unit axis, but `axis1` has a leading dimension of
`A`, so the point space leads with `A` and the two arrays vary along different
axes. Crossed strides never coalesce. Note this measures the coordinate array
equivalent, which is what goes through this machinery. A native
`rray_index_axis()` implementation could iterate differently.

Take shapes are also already the fastest cases measured, 0.97 to 1.82 ns/elt,
because one coordinate array is tiny and gets reused across the whole result.
They do not need help from this step.

Nothing here touches small call overhead, which is a separate problem described
at the end.

One caveat on attribution. The run loop was only ever measured on top of
specialization, so the split between steps 1 and 2 is not isolated. The order
below is the order they were measured in.

## Step 1: specialize on the coordinate array count

Do this first. It does not depend on any other step, and step 2 builds directly
on the macro split it introduces.

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

## Step 2: walk the first axis as a run

This is the largest single win and the piece `src/index.c` was missing all
along. The rest of the package already works this way. `src/strided-iterator.h`
documents it under "First axis runs" and "Fixed zero stride paths", and index
was the one operation still carrying the point vector on every element.

Pull the first axis out into an inner run, and only advance the later axes
between runs. Add a sibling to `rray_index_plan_next()` that skips axis 0:

```c
static inline void rray_index_plan_next_run(
  const struct rray_index_plan* plan,
  int* v_point,
  r_ssize* v_index_locations,
  const int x_dimensionality
) {
  for (int axis = 1; axis < plan->dimensionality; ++axis) {
    ++v_point[axis];

    if (v_point[axis] < plan->v_dimensions[axis]) {
      for (int i = 0; i < x_dimensionality; ++i) {
        v_index_locations[i] += plan->v_index_strides[axis][i];
      }
      break;
    }

    v_point[axis] = 0;

    for (int i = 0; i < x_dimensionality; ++i) {
      v_index_locations[i] -=
        (plan->v_dimensions[axis] - 1) * plan->v_index_strides[axis][i];
    }
  }
}
```

Inside a run, coordinate array `k` advances by the fixed stride
`plan->v_index_strides[0][k]`. Emit the run body twice, once with that stride
and once with a literal 1, exactly as the existing iterators pass a literal 0
for fixed strides:

```c
#define RRAY_INDEX_STRIDE_ONE(K) 1
#define RRAY_INDEX_STRIDE_RUNTIME(K) plan->v_index_strides[0][K]

#define RRAY_INDEX_ATOMIC_RUN(MISSING, X_DIMENSIONALITY, STRIDE)               \
  for (r_ssize j = 0; j < run_size; ++j) {                                     \
    r_ssize location = 0;                                                      \
                                                                               \
    for (int k = 0; k < X_DIMENSIONALITY; ++k) {                               \
      const int index =                                                        \
        plan->v_indices[k][v_index_locations[k] + j * STRIDE(k)];              \
                                                                               \
      if (index == r_globals.na_int) {                                         \
        location = -1;                                                         \
        break;                                                                 \
      }                                                                        \
                                                                               \
      location += (r_ssize) (index - 1) * plan->v_x_strides[k];                \
    }                                                                          \
                                                                               \
    v_out[i + j] = location == -1 ? MISSING : v_x[location];                   \
  }
```

With the literal, `j * STRIDE(k)` collapses to `+ j` and every coordinate array
read becomes a contiguous walk. Combined with step 1 the whole body is straight
line code. This pairing is what produces the Cartesian row at 0.94 ns/elt and
the 2-d row at 2.10 ns/elt.

Store the unit stride test on the plan rather than checking it in the loop:

```c
  plan.unit_run = true;

  for (int i = 0; i < x_dimensionality; ++i) {
    if (plan.v_index_strides[0][i] != 1) {
      plan.unit_run = false;
      break;
    }
  }
```

Two details that are easy to get wrong.

Guard the run count against an empty result. `run_size` is
`plan->v_dimensions[0]`, which can be 0, so compute
`run_size == 0 ? 0 : plan->size / run_size` rather than dividing blindly.

There is no all-zero stride case to specialize, unlike broadcast and reduce.
The point space here is the common dimensions of the coordinate arrays
themselves, so every axis has at least one coordinate array with a nonzero
stride on it. If every coordinate array were dimension 1 on the first axis then
the common dimension would also be 1 and the run would be a single element. A
mix of 0 and 1 does occur and that is exactly the Cartesian case, which takes
the runtime stride path. A dedicated zero stride path could hoist the repeated
load, but the Cartesian row already matches numpy's best number so it was not
worth measuring.

## Step 3: coalesce adjacent axes n ways

Do this third, and treat it as optional. It is the weakest step in this plan.

It buys one thing: a result whose leading dimension is 1 costs the same as the
equivalent flat result, 38% to 46% rather than falling off a cliff. Every other
shape measured is unchanged, including both take-along-axis forms and any mix
where only some coordinate arrays lead with 1.

The case for doing it anyway is that it is about forty lines, it reuses a rule
that already exists, results stay bit identical, and every other iterator in
the package coalesces so index not doing it is a surprise to the next reader.
The case against is that the shape it protects is one a caller has to go out of
their way to produce. Either answer is defensible. Do not let it block steps 1
and 2.

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

Coalescing and the step 2 run loop are one optimization, not two. Coalescing
exists to make the first axis long enough for the run loop to pay for itself.
When the first axis is already long there is nothing left to gain, and when it
is 1 the run loop does nothing at all without coalescing. Ship them together
and do not benchmark either one on square coordinate arrays alone.

One limit is permanent and worth stating in the pull request. numpy runs
`npyiter_find_best_axis_ordering()` before coalescing, sorting axes by stride
so more pairs become adjacent and mergeable. We cannot. numpy allocates the
result through its iterator and is free to pick any point order, while we write
into a contiguous result whose dimensions are fixed by the common coordinate
dimensions. We merge adjacent axes only and lose the non-adjacent cases.

## Step 4: hoist the missing value check

Not measured. Expected to help most on the one coordinate array case, which is
the only place numpy is still ahead after steps 1 through 3.

`rray_index_plan_location()` returns `-1` when any coordinate is missing, so
every element carries a data dependent branch per coordinate array plus a
ternary on the result. numpy has no missing value concept and no equivalent
cost.

`rray_as_index_array()` already walks every coordinate array to validate it, so
it can record whether that array contained any `NA` for free. When no
coordinate array contains one, run a variant of the loop with the check and the
sentinel removed entirely.

This composes with steps 1 and 2 rather than replacing them. With the count
specialized, the stride a literal 1, and the check gone, the one coordinate
array run body reduces to numpy's trivial loop with no iterator state left in
it at all:

```c
for (r_ssize j = 0; j < run_size; ++j) {
  v_out[i + j] = v_x[v_index[j] - 1];
}
```

That is the shape of `mapiter_trivial_get()`, which is where numpy's 0.94
ns/elt comes from. Steps 1 through 3 land the same case at 1.85 ns/elt, so this
is the obvious candidate for the rest of that gap, but nothing here proves it
closes.

## Out of scope

Small call overhead is a separate problem and no step above moves it.
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
identical values, because that pair is exactly what coalescing collapses. Cover
a leading unit axis too, `[1, n]` and `[1, 1, n]`, since those take the branch
that adopts the right hand strides when the left dimension is 1.

The run loop needs both stride paths covered. Identically shaped coordinate
arrays take the unit stride path, and a Cartesian broadcast of `[n, 1]` against
`[1, n]` takes the runtime stride path, so cover both alongside a result with a
size of 0 to exercise the empty run count guard.

Both steps need coverage at the edges of the dispatch. Cover 1, 2, 3, 4, and 5
coordinate arrays so that every specialized case and the generic fallback are
exercised, and keep the existing 64 axis maximum tests.

Zero dimensions need to stay covered for coalescing. A dimension of 0 makes the
size 0 and the loop never runs, but the merge rule still executes, so the
existing zero dimension tests should be checked rather than assumed.
