# Macro-free strided iteration

## Status

The macro-free strided iteration design is implemented on this branch.
`RRAY_STRIDED_ITERATOR_FOR_EACH()` and
`RRAY_STRIDED_ITERATOR2_FOR_EACH()` have been removed, and every caller now
uses explicit run-based loops.

The implementation has two parts:

- A plan holds the dimensions and strides that stay fixed for one traversal.
- An iterator holds only the state that changes while walking the plan.

The iterator does not store a pointer to its plan. The few operations that need
plan data receive the plan as an argument. This keeps the type honest and had
no measurable performance cost.

All 1,512 package tests pass.

Type-generation macros still exist in arithmetic and similar code. Those are
separate from strided iteration. They generate specialized typed workers and
avoid runtime type dispatch in hot loops.

## Result

The main conclusion is that the plan and iterator should remain separate.

This is not mainly about `const`, whether construction happens in place, or
whether the full struct fits in cache. The important property is that the
changing traversal state is a small, local object that Clang can break into
independent values and keep in registers.

Combining the fixed plan data and changing iterator data into one struct made
carry-heavy broadcasts 15% to 19% slower. Moving construction into the typed
worker did not help. Reducing the maximum dimensionality from 64 to 8 made the
struct much smaller but did not help either.

The current two-stage design keeps the large arrays in the plan and the
changing run, locations, and point in the iterator.

## Requirements

A macro-free replacement still has to preserve the optimizations that were
inside the old macros:

- Coalesce compatible axes before traversal.
- Process the coalesced first axis as one run.
- Perform point carry work between runs, not for every element.
- Keep zero first-axis strides visible as separate fixed paths.
- Keep operation, type, and missing-value dispatch outside inner loops.
- Support one and two mapped location spaces.
- Keep mutable traversal state local to the typed worker.
- Stay within normal benchmark noise for the macro implementation.

The public shape of the loop matters as much as the iterator functions. A
generic iterator API can still be slow if it hides these facts from the
compiler.

## Current design

The one-location plan contains fixed traversal data:

```c
struct rray_strided_iterator_plan {
  r_ssize size;
  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  int dimensionality;
  r_ssize v_strides[RRAY_MAX_DIMENSIONALITY];
};
```

The one-location iterator contains only changing state:

```c
struct rray_strided_iterator {
  r_ssize run_start;
  r_ssize location;
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
};
```

The two-location forms follow the same split:

```c
struct rray_strided_iterator2_plan {
  r_ssize size;
  r_ssize v_dimensions[RRAY_MAX_DIMENSIONALITY];
  int dimensionality;
  r_ssize v_strides1[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_strides2[RRAY_MAX_DIMENSIONALITY];
};

struct rray_strided_iterator2 {
  r_ssize run_start;
  r_ssize location1;
  r_ssize location2;
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
};
```

Plan constructors validate dimensions, compute strides, and coalesce axes.
Iterator constructors only initialize changing state:

```c
struct rray_strided_iterator_plan rray_strided_iterator_plan(...);
struct rray_strided_iterator_plan rray_broadcast_iterator_plan(...);
struct rray_strided_iterator rray_strided_iterator(void);

struct rray_strided_iterator2_plan rray_strided_iterator2_plan(...);
struct rray_strided_iterator2_plan rray_broadcast_iterator2_plan(...);
struct rray_strided_iterator2 rray_strided_iterator2(void);
```

Run size and first-axis strides come from the plan. Run start and locations
come from the iterator. Only `finished()` and `next()` need both:

```c
bool rray_strided_iterator_finished(
  const struct rray_strided_iterator* it,
  const struct rray_strided_iterator_plan* plan
);

void rray_strided_iterator_next(
  struct rray_strided_iterator* it,
  const struct rray_strided_iterator_plan* plan
);
```

The `iterator2` functions have the same shape.

Passing the plan directly to these inline functions is free in the generated
hot loops. The worker already has the plan available, usually in a register.
Removing the plan field made the one-location iterator 528 bytes instead of
536 and the two-location iterator 536 bytes instead of 544. More importantly,
the iterator now represents only mutable state.

## Run-based traversal

The iterator advances one first-axis run at a time, not one element at a time.

After coalescing, `plan->v_dimensions[0]` is the run size. The caller walks
that whole run with a small inner loop. `next()` then increments the later
point coordinates and updates the mapped locations for the next run.

A one-location caller has this shape:

```c
const r_ssize run_size =
  rray_strided_iterator_plan_run_size(plan);
const r_ssize run_stride =
  rray_strided_iterator_plan_run_stride(plan);

for (struct rray_strided_iterator it = rray_strided_iterator();
     !rray_strided_iterator_finished(&it, plan);
     rray_strided_iterator_next(&it, plan)) {
  const r_ssize run_start = rray_strided_iterator_run_start(&it);
  const r_ssize run_end = run_start + run_size;
  r_ssize location = rray_strided_iterator_location(&it);

  if (run_stride == 0) {
    const double elt = v_x[location];

    for (r_ssize i = run_start; i < run_end; ++i) {
      v_out[i] = elt;
    }
  } else {
    for (r_ssize i = run_start; i < run_end; ++i) {
      v_out[i] = v_x[location];
      location += run_stride;
    }
  }
}
```

The iterator location always means the beginning of the current run. The
caller increments a local copy while processing elements. `next()` must not
undo those local increments because the iterator location never made them.

There is no separate terminal adjustment. On the last call, `next()`
increments `run_start` to `plan->size` and completes the normal point carry.
The next `finished()` check ends the loop.

An element-at-a-time `next()` would test for a run boundary and possibly carry
later axes for every element. Earlier work found that batching first-axis runs
can improve these operations by 3 to 4 times. The run boundary must stay
outside the element loop.

## `rray_mean_along()` plan

The macro-free API also makes a better `rray_mean_along()` traversal possible.
The current reduction traversal walks `x` in physical order and maps each
element to an output location. That works well for sums and products, but a
mean needs more numerical state and may need several passes over each reduced
slice.

R accumulates real means in `LDOUBLE`. It can use a scaled sum if the first sum
overflows, then makes a correction pass to reduce rounding error. Removing
missing values also requires a count for each slice. Complex means need
separate real and imaginary accumulators.

`RRAY_REDUCE()` uses its R output vector as the accumulator, so it cannot hold
this wider state directly. One implementation could allocate sum and count
buffers indexed by output location. A better plan is to visit one complete
reduced slice for each output location. This needs two immutable strided plans:

| Plan | Axes | Reported location |
|---|---|---|
| Outer | Retained axes in their original order | Base location in `x` |
| Inner | Reduced axes in their original order | Offset from that base |

Both plans use the physical strides of `x`. They change the order in which the
array is visited without copying or physically permuting it. The outer run
start is the flat output location because reduced axes have dimension one in
the output.

The outer iterator still advances by runs. Each output location in the current
outer run therefore starts its own inner iterator:

```c
const r_ssize out_run_size =
  rray_strided_iterator_plan_run_size(outer_plan);
const r_ssize x_run_stride =
  rray_strided_iterator_plan_run_stride(outer_plan);

for (struct rray_strided_iterator outer = rray_strided_iterator();
     !rray_strided_iterator_finished(&outer, outer_plan);
     rray_strided_iterator_next(&outer, outer_plan)) {
  r_ssize out_location = rray_strided_iterator_run_start(&outer);
  const r_ssize out_end = out_location + out_run_size;
  r_ssize x_base = rray_strided_iterator_location(&outer);

  for (; out_location < out_end;
       ++out_location, x_base += x_run_stride) {
    struct rray_strided_iterator inner = rray_strided_iterator();
  }
}
```

The example stops where each fresh inner traversal begins. The mean worker
then walks `inner_plan` with the same run-based loop shape.

For each output location, the real implementation can use scalar `long double`
values for the sum and correction, plus one `r_ssize` count when removing
missing values. It creates a fresh inner iterator for each numerical pass over
the same inner plan. The usual path needs a sum pass and a correction pass. A
non-finite first sum can add a scaled sum pass before correction.

This avoids per-output `long double` buffers and is a useful reason to keep
plans immutable and restartable. An empty retained axis set represents one
output location. An empty reduced axis set represents one input value per
output location. A zero-size reduced slice produces `NaN`.

The grouped traversal can read a middle or later axis with a stride. It should
be benchmarked against the current input-major reduction before it is used for
other reducers.

## Coalescing

Coalescing merges adjacent compatible axes before traversal. For example,
dimensions `[2, 4, 5]` with strides `[1, 2, 8]` become one run of size 40.

This reduces point carry work and often turns a multidimensional traversal into
one contiguous loop. Earlier benchmarks found gains of 3 to 5 times in shapes
where several axes can be merged.

For `iterator2`, both stride mappings must allow the same merge. Coalescing
changes the shared point space, so one location mapping cannot merge an axis
unless the other can merge it too.

Dimensions of size one need special handling. They can be absorbed while
adopting the useful neighboring stride. This is important for shared singleton
axes and higher-dimensional broadcasting.

## Zero first-axis strides

A zero first-axis stride means a mapped location stays fixed for the whole
inner run. Broadcasting uses this to reuse one input value. Reduction uses it
to write repeatedly to one output location.

The caller must branch on this before entering the inner loop. When Clang sees
a fixed path with no location increment, it can hoist the repeated load and
vectorize the other work.

For two locations, callers retain four paths:

- Both strides are zero.
- Only the first stride is zero.
- Only the second stride is zero.
- Both strides advance.

Putting an unknown stride check inside the element loop loses useful facts.
Trying to hide the four paths behind one general loop is shorter source but
worse generated code.

Important shapes include scalar broadcasts, row broadcasts, reductions over
the first axis, alternating singleton axes, and both `rray_split()` location
layouts.

## Dispatch must stay outside inner loops

Operation choices that stay fixed for the whole call must be resolved before
traversal.

The first macro-free equality port selected equal versus not-equal inside every
element loop. It was 34% to 35% slower. The first extrema port selected the
missing-value policy inside every element loop. Its median regression was 36%,
and some cases were 2.8 to 9.1 times slower.

The corrected implementation selects the operation first and then enters a
specialized loop. Equality returned to about 0.92 times the macro baseline in
the broad benchmark, where values below 1 are faster.

The same rule applies to:

- Arithmetic operation selection.
- Comparison selection.
- Missing-value policy.
- Input and output types.
- Output representation.
- Zero-stride paths.

A callback or function pointer for each element would create the same problem.
The element operation must remain visible inside the typed worker.

## Plan and iterator experiments

Several designs were tested against the split plan and iterator implementation.
The focused cases used arrays with one million elements. Alternating cases used
six axes and forced frequent point carries.

### One combined struct passed through dispatch

The first combined design stored fixed plan data and mutable locations in one
struct. The caller constructed it and passed a mutable pointer through the
typed-worker function pointer.

Compared with the split design:

| Case | Combined / split |
|---|---:|
| Arithmetic, alternating broadcast | 1.178x |
| Arithmetic, row broadcast | 1.038x |
| Arithmetic, scalar broadcast | 1.054x |
| Direct broadcast, alternating axes | 1.151x |
| Direct broadcast, row | 0.970x |
| Equality, row | 0.979x |

The carry-heavy cases were the clear regressions.

In `rray_add_dbl_dbl()`, Clang held the two locations in registers while
processing a run, then stored them back into the caller-owned struct after
every point carry. The worker stack frame was only 112 bytes, but the repeated
stores remained in the hot outer loop.

### Combined struct built inside the typed worker

The next design passed raw dimension metadata through dispatch and constructed
the combined struct directly inside each typed worker.

This removed caller ownership but did not remove the stores:

| Case | Worker-local combined / split |
|---|---:|
| Arithmetic, alternating broadcast | 1.193x |
| Arithmetic, row broadcast | 1.051x |
| Arithmetic, scalar broadcast | 1.056x |
| Direct broadcast, alternating axes | 1.186x |
| Direct broadcast, row | 1.013x |
| Equality, row | 1.007x |

The worker stack frame grew to 2,096 bytes. Clang still wrote both mutable
locations into the local stack object after every carry.

Forcing the large initializer to inline increased the frame to 3,168 bytes and
still did not remove the stores.

This showed that caller ownership was not the full explanation. A local
address-taken combined object can have the same problem.

### Copying the combined struct into a worker-local value

A temporary experiment copied a caller-built combined struct into a local
combined struct before traversal.

This removed the location stores from the carry loop and brought alternating
arithmetic back to 0.986 times the split design. That supports the idea that
register promotion of mutable locations is important.

It was not a good general solution. The worker copied 2,088 bytes and used a
2,096-byte stack frame. Scalar arithmetic was 1.065 times the split design,
row arithmetic was 1.030 times, and alternating direct broadcast was 1.088
times. It traded carry-loop stores for setup and stack costs.

### Maximum dimensionality of eight

Reducing `RRAY_MAX_DIMENSIONALITY` from 64 to 8 shrank the worker-local
combined frame from 2,096 bytes to 416 bytes.

It did not fix the carry-heavy regression. With both designs built for eight
axes, the combined version was still:

- 1.185 times the split design for alternating arithmetic.
- 1.157 times the split design for alternating direct broadcast.

Clang still stored both locations after every carry. This showed that total
struct size was not the main problem. Both frames fit comfortably in cache,
but the memory dependency remained.

The eight-axis limit also broke the existing dimensionality contract and its
tests, so it was restored to 64.

### Removing the plan pointer from the iterator

The final cleanup removed the plan pointer from both mutable iterator structs.
`finished()` and `next()` now take the plan directly.

The arithmetic, equality, and broadcast hot loops were unchanged. Their text
section sizes were also unchanged. Focused benchmarks stayed within normal run
noise, including the alternating carry-heavy cases.

This is the current design. It improves the meaning of the types without a
performance cost.

## What we think Clang is doing

The best explanation is register promotion, sometimes called scalar
replacement. Clang tries to replace fields of a local struct with independent
compiler values. Those values can then live in registers.

The old macros explicitly unpacked iterator fields into local values. In the
old `rray_add_dbl_dbl()`, the worker used a 112-byte stack frame and kept the
changing locations in registers.

The split function design gives Clang a similar opportunity:

| Design | Worker stack | Location handling |
|---|---:|---|
| Old iteration macro | 112 bytes | Registers |
| Split plan and iterator | 608 bytes | Registers |
| Combined pointer from caller | 112 bytes | Stored through caller pointer |
| Combined worker-local struct | 2,096 bytes | Stored to local stack |
| Combined worker-local, eight axes | 416 bytes | Stored to local stack |

The split worker has a larger frame than the old macro because the mutable
point array is a real local object. That did not cause a meaningful regression.
The important locations and run counters still remain in registers.

A `const` plan helps express that plan data does not change, but `const`
does not promise registers. Constructing an object inside the worker also does
not promise registers. A small stack frame does not promise registers either.

The common failure in the combined designs was that fixed arrays and changing
fields remained part of one address-taken aggregate. Clang kept the locations
coherent with that memory object across carries. Separating the large fixed
plan from the mutable iterator gave the optimizer a simpler mutable object.

This is an explanation based on the generated assembly and benchmark behavior,
not a C language guarantee. Compiler versions can make different choices, so
performance-sensitive changes still need assembly checks.

## Broad benchmark record

The corrected macro-free implementation was compared with the macro baseline
in two broad suites. Ratios are candidate divided by baseline, so values below
1 are faster.

The stride-zero suite covered arithmetic, comparison, equality, extrema,
broadcast, numeric and logical reductions, and both `rray_split()` layouts:

| Summary across 39 cases | Candidate / baseline |
|---|---:|
| Minimum | 0.57x |
| First quartile | 0.88x |
| Median | 0.92x |
| Third quartile | 0.98x |
| Maximum | 1.05x |

The slowest repeatable cases were about 4% to 5% slower. Numeric and logical
reductions were within 0.5% of baseline in that run.

The exhaustive extrema suite covered 96 combinations of operation, type,
missing-value policy, missing-value layout, and traversal layout:

| Summary across 96 cases | Candidate / baseline |
|---|---:|
| Minimum | 0.41x |
| First quartile | 0.96x |
| Median | 1.01x |
| Third quartile | 1.06x |
| Maximum | 3.26x |

The maximum came from a short noisy case. The median and upper quartile were
the useful signals. Suspicious short cases need more iterations, warm-up, and
paired run-order changes before being treated as regressions.

## Benchmark method

Focused combined-struct comparisons used two 200-iteration runs and the
geometric mean of the medians. The plan-field cleanup used two 300-iteration
mixed runs plus isolated 1,000-iteration broadcast runs. Reversing process order
reversed the small broadcast difference, while the hot-loop assembly stayed
the same. We therefore treated that result as noise.

R allocation and garbage collection make minima especially noisy. Use medians,
warm each case, alternate candidate and baseline process order, and rerun any
difference that changes direction.

Useful benchmark commands are:

```sh
RRAY_BENCH_ITERATIONS=50 Rscript bench/stride-zero.R
RRAY_BENCH_ITERATIONS=50 Rscript bench/extremum.R
RRAY_BENCH_ITERATIONS=50 Rscript bench/reduce-logical.R
RRAY_BENCH_ITERATIONS=50 Rscript bench/matrix-stats.R
Rscript bench/iterator.R
Rscript bench/broadcast.R
```

Pay particular attention to:

- Alternating singleton axes with frequent carries.
- Scalar and row broadcasting.
- Short first-axis runs.
- Contiguous controls.
- Reductions with fixed output locations.
- Both `iterator2` split directions.
- Tiny arrays where setup dominates.
- Large arrays where vectorization dominates.

For a regression, inspect the typed worker assembly. Check for location stores
between runs, element-level carry work, runtime operation branches, and unknown
zero strides.

## Rules for future changes

Keep these properties unless new measurements prove a better design:

1. Keep plan data and mutable iterator state in separate structs.
2. Keep the plan out of the iterator and pass it to the inline functions that
   need it.
3. Construct the iterator inside the typed worker.
4. Walk first-axis runs, not individual elements.
5. Keep point carries outside the element loop.
6. Read run size and first-axis strides from the plan before traversal.
7. Keep explicit zero-stride paths.
8. Keep operation, type, and missing-value dispatch outside inner loops.
9. Keep element work visible to the compiler instead of using callbacks.
10. Treat iterator locations as the beginning of the current run.
11. Do not update the plan during traversal.
12. Compare both benchmarks and generated assembly after changing this code.

The current two-stage design is not only an organizational choice. It preserves
the source shape that produced the best and most stable generated code in these
experiments.
