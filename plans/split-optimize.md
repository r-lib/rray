# Rewrite `rray_split()` as a two stage traversal

Not built. This document records a measured 2.5x to 5x win available in
`rray_split()`, why it exists, the exact change, and the evidence.

Short version: the flat kernel keeps one write stream open per output array
while it walks `x`. When you split on a leading axis there are many of those
streams, they collide in the cache, and the cost swings by 8x depending on
where the allocator happened to place the outputs. Walking the split axes and
the retained axes as two nested loops writes one output at a time, which is
both faster and predictable.

Depends on `feature/mean`. That branch introduces `rray_reduce_nested()`, the
per axes plan helper this reuses, and the zero dimensionality normalization in
`rray_strided_iterator_plan()` that the empty complement relies on.

---

# Part 1: What is slow

`rray_split()` builds a single `rray_strided_iterator2_plan` over the point
space of `x`, reporting two locations as it walks: one into the output list,
one into the current output element.

```c
struct rray_strided_iterator2_plan plan = rray_broadcast_iterator2_plan(
  v_out_dimensions,
  dimensionality,
  v_out_elt_dimensions,
  dimensionality,
  v_x_dimensions,
  dimensionality
);
```

Because the list location is a runtime index, `RRAY_SPLIT_ATOMIC()` has to
carry an array of pointers to every output element and index into it:

```c
r_obj* shelter = KEEP(r_alloc_raw(out_size * sizeof(CTYPE*)));
CTYPE** v_v_out = (CTYPE**) r_raw_begin(shelter);
```

Whether that second indirection costs anything depends on the plan's first
axis stride into the list, `run_stride1`.

When it is zero the list location is loop invariant, the compiler hoists
`v_v_out[out_loc]`, and the run is an ordinary contiguous copy. This is what
you get splitting on a trailing axis.

When it is non-zero every iteration writes a single element into a different
output buffer:

```c
} else if (out_elt_run_stride == 0) {
  for (r_ssize i = run_start; i < run_end; ++i) {
    v_v_out[out_loc][out_elt_loc] = v_x[i];
    out_loc += out_run_stride;
  }
}
```

This is what you get splitting on a leading axis, which is the common case:
`rray_split(x, 1)` on a matrix. Reads stay contiguous, but `run_size` separate
write streams stay open at once, each advancing by one element per outer
iteration.

## Why that is worse than it looks

Splitting a `1000x1000` matrix on axis 1 leaves 1000 open streams. The spacing
between them is whatever the allocator chose for a 1000 element double vector,
which after rounding is 8192 bytes. A power of two spacing maps every stream
onto the same cache sets, so essentially every write is a conflict miss.

Change the shape slightly and the spacing stops being a power of two and the
conflicts disappear. That is the 8x swing in Part 2.

---

# Part 2: The measurement

Three implementations were built and compared: the current flat kernel, the
flat kernel with a `memcpy()` fast path added for contiguous runs, and the two
stage version proposed here.

Timings are round robin. Each iteration runs the implementations in a random
order with a `gc()` before each call, median of 31 to 51 iterations. Block
timing one implementation at a time gave results that moved by 2x between runs,
so it is not trustworthy here. Apple Silicon, R 4.6.

## The decisive table

8 MB double matrix, split on axis 1. Every row does identical work.

| dimensions | current (us) | nested (us) | nested vs current | `out_elt` bytes |
|---|---|---|---|---|
| 10x100000 | 2168.6 | 2252.9 | 0.96 | 800000 |
| 25x40000 | 3266.4 | 2587.0 | 1.26 | 320000 |
| 50x20000 | 5354.2 | 2152.1 | 2.49 | 160000 |
| 100x10000 | 6272.7 | 2328.8 | 2.69 | 80000 |
| 125x8000 | 6666.2 | 2371.8 | 2.81 | 64000 |
| 250x4000 | 783.4 | 1208.9 | 0.65 | 32000 |
| 500x2000 | 5825.4 | 879.8 | 6.62 | 16000 |
| 625x1600 | 1290.6 | 1499.2 | 0.86 | 12800 |
| 1000x1000 | 4852.7 | 920.8 | 5.27 | 8000 |
| 1250x800 | 1393.1 | 1267.0 | 1.10 | 6400 |
| 2000x500 | 4203.3 | 919.6 | 4.57 | 4000 |
| 2500x400 | 1626.0 | 1287.2 | 1.26 | 3200 |
| 5000x200 | 3557.4 | 2242.5 | 1.59 | 1600 |
| 10000x100 | 2711.4 | 2507.8 | 1.08 | 800 |

`current` spans 783 us to 6666 us. `nested` spans 880 us to 2600 us.

The slow rows are exactly the ones where the output element size rounds to a
power of two (4000, 8000, 16000, 80000 bytes). The three rows where `nested`
loses are rows where `current` got a lucky spacing. `nested` never loses to a
bad case, only to a good one.

That reframes the change. This is not "usually fast, rare regressions". The
flat kernel is fast when the shape is lucky, and `1000x1000` is not lucky.

## Shape coverage

Selected rows from a 29 case sweep, reproduced across two seeds. The `memcpy`
column is the control.

| case | current (us) | memcpy (us) | nested (us) | memcpy vs | nested vs |
|---|---|---|---|---|---|
| cube_100^3_axis1 | 5282.5 | 5284.4 | 1388.0 | 1.00 | 3.81 |
| matrix_1000x1000_axis1 | 3962.9 | 3961.4 | 883.6 | 1.00 | 4.48 |
| integer_cube_100^3_axis1 | 6553.8 | 6522.1 | 1779.5 | 1.00 | 3.68 |
| big_cube_200^3_axis1 | 55427.9 | 55163.3 | 20849.6 | 1.00 | 2.66 |
| character_50x50x40_axis1 | 699.0 | 692.3 | 434.3 | 1.01 | 1.61 |
| wide_2x500000_axis1 | 1398.8 | 1640.5 | 912.5 | 0.85 | 1.53 |
| cube_100^3_axis2 | 1177.4 | 1137.5 | 984.2 | 1.04 | 1.20 |
| cube_100^3_axis3 | 652.6 | 456.6 | 456.0 | 1.43 | 1.43 |
| cube_100^3_axes1_2 | 2471.4 | 2465.2 | 2234.5 | 1.00 | 1.11 |
| cube_100^3_axes2_3 | 1572.4 | 1628.6 | 1610.4 | 0.97 | 0.98 |
| character_50x50x40_axis3 | 333.7 | 330.6 | 332.3 | 1.01 | 1.00 |
| cube_100^3_axes1_3 | 1829.3 | 1822.8 | 2304.7 | 1.00 | 0.79 |
| six_10^6_even_axes | 739.0 | 742.4 | 910.9 | 1.00 | 0.81 |

Two things to read off this.

The `memcpy` control is 1.00 nearly everywhere. The win is not about bulk
copies, it is about the write pattern. Worth knowing before anyone spends
effort adding `memcpy()` to the existing kernel.

The only consistent losses are multi axis splits on non adjacent axes, at about
0.8x. See Part 4.

## Large arrays

64 MB double matrix, split on axis 1, so nothing is cache resident.

| outputs | inner size | current (us) | nested (us) | nested vs |
|---|---|---|---|---|
| 50 | 160000 | 48180.4 | 28643.1 | 1.68 |
| 100 | 80000 | 55316.4 | 22227.5 | 2.49 |
| 200 | 40000 | 55476.3 | 21323.9 | 2.60 |
| 500 | 16000 | 48899.0 | 23779.6 | 2.06 |
| 1000 | 8000 | 53891.9 | 27068.1 | 1.99 |
| 2000 | 4000 | 22237.2 | 27425.7 | 0.81 |
| 4000 | 2000 | 46589.2 | 16559.8 | 2.81 |
| 8000 | 1000 | 41259.9 | 18493.5 | 2.23 |
| 16000 | 500 | 41091.3 | 18591.3 | 2.21 |

The win holds at sizes well past any cache. The single 0.81 row is another
lucky spacing for `current`, not a trend.

---

# Part 3: The change

Same shape as `rray_reduce_nested()`. The outer plan walks the split axes,
selecting one output element per point. The inner plan walks the retained axes,
filling that element from front to back. Conceptually this is
`rray_permute_axes()` bringing the split axes to the front, without
materializing the permuted array.

The two plans are complements of each other, so between them they visit every
point of `x` exactly once.

## Share the per axes plan helper

`feature/mean` adds `rray_reduce_axes_plan()` as a static helper in
`src/reduce.c`. It builds a plan over a subset of an array's axes given the
array's dimensions and strides, which is not specific to reducing.

Lift it into `src/strided-iterator.h` as a `static inline` named
`rray_axes_iterator_plan()`, alongside `rray_broadcast_iterator_plan()`. It
needs nothing new in scope, since the caller already passes the strides in. Add
a short doc comment in the style of the two constructors above it. Then both
`reduce.c` and `split.c` call it and the static copy in `reduce.c` goes away.

## `src/split.c`

Drop `rray_broadcast_iterator2_plan()` and the `strided-iterator2` include.
Build strides for `x` with `rray_fill_strides_from_dimensions()`, then two
plans:

```c
const struct rray_strided_iterator_plan outer_plan =
  rray_axes_iterator_plan(v_x_dimensions, v_x_strides, v_axes, axes_size);

const struct rray_strided_iterator_plan inner_plan = rray_axes_iterator_plan(
  v_x_dimensions,
  v_x_strides,
  v_axes_complement,
  axes_complement_size
);
```

`check_max_dimensionality()` has to be called on the dimensionality of `x`
before this, the way `rray_reduce_nested()` does it. The flat version got that
check for free from `rray_broadcast_iterator2_plan()`.

Note the ordering constraint that makes this correct: `arg_as_axes()`
guarantees strictly increasing axes and `rray_axes_complement()` preserves
that, so walking the split axes in order visits output list elements in
storage order, and walking the retained axes in order fills each output element
in storage order. Neither needs an explicit index.

The `v_v_out` shelter array goes away. Each output element is dereferenced once
per output, in the outer loop, not once per element of `x`.

`RRAY_SPLIT_ATOMIC()` and `RRAY_SPLIT_BARRIER()` become one outer walk over
`outer_plan` containing an inner walk over `inner_plan`. The four way branch on
the two run strides collapses, because the write is always sequential. The
atomic version is worth keeping a `memcpy()` branch in for
`inner_run_stride == 1`, which is the trailing axis split. The barrier version
cannot, since `r_chr_poke()` and `r_list_poke()` have to go through the write
barrier.

## What this buys elsewhere

`rray_broadcast_iterator2_plan()` is used in 9 files. Eight of them are binary
operations feeding two *inputs* into one output. `split.c` is the only caller
using it for two *outputs*. After this change iterator2 means one thing, and
`split.c` structurally matches `reduce.c`.

---

# Part 4: What it costs

Multi axis splits on non adjacent axes get about 20% slower. Measured on
`rray_split(x, c(1, 3))` for a `100x100x100` array (0.79x) and on a 6d array
split over axes 2, 4 and 6 (0.81x). Both reproduced across seeds.

The cause is the same trade in reverse. When the split axes are not a
contiguous block, the inner plan cannot coalesce, so each output element reads
`x` in a strided pattern and the array gets re-swept once per output. The flat
kernel reads `x` linearly exactly once.

A dispatch rule, flat when `run_stride1 == 0` and nested otherwise, would buy
these back. It is not worth it. It keeps two kernels alive forever, it does not
fix the `250x4000` style rows where `current` is fast for reasons no user can
predict, and it trades the main benefit of the rewrite, which is that cost
stops depending on allocator luck.

Take the 20% on the rare shapes.

---

# Part 5: Verification

Correctness was already checked against the current implementation across 1568
comparisons with zero mismatches: 9 shapes from 1d to 4d including singleton
and leading unit axes, all 7 storage types, named and unnamed, and every axes
subset including `integer()` and all axes at once.

`tests/testthat/test-split.R` already has the right tool for this. The
`expected_split()` reference implementation inside the "coalesces split axes"
test builds the answer from `[` and `arrayInd()`. Widen that test to sweep
every axes subset across a few more shapes rather than the fixed list of six it
uses now.

The cases that need to stay covered, because they exercise the normalization
and complement edges rather than the fast paths:

- `integer()` axes, where the outer plan has zero dimensionality and must
  normalize to one axis of one element.

- All axes at once, where the inner plan has zero dimensionality and each
  output element holds exactly one value.

- Arrays with a zero dimension, where both plans have size 0.

- Leading and internal unit axes, which are what coalescing folds away.

Re-run `bench/iterator.R`. Its "Splitting" section covers `unnamed_axis1`
through `named_axes1_3`, which is the right spread to confirm the win landed.
