# Specialize 1D and 2D strided iteration

Not built. This document records two implementations, their benchmarks, the
missing-value regression found in the first implementation, and the assembly
that explained it.

The branch containing this document intentionally has no iterator, test, or
benchmark changes. If this work is resumed, the shared-loop implementation in
Part 3 is the best starting point.

Short version: the existing iterator already handles 1D as one flat first-axis
run. A direct 2D second-axis advance can avoid the point carry loop and helps
when the first axis is short. A separate counted double loop produced larger
wins in those cases, but it duplicated the operation body, doubled generated
code size, and made numeric missing-value reductions much slower. Keeping one
shared inner loop removed those regressions while retaining a smaller 2D win.

---

# Part 1: The opportunity

The strided iterator coalesces compatible adjacent axes before walking an
array. This commonly leaves one or two dimensions.

A 1D iterator already takes a simple path through the existing first-axis run
loop. Its first dimension is the full size, so the inner loop visits every
element. The later-axis carry loop has zero iterations and the outer loop then
exits. There is no meaningful coordinate work to remove.

A 2D iterator walks the first axis in the inner loop. After every run, the
general iterator enters the point carry loop at axis 2, increments that point,
checks its dimension, and advances the mapped locations. This work occurs once
per first-axis run rather than once per element, but it matters when the first
axis is short.

The direct 2D operation is:

```c
LOCATION += v_strides[1];
```

For iterator2, both locations advance directly:

```c
LOCATION1 += v_strides1[1];
LOCATION2 += v_strides2[1];
```

The cases most likely to benefit are:

- row broadcasting with dimensions such as `[2, 500000]` and `[1, 500000]`;
- higher dimensional inputs that coalesce to the same 2D shape;
- shared leading singleton dimensions that coalesce to 2D;
- broadcasting one row into a short-first-axis matrix;
- reducing or splitting the first axis of a short-first-axis matrix;
- transposing a matrix;
- other operations with many short first-axis runs.

The benefit falls as the first axis grows because the point carry cost is
amortized over more elements. With first-axis dimensions of 32 or 1000, the
effect was small enough to mix with allocation and benchmark noise.

True 3D and higher iterators still need the point carry loop. Examples that
must remain 3D after coalescing include:

- broadcasting `[2, 1, 4]` to `[2, 3, 4]`, with strides `[1, 0, 2]`;
- reducing `[2, 3, 4]` over its second axis, with output strides `[1, 0, 2]`;
- alternating broadcast patterns in higher dimensional arrays.

---

# Part 2: Separate 1D and 2D loop bodies

The first implementation unpacked the iterator once, then dispatched on its
post-coalescing dimensionality.

Its shape was:

```c
if (dimensionality == 1) {
  while (INDEX != size) {
    __VA_ARGS__
    LOCATION += ROW_STRIDE;
    ++INDEX;
  }
} else {
  const r_ssize rows = v_dimensions[0];
  const r_ssize row_reset = rows * ROW_STRIDE;

  if (dimensionality == 2) {
    const r_ssize runs = v_dimensions[1];

    for (r_ssize run = 0; run < runs; ++run) {
      for (r_ssize row = 0; row < rows; ++row) {
        __VA_ARGS__
        LOCATION += ROW_STRIDE;
        ++INDEX;
      }
      LOCATION -= row_reset;
      LOCATION += v_strides[1];
    }
  } else {
    // Existing first-axis run and point carry loop.
  }
}
```

Iterator2 used the same structure with two locations, two row strides, and two
second-axis advances.

This is easy to understand locally. It gives Clang a counted 2D outer loop and
removes the dimensionality check and point carry from every run. It also
duplicates `__VA_ARGS__` three times inside every fixed zero-stride expansion.
Because the public macros already specialize zero first-axis strides, the
total generated control flow becomes much larger than the source first
suggests.

## Results from the separate loop bodies

`bench/1d-2d.R` used one million elements and 100 iterations per standalone
`bench::mark()` call. The baseline was `main` at `85d3053`.

| Case | Main median | Specialized median | Speedup |
|---|---:|---:|---:|
| 2D rows of 2, right broadcast | 1327 us | 943 us | 1.41x |
| 2D rows of 2, left broadcast | 1331 us | 937 us | 1.42x |
| 2D rows of 4 | 998 us | 881 us | 1.13x |
| 3D input coalesced to 2D | 1348 us | 945 us | 1.43x |
| Shared leading singleton | 1430 us | 957 us | 1.49x |
| Broadcast with rows of 2 | 1410 us | 924 us | 1.53x |
| Reduce first axis of size 2 | 1207 us | 651 us | 1.85x |
| Reduce second axis with rows of 2 | 2083 us | 1871 us | 1.11x |
| Transpose | 858 us | 754 us | 1.14x |
| Split first axis of size 2 | 1326 us | 801 us | 1.66x |

The geometric mean for the complete 2D group was 1.30x. The 1D group was
1.00x and the 3D and higher controls were effectively unchanged in this first
benchmark.

These results initially made the counted double loop look attractive. The
matrixStats benchmark then exposed a serious exception.

---

# Part 3: One shared inner loop

The second implementation retained one copy of the operation body. It added a
direct 2D advance between the location reset and the general carry loop.

For the single-location iterator, the complete change inside the existing
private iteration macro was:

```c
const r_ssize rows = v_dimensions[0];
const r_ssize row_reset = rows * ROW_STRIDE;

while (INDEX != size) {
  for (r_ssize row = 0; row < rows; ++row) {
    __VA_ARGS__
    LOCATION += ROW_STRIDE;
    ++INDEX;
  }
  LOCATION -= row_reset;

  if (dimensionality == 2) {
    LOCATION += v_strides[1];
    continue;
  }

  for (int axis = 1; axis < dimensionality; ++axis) {
    ++v_point[axis];

    if (v_point[axis] < v_dimensions[axis]) {
      LOCATION += v_strides[axis];
      break;
    }

    v_point[axis] = 0;
    LOCATION -= (v_dimensions[axis] - 1) * v_strides[axis];
  }
}
```

For iterator2, the corresponding implementation was:

```c
const r_ssize rows = v_dimensions[0];
const r_ssize row_reset1 = rows * ROW_STRIDE1;
const r_ssize row_reset2 = rows * ROW_STRIDE2;

while (INDEX != size) {
  for (r_ssize row = 0; row < rows; ++row) {
    __VA_ARGS__
    LOCATION1 += ROW_STRIDE1;
    LOCATION2 += ROW_STRIDE2;
    ++INDEX;
  }
  LOCATION1 -= row_reset1;
  LOCATION2 -= row_reset2;

  if (dimensionality == 2) {
    LOCATION1 += v_strides1[1];
    LOCATION2 += v_strides2[1];
    continue;
  }

  for (int axis = 1; axis < dimensionality; ++axis) {
    ++v_point[axis];

    if (v_point[axis] < v_dimensions[axis]) {
      LOCATION1 += v_strides1[axis];
      LOCATION2 += v_strides2[axis];
      break;
    }

    v_point[axis] = 0;

    LOCATION1 -= (v_dimensions[axis] - 1) * v_strides1[axis];
    LOCATION2 -= (v_dimensions[axis] - 1) * v_strides2[axis];
  }
}
```

There are no helper functions. The 2D branch and location updates are inline in
the macro. The existing loop naturally handles 1D as one flat run, so it does
not need a separate copy of `__VA_ARGS__`.

## Results from the shared loop

Two runs of `bench/1d-2d.R` produced these average medians:

| Case | Main median | Shared-loop median | Speedup |
|---|---:|---:|---:|
| 2D rows of 2, right broadcast | 1327 us | 993 us | 1.34x |
| 2D rows of 2, left broadcast | 1331 us | 976 us | 1.36x |
| 2D rows of 4 | 998 us | 886 us | 1.13x |
| 2D rows of 8 | 682 us | 621 us | 1.10x |
| 2D rows of 32 | 597 us | 574 us | 1.04x |
| 2D rows of 1000 | 598 us | 580 us | 1.03x |
| 3D input coalesced to 2D | 1348 us | 1050 us | 1.28x |
| Shared leading singleton | 1430 us | 990 us | 1.44x |
| Broadcast with rows of 2 | 1410 us | 920 us | 1.53x |
| Reduce first axis of size 2 | 1207 us | 793 us | 1.52x |
| Reduce second axis with rows of 2 | 2083 us | 1875 us | 1.11x |
| Transpose | 858 us | 703 us | 1.22x |
| Split first axis of size 2 | 1326 us | 860 us | 1.54x |

The raw geometric mean for the 2D group was 1.27x. The unrelated controls were
about 1.05x faster in the same comparison, which indicates machine-state and
allocation noise. Normalizing by that control movement puts the specific 2D
effect near 1.21x.

The apparent 1D movement should not be attributed to this change. The shared
implementation does not alter the 1D operation body. Focused missing-value 1D
benchmarks also matched main.

The shared loop retained most of the short-run improvement and removed the
numeric missing-value regressions described below.

---

# Part 4: The matrixStats regression

`bench/matrix-stats.R` covers sum, product, all, and any reductions across rows
and columns. It crosses those operations with `na.rm` values of `FALSE` and
`TRUE`, plus no, sparse, and dense missing values. Inputs have one million
elements in a `[1000, 1000]` double matrix. Each case uses 15 iterations.

All 48 cases remain 2D after coalescing:

- column reductions map through output strides `[0, 1]`;
- row reductions map through output strides `[1, 0]`.

The matrixStats suite therefore tests the 2D iterator path rather than 1D or
3D paths.

Relative to matrixStats 1.5.0, the separate-loop implementation still won 37
of 48 cases. It won 22 of 24 row cases and 15 of 24 column cases. Several row
results were much faster than matrixStats:

| Case | Relative result |
|---|---:|
| Row sum, no missing values | 1.44x faster |
| Row sum, `na.rm = TRUE` | 5.02x faster |
| Row product, `na.rm = TRUE` | 34x to 37x faster |
| Row all, `na.rm = TRUE` | 11x to 15x faster |
| Row any, `na.rm = TRUE` | about 12x faster |

Those ratios hide whether the iterator change itself helped. Comparing the
same rray cases directly with main showed a geometric mean of 0.97x, or about
3 percent slower overall. The problem was concentrated in double sum and
product with `na.rm = FALSE` and actual missing values:

| Case | Main | Separate loops | Change |
|---|---:|---:|---:|
| Sparse row sum | 1.92 ms | 3.05 ms | 1.59x slower |
| Sparse row product | 1.94 ms | 3.18 ms | 1.64x slower |
| Sparse column sum | 1.93 ms | 2.34 ms | 1.21x slower |
| Sparse column product | 1.94 ms | 2.37 ms | 1.22x slower |
| Dense column sum | 1.98 ms | 2.43 ms | 1.23x slower |
| Dense column product | 1.98 ms | 2.38 ms | 1.21x slower |

A focused path benchmark forced the same missing-value sum and product bodies
through 1D, 2D column, 2D row, and genuine 3D middle-axis walks. Its geometric
mean time ratios, separate loops divided by main, were:

| Path | Time ratio |
|---|---:|
| 1D | 1.00x |
| 2D columns | 1.13x |
| 2D rows | 1.34x |
| 3D middle axis | 1.03x |

The primary regression was 2D, with a smaller spillover into the larger 3D
compiled body.

After changing to the shared loop, the full matrixStats suite had a 1.002x
geometric speedup against main. Columns were 1.001x and rows were 1.003x. Those
are all equivalent to main at benchmark precision. The focused missing-value
path cases also returned to main performance.

---

# Part 5: Why the counted double loop regressed

The ARM64 assembly showed that the counted double loop itself was not slower
for ordinary values. Both implementations compiled the double sum inner body
to the same scalar shape:

```asm
ldr    d0, [output]
ldr    d1, [input]
fcmp   d0, d1
b.vs   missing_path
fadd   d0, d0, d1
str    d0, [output]
```

Neither missing-aware reduction loop was vectorized. The difference appeared
after `b.vs`, when a `NaN` or `NA` requires calls to `R_IsNA()`.

The separate counted loop kept its outer run counter, run bound, row counter,
dimensions, reset, strides, index, and locations live across those calls.
Clang placed much of that state in caller-saved registers. The missing path
therefore stored seven live values before the calls:

```asm
stp    x9, x14, [sp, ...]
str    x8, [sp, ...]
stp    x10, x11, [sp, ...]
str    d1, [sp, ...]
str    x0, [sp, ...]
bl     R_IsNA
```

It then reloaded the state afterward.

The shared loop gave Clang shorter live ranges and let it hold the persistent
state in more callee-saved registers. Its missing path stored only three live
values:

```asm
stp    x12, x0, [sp, ...]
str    d1, [sp, ...]
bl     R_IsNA
```

The shared function saves four more registers in its prologue and restores
them in its epilogue. That cost is paid once per function call. The separate
loop instead pays its larger spill and reload sequence every time the missing
path runs.

This matters because an accumulator remains `NA` after it first sees an `NA`.
Every later element mapped to that accumulator enters the missing path again.
One input `NA` can therefore cause hundreds of later calls to `R_IsNA()`, each
with the additional stack traffic.

The repeated operation bodies also nearly doubled the generated functions:

| Function | Main | Separate loops | Shared loop |
|---|---:|---:|---:|
| Double sum, `na.rm = FALSE` | 628 bytes | 1284 bytes | 664 bytes |
| Double product, `na.rm = FALSE` | 636 bytes | 1292 bytes | 672 bytes |

The function-size increase is secondary to the observed spill sequence, but
it can also affect instruction-cache use and Clang's register allocation.

The result is a real compiler tradeoff:

- ordinary short runs benefit from a dedicated counted 2D loop;
- missing-value propagation is hurt by the extra live state around calls;
- one shared inner body gives up some of the largest counted-loop gain but
  avoids the pathological slow path.

---

# Part 6: Benchmark file design

The experimental `bench/1d-2d.R` followed the standalone style of
`bench/stride-zero.R`. It used many independent `bench::mark()` calls, each
preceded by a comment of no more than 72 characters. It accepted:

```text
RRAY_BENCH_ITERATIONS
RRAY_BENCH_OUTPUT
```

It saved the individual benchmark objects, a numeric summary, the iteration
count, and the Git revision to an RDS file.

The 22 cases were:

- two direct 1D arrays;
- identical 3D arrays that coalesce completely to 1D;
- scalar arithmetic and broadcasting that coalesce to 1D;
- reduction over every axis after 1D coalescing;
- 2D broadcast runs of size 2, 4, 8, 32, and 1000;
- left and right broadcast operands for runs of size 2;
- a 3D input that coalesces to 2D;
- shared leading singleton axes that coalesce to 2D;
- 2D broadcasting with first-axis size 2;
- reductions over each axis of a `[2, 500000]` matrix;
- a 2D transpose;
- splitting the first axis of a `[2, 500000]` matrix;
- a genuine 3D middle-axis binary broadcast control;
- a genuine 3D middle-axis broadcast control;
- a genuine 3D middle-axis reduction control;
- an alternating six-dimensional broadcast control.

Any future benchmark should restore this file or an equivalent set of
standalone calls. The missing-value path benchmark should also be retained as
a required regression check, even if it lives in a separate experimental
script.

---

# Part 7: Correctness and validation

The implementation was applied to both `RRAY_STRIDED_ITERATOR_FOR_EACH()` and
`RRAY_STRIDED_ITERATOR2_FOR_EACH()`.

Tests exercised:

- an identical shape that coalesces to 1D;
- 2D row broadcasting in both operand orders;
- a true 3D broadcast in both operand orders;
- scalar broadcasting through the single-location iterator;
- 2D single-location broadcasting;
- true 3D single-location broadcasting.

The complete test suite passed:

```text
FAIL 0 | WARN 0 | SKIP 0 | PASS 1520
```

The package check completed with zero errors. It retained the existing
bitwise-boolean compiler warning and the unrelated notes for `AGENTS.md` and
Xcode's temporary `xcrun_db` directory.

All C sources and headers were formatted with `clang-format`. All R files were
formatted with Air. The implementation touched no `r_obj*` values and created
no new protection paths.

---

# Part 8: Documentation if resumed

The iterator header should gain a section titled:

```c
// Optimization - 1D and 2D specialization
```

It should explain these points:

- coalescing frequently leaves one or two dimensions;
- 1D naturally walks one flat first-axis run;
- 2D can advance mapped locations directly by second-axis strides;
- only 3D and higher walks need coordinate carrying;
- identical `[2, 3, 4]` inputs coalesce to a 1D run of size 24;
- `[1, 4]` plus `[2, 4]` remains 2D with four runs of size 2;
- `[1, 3, 4]` plus `[2, 3, 4]` coalesces to `[2, 12]`.

Existing first-axis run examples that coalesce to 1D or 2D should move to this
new section. The first-axis run section should use examples that remain 3D,
such as middle-axis broadcasting and reduction with strides `[1, 0, 2]`.

---

# Part 9: Recommendation

Do not implement this optimization now.

The 1D case already has the desired traversal shape. The shared 2D branch is
small and safe, but its gains are concentrated in unusually short first-axis
runs. The separate counted loop has larger wins in those cases, but its code
growth and missing-value spill behavior make it unsuitable as written.

If this work is reconsidered:

1. Start from the shared-loop implementation in Part 3.
2. Keep one copy of `__VA_ARGS__` in each fixed stride expansion.
3. Benchmark directly against the current main branch in interleaved runs.
4. Include numeric missing-value reductions with `na.rm = FALSE`.
5. Inspect generated function size and the `R_IsNA()` slow path.
6. Require stable wins beyond short first-axis dimensions before merging.

The saved result is useful even without code: a direct 2D advance is valid and
can help, but the obvious dedicated double loop changes register pressure in a
way that matters for real reduction bodies.
