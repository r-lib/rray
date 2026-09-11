# Specialize stride 0 in the iterator inner loop

Not built. This document records a measured 1.8x win available in
`RRAY_ITERATOR2_FOR_EACH()`, why it exists, the exact change, and the evidence.

Short version: when either input is broadcast along the innermost axis, the
inner loop silently falls off clang's vectorized path onto a scalar fallback.
Passing a literal `0` for that stride puts it back on the vector path. It costs
code size.

---

# Part 1: What is slow

`RRAY_ITERATOR2_FOR_EACH()` reads both row strides out of the iterator:

```c
const r_ssize rows = v_point_dimensions[0];
const r_ssize row_stride1 = v_location1_strides[0];
const r_ssize row_stride2 = v_location2_strides[0];
```

Both are runtime values. `rray__location_strides_init()` sets a stride to `0`
on any axis where that input has a dimension of 1, which is how broadcasting is
expressed. So `row_stride2 == 0` means "y is broadcast along the innermost
axis".

Timings for `rray_compare_dbl_dbl()` at 1e6 elements, min of 200 reps:

| inner stride pair | min (ms) |
|---|---|
| `(1, 1)` | 0.229 |
| `(1, 0)` | 0.410 |

That is the same compiled function, called twice, with different data in the
iterator. 1.8x apart.

## Which shapes hit it

Not simply "the first axis has a dimension of 1". Coalescing absorbs leading
axes whose broadcast dimension is 1, so the test is on the coalesced axis 0.

The rule: skip leading axes whose broadcast dimension is 1. At the first axis
where the broadcast dimension is greater than 1, if either input has a
dimension of 1 there, that input gets stride 0 and you pay the 1.8x.

For matrices that reduces to: broadcasting a row vector `[1, n]` is slow,
broadcasting a column vector `[m, 1]` is fast. Broadcasting along the
contiguous direction is what costs you, which is a statement about column-major
layout.

Two shapes show why the naive test is wrong:

- `x[1,1000,1000]` against `y[1,1000,1000]` has a leading 1 on both sides, but
  neither is broadcast against the other, so the axis is absorbed and it
  coalesces to one contiguous run. Fast.

- `x[100,100,100]` against `y[100,1,100]` has y broadcast, just not on the
  first axis. Fast.

---

# Part 2: Why it is slow

It is not the cost of `LOCATION2 += row_stride2`. That add executes either way,
in identical machine code.

Clang versions the loop. It emits one vectorized body plus a scalar fallback,
and chooses between them at runtime with a guard emitted just before the loop.
Disassembling `compare_current` from the harness in Part 6:

```asm
38:  cmp   x10, #0x1
3c:  ccmp  x11, #0x1, #0x0, eq
```

That is `row_stride1 == 1 && row_stride2 == 1`. Both strides must be exactly 1.
With `row_stride2 == 0` the guard fails and the whole loop runs scalar.

The reason clang guards rather than handling stride 0 is that with an unknown
runtime stride, `v_y[y_loc]` has an unknown access pattern. Vectorizing it
would need a gather, which is not profitable on NEON, so it guards on the one
case it can vectorize and gives up otherwise.

Passing a literal `0` changes what the compiler can prove about the load, not
about the add. `y_loc` becomes provably constant across the run, so
`v_y[y_loc]` hoists out of the loop body entirely. What remains touches only
`v_x` and `v_out` at stride 1, which vectorizes without needing the guard that
was failing.

Vector compare instructions (`fcmge.2d`, `fcmgt.2d`) in the two functions:

- current: 4, so one vectorized body

- fixed: 12, so three, one per arm

---

# Part 3: The change

Split the walk out of the entry macro into a helper parameterized by the two
row strides, then branch on which one is zero and pass a literal `0`. No call
site changes.

Correctness argument: when `row_stride == 0`, `row_reset == rows * 0 == 0`, so
`LOCATION += row_stride` and `LOCATION -= row_reset` are already no-ops. The
specialization deletes statements that do nothing. This was also verified
element by element, see Part 4.

Replace `RRAY_ITERATOR2_FOR_EACH()` in `src/iterator.h` with:

```c
#define RRAY__ITERATOR2_WALK(                                                  \
  INDEX,                                                                       \
  LOCATION1,                                                                   \
  LOCATION2,                                                                   \
  ROW_STRIDE1,                                                                 \
  ROW_STRIDE2,                                                                 \
  ...                                                                          \
)                                                                              \
  do {                                                                         \
    const r_ssize row_reset1 = rows * (ROW_STRIDE1);                           \
    const r_ssize row_reset2 = rows * (ROW_STRIDE2);                           \
                                                                               \
    while (INDEX != size) {                                                    \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        LOCATION1 += (ROW_STRIDE1);                                            \
        LOCATION2 += (ROW_STRIDE2);                                            \
        ++INDEX;                                                               \
      }                                                                        \
      LOCATION1 -= row_reset1;                                                 \
      LOCATION2 -= row_reset2;                                                 \
                                                                               \
      for (int axis = 1; axis < point_dimensionality; ++axis) {                \
        ++v_point[axis];                                                       \
                                                                               \
        if (v_point[axis] < v_point_dimensions[axis]) {                        \
          LOCATION1 += v_location1_strides[axis];                              \
          LOCATION2 += v_location2_strides[axis];                              \
          break;                                                               \
        }                                                                      \
                                                                               \
        v_point[axis] = 0;                                                     \
                                                                               \
        LOCATION1 -=                                                           \
          (v_point_dimensions[axis] - 1) * v_location1_strides[axis];          \
        LOCATION2 -=                                                           \
          (v_point_dimensions[axis] - 1) * v_location2_strides[axis];          \
      }                                                                        \
    }                                                                          \
  } while (0)

#define RRAY_ITERATOR2_FOR_EACH(IT, INDEX, LOCATION1, LOCATION2, ...)          \
  do {                                                                         \
    struct rray_iterator2* const iterator = (IT);                              \
    r_ssize INDEX = iterator->index;                                           \
    const r_ssize size = iterator->size;                                       \
    r_ssize* v_point = iterator->v_point;                                      \
    const r_ssize* v_point_dimensions = iterator->v_point_dimensions;          \
    const int point_dimensionality = iterator->point_dimensionality;           \
    r_ssize LOCATION1 = iterator->location1;                                   \
    const r_ssize* v_location1_strides = iterator->v_location1_strides;        \
    r_ssize LOCATION2 = iterator->location2;                                   \
    const r_ssize* v_location2_strides = iterator->v_location2_strides;        \
                                                                               \
    const r_ssize rows = v_point_dimensions[0];                                \
    const r_ssize row_stride1 = v_location1_strides[0];                        \
    const r_ssize row_stride2 = v_location2_strides[0];                        \
                                                                               \
    if (row_stride2 == 0) {                                                    \
      RRAY__ITERATOR2_WALK(                                                    \
        INDEX,                                                                 \
        LOCATION1,                                                             \
        LOCATION2,                                                             \
        row_stride1,                                                           \
        0,                                                                     \
        __VA_ARGS__                                                            \
      );                                                                       \
    } else if (row_stride1 == 0) {                                             \
      RRAY__ITERATOR2_WALK(                                                    \
        INDEX,                                                                 \
        LOCATION1,                                                             \
        LOCATION2,                                                             \
        0,                                                                     \
        row_stride2,                                                           \
        __VA_ARGS__                                                            \
      );                                                                       \
    } else {                                                                   \
      RRAY__ITERATOR2_WALK(                                                    \
        INDEX,                                                                 \
        LOCATION1,                                                             \
        LOCATION2,                                                             \
        row_stride1,                                                           \
        row_stride2,                                                           \
        __VA_ARGS__                                                            \
      );                                                                       \
    }                                                                          \
  } while (0)
```

Both strides being 0 falls into the first arm, with `row_stride1` passed as a
variable that happens to hold 0. That case only arises when the coalesced axis
0 has a point dimension of 1, so `rows` is 1 and it does not matter.

## The same change for `RRAY_ITERATOR_FOR_EACH()`

The single location iterator has the same problem, with two arms instead of
three. It is used by `rray_broadcast()` and the reductions. This half is
written out in the same shape but has not been benchmarked or tested, so treat
it as a second step, not part of the first change.

```c
#define RRAY__ITERATOR_WALK(INDEX, LOCATION, ROW_STRIDE, ...)                  \
  do {                                                                         \
    const r_ssize row_reset = rows * (ROW_STRIDE);                             \
                                                                               \
    while (INDEX != size) {                                                    \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        LOCATION += (ROW_STRIDE);                                              \
        ++INDEX;                                                               \
      }                                                                        \
      LOCATION -= row_reset;                                                   \
                                                                               \
      for (int axis = 1; axis < point_dimensionality; ++axis) {                \
        ++v_point[axis];                                                       \
                                                                               \
        if (v_point[axis] < v_point_dimensions[axis]) {                        \
          LOCATION += v_location_strides[axis];                                \
          break;                                                               \
        }                                                                      \
                                                                               \
        v_point[axis] = 0;                                                     \
                                                                               \
        LOCATION -= (v_point_dimensions[axis] - 1) * v_location_strides[axis]; \
      }                                                                        \
    }                                                                          \
  } while (0)
```

The entry macro keeps its whole preamble and ends with:

```c
    if (row_stride == 0) {                                                     \
      RRAY__ITERATOR_WALK(INDEX, LOCATION, 0, __VA_ARGS__);                    \
    } else {                                                                   \
      RRAY__ITERATOR_WALK(INDEX, LOCATION, row_stride, __VA_ARGS__);           \
    }                                                                          \
```

Keep the existing comments on both entry macros. They still describe what the
walk does.

---

# Part 4: Results

15 shapes, 1e6 output elements, `clang -O2`, min of 200 reps, using the
`rray_compare_dbl_dbl()` body with `NaN` seeded into both inputs. `exact` means
the output was compared element by element against the current macro.

| Shape | current | fixed | speedup | exact |
|---|---|---|---|---|
| x[1000,1000] y[1000,1000] | 0.229 | 0.235 | 0.97x | yes |
| x[1000,1000] y[1,1] | 0.410 | 0.226 | 1.81x | yes |
| x[1,1] y[1000,1000] | 0.405 | 0.226 | 1.79x | yes |
| x[1000,1000] y[1000,1] | 0.227 | 0.231 | 0.98x | yes |
| x[1000,1000] y[1,1000] | 0.420 | 0.226 | 1.86x | yes |
| x[1,1000] y[1000,1000] | 0.416 | 0.228 | 1.82x | yes |
| x[1,1000,1000] y[1,1000,1000] | 0.235 | 0.235 | 1.00x | yes |
| x[1,1000,1000] y[1,1,1000] | 0.421 | 0.229 | 1.84x | yes |
| x[1,1000,1000] y[1,1000,1] | 0.226 | 0.226 | 1.00x | yes |
| x[1000,1000,1] y[1,1,1] | 0.411 | 0.226 | 1.82x | yes |
| x[100,100,100] y[100,100,100] | 0.235 | 0.235 | 1.00x | yes |
| x[100,100,100] y[1,100,100] | 0.415 | 0.228 | 1.82x | yes |
| x[100,100,100] y[100,1,100] | 0.234 | 0.233 | 1.00x | yes |
| 6d x[10,10,10,10,10,10] y[10,1,10,1,10,1] | 0.377 | 0.378 | 1.00x | yes |
| 6d x[1,10,1,10,1,10] y[10,10,10,10,10,10] | 0.591 | 0.331 | 1.79x | yes |

Every stride 0 case gains 1.79x to 1.86x. Nothing regresses. Both sides
benefit, so it works when `x` is the broadcast operand too.

Machine: aarch64-apple-darwin23, R 4.6.0, Apple clang.

---

# Part 5: What it costs

Compiling the real `src/compare.c` against a patched `src/iterator.h`:

- before: 124,080 bytes

- after: 266,752 bytes

2.15x, so about +139 KB from this one file. `src/arithmetic-*.c` would grow
similarly, since they instantiate the same macro across the same type matrix.

This is the decision to make before building it. If the growth is not
acceptable, the two arm version (`row_stride2 == 0` and general) captures the
common `x op scalar` and `x op row` cases at roughly two thirds the growth, and
gives up only the case where `x` rather than `y` is the broadcast operand.

For comparison, R Core hit exactly this tradeoff with `MOD_ITERATE2` in
`R_ext/Itermacros.h` and resolved it by hand writing the specializations at the
call site in `src/main/arithmetic.c` only, where they judged it worth the code
size. `src/main/relop.c` has no specialization at all, which is why base R's
`x > y` is slower than base R's `x + y` on identical shapes despite writing
half as many output bytes.

---

# Part 6: Reproducing it

The `/tmp` harness used for all of the above is reproduced here so it is not
lost. Save as `verify-fix.c`, build with:

```
clang -O2 -g -fno-common -Wall verify-fix.c -o verify-fix && ./verify-fix
```

It defines both macros side by side, replicates
`rray__location_strides_init()` and `rray__iterator_axes_coalesce2()` verbatim
so shapes are fed through the real coalescing, then for each shape checks
output equality and times both.

```c
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define N 1000000
#define REPS 200
#define RRAY_MAX_DIMENSIONALITY 64
#define NA_LOGICAL INT_MIN

typedef long r_ssize;

struct rray_iterator2 {
  r_ssize index;
  r_ssize size;
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
  r_ssize v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
  int point_dimensionality;
  r_ssize location1;
  r_ssize v_location1_strides[RRAY_MAX_DIMENSIONALITY];
  r_ssize location2;
  r_ssize v_location2_strides[RRAY_MAX_DIMENSIONALITY];
};

static void location_strides_init(
  r_ssize* v_location_strides,
  const int* v_point_dimensions,
  int point_dimensionality,
  const int* v_location_dimensions,
  int location_dimensionality
) {
  r_ssize stride = 1;
  for (int i = 0; i < point_dimensionality; ++i) {
    const int dimension =
      (i < location_dimensionality) ? v_location_dimensions[i] : 1;
    v_location_strides[i] = (dimension == 1) ? 0 : stride;
    stride *= dimension;
  }
}

static bool axes_coalescible(
  r_ssize left_dimension,
  r_ssize left_stride,
  r_ssize right_dimension,
  r_ssize right_stride
) {
  return left_dimension == 1 || right_dimension == 1 ||
    right_stride == left_dimension * left_stride;
}

static int axes_coalesce2(
  r_ssize* v_dimensions,
  r_ssize* v_strides1,
  r_ssize* v_strides2,
  int dimensionality
) {
  int out_axis = 0;
  for (int axis = 1; axis < dimensionality; ++axis) {
    const r_ssize ld = v_dimensions[out_axis];
    const r_ssize ls1 = v_strides1[out_axis];
    const r_ssize ls2 = v_strides2[out_axis];
    const r_ssize rd = v_dimensions[axis];
    const r_ssize rs1 = v_strides1[axis];
    const r_ssize rs2 = v_strides2[axis];

    if (axes_coalescible(ld, ls1, rd, rs1) &&
        axes_coalescible(ld, ls2, rd, rs2)) {
      if (ld == 1) {
        v_strides1[out_axis] = rs1;
        v_strides2[out_axis] = rs2;
      }
      v_dimensions[out_axis] = ld * rd;
    } else {
      ++out_axis;
      v_dimensions[out_axis] = rd;
      v_strides1[out_axis] = rs1;
      v_strides2[out_axis] = rs2;
    }
  }
  return out_axis + 1;
}

#define CURRENT_FOR_EACH(IT, INDEX, LOCATION1, LOCATION2, ...)                 \
  do {                                                                         \
    struct rray_iterator2* const iterator = (IT);                              \
    r_ssize INDEX = iterator->index;                                           \
    const r_ssize size = iterator->size;                                       \
    r_ssize* v_point = iterator->v_point;                                      \
    const r_ssize* v_point_dimensions = iterator->v_point_dimensions;          \
    const int point_dimensionality = iterator->point_dimensionality;           \
    r_ssize LOCATION1 = iterator->location1;                                   \
    const r_ssize* v_location1_strides = iterator->v_location1_strides;        \
    r_ssize LOCATION2 = iterator->location2;                                   \
    const r_ssize* v_location2_strides = iterator->v_location2_strides;        \
                                                                               \
    const r_ssize rows = v_point_dimensions[0];                                \
    const r_ssize row_stride1 = v_location1_strides[0];                        \
    const r_ssize row_stride2 = v_location2_strides[0];                        \
    const r_ssize row_reset1 = rows * row_stride1;                             \
    const r_ssize row_reset2 = rows * row_stride2;                             \
                                                                               \
    while (INDEX != size) {                                                    \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        LOCATION1 += row_stride1;                                              \
        LOCATION2 += row_stride2;                                              \
        ++INDEX;                                                               \
      }                                                                        \
      LOCATION1 -= row_reset1;                                                 \
      LOCATION2 -= row_reset2;                                                 \
                                                                               \
      for (int axis = 1; axis < point_dimensionality; ++axis) {                \
        ++v_point[axis];                                                       \
        if (v_point[axis] < v_point_dimensions[axis]) {                        \
          LOCATION1 += v_location1_strides[axis];                              \
          LOCATION2 += v_location2_strides[axis];                              \
          break;                                                               \
        }                                                                      \
        v_point[axis] = 0;                                                     \
        LOCATION1 -=                                                           \
          (v_point_dimensions[axis] - 1) * v_location1_strides[axis];          \
        LOCATION2 -=                                                           \
          (v_point_dimensions[axis] - 1) * v_location2_strides[axis];          \
      }                                                                        \
    }                                                                          \
  } while (0)

#define RRAY__ITERATOR2_WALK(                                                  \
  INDEX,                                                                       \
  LOCATION1,                                                                   \
  LOCATION2,                                                                   \
  ROW_STRIDE1,                                                                 \
  ROW_STRIDE2,                                                                 \
  ...                                                                          \
)                                                                              \
  do {                                                                         \
    const r_ssize row_reset1 = rows * (ROW_STRIDE1);                           \
    const r_ssize row_reset2 = rows * (ROW_STRIDE2);                           \
                                                                               \
    while (INDEX != size) {                                                    \
      for (r_ssize row = 0; row < rows; ++row) {                               \
        __VA_ARGS__                                                            \
        LOCATION1 += (ROW_STRIDE1);                                            \
        LOCATION2 += (ROW_STRIDE2);                                            \
        ++INDEX;                                                               \
      }                                                                        \
      LOCATION1 -= row_reset1;                                                 \
      LOCATION2 -= row_reset2;                                                 \
                                                                               \
      for (int axis = 1; axis < point_dimensionality; ++axis) {                \
        ++v_point[axis];                                                       \
        if (v_point[axis] < v_point_dimensions[axis]) {                        \
          LOCATION1 += v_location1_strides[axis];                              \
          LOCATION2 += v_location2_strides[axis];                              \
          break;                                                               \
        }                                                                      \
        v_point[axis] = 0;                                                     \
        LOCATION1 -=                                                           \
          (v_point_dimensions[axis] - 1) * v_location1_strides[axis];          \
        LOCATION2 -=                                                           \
          (v_point_dimensions[axis] - 1) * v_location2_strides[axis];          \
      }                                                                        \
    }                                                                          \
  } while (0)

#define FIXED_FOR_EACH(IT, INDEX, LOCATION1, LOCATION2, ...)                   \
  do {                                                                         \
    struct rray_iterator2* const iterator = (IT);                              \
    r_ssize INDEX = iterator->index;                                           \
    const r_ssize size = iterator->size;                                       \
    r_ssize* v_point = iterator->v_point;                                      \
    const r_ssize* v_point_dimensions = iterator->v_point_dimensions;          \
    const int point_dimensionality = iterator->point_dimensionality;           \
    r_ssize LOCATION1 = iterator->location1;                                   \
    const r_ssize* v_location1_strides = iterator->v_location1_strides;        \
    r_ssize LOCATION2 = iterator->location2;                                   \
    const r_ssize* v_location2_strides = iterator->v_location2_strides;        \
                                                                               \
    const r_ssize rows = v_point_dimensions[0];                                \
    const r_ssize row_stride1 = v_location1_strides[0];                        \
    const r_ssize row_stride2 = v_location2_strides[0];                        \
                                                                               \
    if (row_stride2 == 0) {                                                    \
      RRAY__ITERATOR2_WALK(                                                    \
        INDEX, LOCATION1, LOCATION2, row_stride1, 0, __VA_ARGS__               \
      );                                                                       \
    } else if (row_stride1 == 0) {                                             \
      RRAY__ITERATOR2_WALK(                                                    \
        INDEX, LOCATION1, LOCATION2, 0, row_stride2, __VA_ARGS__               \
      );                                                                       \
    } else {                                                                   \
      RRAY__ITERATOR2_WALK(                                                    \
        INDEX, LOCATION1, LOCATION2, row_stride1, row_stride2, __VA_ARGS__     \
      );                                                                       \
    }                                                                          \
  } while (0)

#define COMPARE_BODY                                                           \
  {                                                                            \
    const double x_value = v_x[x_loc];                                         \
    const double y_value = v_y[y_loc];                                         \
    const int missing = (x_value != x_value) | (y_value != y_value);           \
    const int value = x_value < y_value;                                       \
    v_out[i] = missing ? NA_LOGICAL : value;                                   \
  }

void compare_current(
  const double* v_x,
  const double* v_y,
  int* v_out,
  struct rray_iterator2* it
) {
  CURRENT_FOR_EACH(it, i, x_loc, y_loc, COMPARE_BODY);
}

void compare_fixed(
  const double* v_x,
  const double* v_y,
  int* v_out,
  struct rray_iterator2* it
) {
  FIXED_FOR_EACH(it, i, x_loc, y_loc, COMPARE_BODY);
}

static double now(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return ts.tv_sec + ts.tv_nsec * 1e-9;
}

struct shape {
  const char* label;
  int nx;
  int x[6];
  int ny;
  int y[6];
};

static void build(struct rray_iterator2* it, const struct shape* s) {
  const int d = s->nx > s->ny ? s->nx : s->ny;
  int point[RRAY_MAX_DIMENSIONALITY];
  for (int i = 0; i < d; ++i) {
    const int xd = i < s->nx ? s->x[i] : 1;
    const int yd = i < s->ny ? s->y[i] : 1;
    point[i] = xd > yd ? xd : yd;
  }

  memset(it, 0, sizeof(*it));
  it->index = 0;
  it->size = N;

  r_ssize s1[RRAY_MAX_DIMENSIONALITY], s2[RRAY_MAX_DIMENSIONALITY];
  location_strides_init(s1, point, d, s->x, s->nx);
  location_strides_init(s2, point, d, s->y, s->ny);

  for (int i = 0; i < d; ++i) {
    it->v_point_dimensions[i] = point[i];
    it->v_location1_strides[i] = s1[i];
    it->v_location2_strides[i] = s2[i];
  }

  it->point_dimensionality = axes_coalesce2(
    it->v_point_dimensions,
    it->v_location1_strides,
    it->v_location2_strides,
    d
  );
}

typedef void (*fn_t)(
  const double*,
  const double*,
  int*,
  struct rray_iterator2*
);

static double timeit(
  fn_t fn,
  const struct shape* s,
  const double* x,
  const double* y,
  int* out
) {
  struct rray_iterator2 it;
  double best = 1e30;
  for (int r = 0; r < REPS; ++r) {
    build(&it, s);
    __asm__ volatile("" ::: "memory");
    const double start = now();
    fn(x, y, out, &it);
    __asm__ volatile("" : : "r"(out) : "memory");
    const double elapsed = now() - start;
    if (elapsed < best) best = elapsed;
  }
  return best * 1000.0;
}

int main(void) {
  double* x = malloc(N * sizeof(double));
  double* y = malloc(N * sizeof(double));
  int* out = malloc(N * sizeof(int));
  int* reference = malloc(N * sizeof(int));

  for (long i = 0; i < N; ++i) {
    x[i] = (double) (i % 1000);
    y[i] = (double) (i % 999);
  }
  y[12345] = 0.0 / 0.0;
  x[54321] = 0.0 / 0.0;

  const struct shape shapes[] = {
    {"x[1000,1000] y[1000,1000]", 2, {1000, 1000}, 2, {1000, 1000}},
    {"x[1000,1000] y[1,1]", 2, {1000, 1000}, 2, {1, 1}},
    {"x[1,1] y[1000,1000]", 2, {1, 1}, 2, {1000, 1000}},
    {"x[1000,1000] y[1000,1]", 2, {1000, 1000}, 2, {1000, 1}},
    {"x[1000,1000] y[1,1000]", 2, {1000, 1000}, 2, {1, 1000}},
    {"x[1,1000] y[1000,1000]", 2, {1, 1000}, 2, {1000, 1000}},
    {"x[1,1000,1000] y[1,1000,1000]", 3, {1, 1000, 1000}, 3, {1, 1000, 1000}},
    {"x[1,1000,1000] y[1,1,1000]", 3, {1, 1000, 1000}, 3, {1, 1, 1000}},
    {"x[1,1000,1000] y[1,1000,1]", 3, {1, 1000, 1000}, 3, {1, 1000, 1}},
    {"x[1000,1000,1] y[1,1,1]", 3, {1000, 1000, 1}, 3, {1, 1, 1}},
    {"x[100,100,100] y[100,100,100]", 3, {100, 100, 100}, 3, {100, 100, 100}},
    {"x[100,100,100] y[1,100,100]", 3, {100, 100, 100}, 3, {1, 100, 100}},
    {"x[100,100,100] y[100,1,100]", 3, {100, 100, 100}, 3, {100, 1, 100}},
    {"6d x[10..] y[10,1,10,1,10,1]",
     6,
     {10, 10, 10, 10, 10, 10},
     6,
     {10, 1, 10, 1, 10, 1}},
    {"6d x[1,10,1,10,1,10] y[10..]",
     6,
     {1, 10, 1, 10, 1, 10},
     6,
     {10, 10, 10, 10, 10, 10}},
  };
  const int n_shapes = sizeof(shapes) / sizeof(shapes[0]);

  printf(
    "%-40s %8s %8s %7s %8s\n",
    "shape",
    "current",
    "fixed",
    "speedup",
    "exact"
  );

  for (int s = 0; s < n_shapes; ++s) {
    struct rray_iterator2 it;

    build(&it, &shapes[s]);
    compare_current(x, y, reference, &it);

    build(&it, &shapes[s]);
    compare_fixed(x, y, out, &it);

    int ok = 1;
    for (long i = 0; i < N; ++i) {
      if (out[i] != reference[i]) {
        ok = 0;
        break;
      }
    }

    const double current = timeit(compare_current, &shapes[s], x, y, out);
    const double fixed = timeit(compare_fixed, &shapes[s], x, y, out);

    printf(
      "%-40s %8.3f %8.3f %6.2fx %8s\n",
      shapes[s].label,
      current,
      fixed,
      current / fixed,
      ok ? "yes" : "NO"
    );
  }

  return 0;
}
```

To re-check the guard in the generated code:

```
clang -O2 -c verify-fix.c -o verify-fix.o
objdump -d --no-show-raw-insn verify-fix.o | grep -nE 'cmp|ccmp|\.2d'
```

To re-measure the object size cost, copy `src/` somewhere scratch, patch
`iterator.h` there, and compile `compare.c` both ways:

```
clang -O2 -g -fno-common -Wall -I./rlang \
  -I/Library/Frameworks/R.framework/Resources/include \
  -c compare.c -o compare.o
```

---

# Part 7: Do not benchmark this from R

R level timings on this machine are bimodal and will mislead you. The same
`rray_greater_than()` call on identical `[1000,1000]` inputs gave 0.280, 0.432
and 0.453 ms on three consecutive `bench::mark()` passes with 150 iterations
each. Shape effects and this noise are the same size, so an R level survey
invents differences that are not there. An earlier pass at this work misread
that noise as a real result.

The C harness above is stable to about 2 percent across runs. Use it to decide
anything about loop shape. Use R only to confirm the end to end win once the
change is in.

---

# Part 8: Context

Where the compare functions currently stand at 1e6 elements, `min`, against the
alternatives:

| implementation | contiguous | scalar broadcast |
|---|---|---|
| rray4 | 0.286 | 0.442 |
| base R | 1.04 | 1.05 |

So this is an optimization on top of an already large lead, not a fix for a
regression. It is worth doing because scalar and row broadcasting are common,
but it is not urgent.
