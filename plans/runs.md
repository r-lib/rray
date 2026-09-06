# Iterator runs

A proof of concept for the idea sketched in Part 7 of `plans/implementation.md`
under "Iterator runs". Built on `feature/runs`, wired into `rray_add()` only,
to see whether it's worth doing for every arithmetic operator and for
`rray_broadcast()` and `rray_tile()`.

---

# Part 1: The problem

Every per type arithmetic loop steps `rray_iterator2_next()` once per output
element, even when no broadcasting is happening and the mapping is the
identity. `rray_iterator2_next()` is an odometer: it walks up to
`point_dimensionality` axes on every call, carrying into the next axis when
the current one wraps. Axis 0 rarely carries, so this is amortized O(1), but
it still costs a loop, a bounds check and a branch on every single element,
whether or not there is anything to carry.

Baseline benchmark, 5 million elements, `int` array plus a `double` scalar
(matching the example already in Part 7):

| case | base R | `rray_add()` |
|---|---|---|
| identity (same dims, no broadcast) | 6.34ms | 10.84ms |
| size-1 broadcast (scalar + array) | 5.99ms | 11.24ms |
| general broadcast (matrix + column recycled along an axis) | 5.96ms | 10.76ms |

The gap is close to identical across all three cases. That's the tell: the
cost isn't proportional to how much broadcasting work there is, it's a fixed
per-element tax paid by the iterator itself.

---

# Part 2: The iterator changes

Added to `struct rray_iterator2` in `src/iterator.h`, alongside the existing
`rray_iterator2_next()`. Nothing existing was changed, this is additive.

## `rray_iterator2_run()`

```c
static inline r_ssize rray_iterator2_run(const struct rray_iterator2* it) {
  return it->v_point_dimensions[0] - it->v_point[0];
}
```

The number of output elements remaining before axis 0 wraps and carries into
axis 1. Axis 0 is walked fastest, so this is exactly the length of the next
contiguous stretch of output where `location1` and `location2` each advance by
a fixed stride per step.

## `rray_iterator2_stride1()` / `rray_iterator2_stride2()`

```c
static inline r_ssize rray_iterator2_stride1(const struct rray_iterator2* it) {
  return it->v_location1_strides[0];
}
```

(same shape for `stride2`, reading `v_location2_strides[0]`)

The per step stride for each input along axis 0. This is `0` when that input
is broadcasting on axis 0 (dimension 1 there), otherwise the input's own axis
0 stride. Exposing it lets a loop body compute `location + k * stride` for
`k` in `0..run` without calling into the iterator at all.

## `rray_iterator2_advance()`

```c
static inline void rray_iterator2_advance(
  struct rray_iterator2* it,
  r_ssize n
) {
  it->location1 += n * it->v_location1_strides[0];
  it->location2 += n * it->v_location2_strides[0];
  it->v_point[0] += (int) n;

  if (it->v_point[0] < it->v_point_dimensions[0]) {
    return;
  }

  it->v_point[0] = 0;
  it->location1 -=
    (r_ssize) it->v_point_dimensions[0] * it->v_location1_strides[0];
  it->location2 -=
    (r_ssize) it->v_point_dimensions[0] * it->v_location2_strides[0];

  for (int i = 1; i < it->point_dimensionality; ++i) {
    ++it->v_point[i];

    if (it->v_point[i] < it->v_point_dimensions[i]) {
      it->location1 += it->v_location1_strides[i];
      it->location2 += it->v_location2_strides[i];
      return;
    }

    it->v_point[i] = 0;
    it->location1 -=
      (it->v_point_dimensions[i] - 1) * it->v_location1_strides[i];
    it->location2 -=
      (it->v_point_dimensions[i] - 1) * it->v_location2_strides[i];
  }
}
```

Moves the iterator forward by `n` steps along axis 0 in one call, rather than
`n` individual calls to `rray_iterator2_next()`. When `n` fills axis 0 exactly
(the normal case, since callers always advance by a full `run`), the naive
`location += n * stride` is corrected back to what `next()` would have landed
on: it subtracts `v_point_dimensions[0] * stride`, then falls through to the
same carry loop `next()` already uses for axis 1 and up. For a partial advance
(`n` less than the run) it returns early, no carry needed.

The carry loop for axis 1+ is unchanged from `RRAY_ITERATOR_NEXT`'s `RESET`
branch, just written out directly instead of via the macro, since it now only
needs to run once per run instead of once per element.

---

# Part 3: The macro

`RRAY_ARITHMETIC_RUNS` in `src/arithmetic.h`, alongside the existing
`RRAY_ARITHMETIC`. Same parameter list, same signature shape, so it drops into
any of the `rray_add_*` bodies unchanged.

```c
#define RRAY_ARITHMETIC_RUNS(
  X_CTYPE,
  X_CONST_DEREF,
  X_CAST,
  Y_CTYPE,
  Y_CONST_DEREF,
  Y_CAST,
  OUT_RTYPE,
  OUT_CTYPE,
  OUT_DEREF,
  ONE
)
  r_obj* out = KEEP(r_alloc_vector(OUT_RTYPE, size));
  OUT_CTYPE* v_out = OUT_DEREF(out);

  const X_CTYPE* v_x = X_CONST_DEREF(x);
  const Y_CTYPE* v_y = Y_CONST_DEREF(y);

  r_ssize i = 0;
  while (i < size) {
    const r_ssize run = rray_iterator2_run(it);
    const r_ssize loc1 = rray_iterator2_location1(it);
    const r_ssize loc2 = rray_iterator2_location2(it);
    const r_ssize stride1 = rray_iterator2_stride1(it);
    const r_ssize stride2 = rray_iterator2_stride2(it);

    for (r_ssize k = 0; k < run; ++k) {
      v_out[i + k] = ONE(
        X_CAST(v_x[loc1 + k * stride1]),
        Y_CAST(v_y[loc2 + k * stride2]),
        error_call
      );
    }

    rray_iterator2_advance(it, run);
    i += run;
  }

  FREE(1);
  return out;
```

The inner `for (k in 0..run)` loop is now a plain strided loop with no
odometer branching, cheap for the compiler to unroll or vectorize. The outer
`while` loop only pays the carry logic once per run, not once per element.

Only `src/arithmetic-add.c` was switched from `RRAY_ARITHMETIC` to
`RRAY_ARITHMETIC_RUNS`, all 16 type combination bodies. `subtract`,
`multiply`, `divide` and `exponentiate` are untouched and still use the
original macro.

---

# Part 4: Results

Same benchmark as Part 1, rerun after the switch:

| case | base R | `rray_add()` before | `rray_add()` after | speedup vs before |
|---|---|---|---|---|
| identity | 6.34ms | 10.84ms | 3.15ms | 3.4x |
| size-1 broadcast | 5.99ms | 11.24ms | 3.97ms | 2.8x |
| general broadcast | 5.96ms | 10.76ms | 2.79ms | 3.9x |

`rray_add()` goes from ~1.8x slower than base R to 1.5x–2.1x *faster* than
base R, across all three cases.

Correctness: the full test suite passes unchanged (1161 tests). Also checked
by hand against a naive triple nested loop for cases the test suite doesn't
obviously stress: broadcasting on a middle axis of a 3D array, broadcasting on
two axes at once, and a fully scalar broadcast where every axis has
dimension 1. All matched.

---

# Part 5: What's not done here

This is scoped as a proof of concept for `rray_add()` only, per Part 7's
instruction to benchmark the idea as its own pull request before committing to
it everywhere. Left for a follow up if the approach is adopted:

- Apply `RRAY_ARITHMETIC_RUNS` to `subtract`, `multiply`, `divide` and
  `exponentiate`. The macro already has the right shape for all of them, no
  further design work needed there.

- The same run and advance idea for the plain `rray_iterator` (one location,
  not two), which `rray_broadcast()` and `rray_tile()` use.

- Decide whether `RRAY_ARITHMETIC` (the non-runs version) should be deleted
  once nothing uses it, or kept as the simple reference implementation the
  runs version is tested against.

- A run collapsing across axes: today a run only ever spans axis 0. Two full
  size arrays being added with matching multi-dimensional shape still pays one
  `advance()` carry per axis 1+ combination, e.g. once per column in a matrix.
  Detecting when every axis is stride-contiguous and collapsing the whole
  thing to a single run would help the true identity case further, at the
  cost of more machinery in the iterator.
