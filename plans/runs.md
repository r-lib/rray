# Iterator runs

A proof of concept for the idea sketched in Part 7 of `plans/implementation.md`
under "Iterator runs". Built on `feature/runs`, wired into `rray_add()` only,
to see whether it's worth doing for every arithmetic operator and for
`rray_broadcast()` and `rray_tile()`.

---

# Part 1: The problem

Every per type arithmetic loop steps `rray_iterator2_next_point()` once per
output element, even when no broadcasting is happening and the mapping is the
identity. `rray_iterator2_next_point()` is an odometer: it walks up to
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
per-element odometer stepper. That stepper used to be called
`rray_iterator2_next()`; it's now `rray_iterator2_next_point()`, freeing up
`rray_iterator2_next()` for the run carry function below, which reads better
under that name than under `next_run()`. `split.c`, the only other caller of
the per-element stepper, was updated to match. Everything else here is
additive.

## `done`

```c
struct rray_iterator2 {
  int v_point[RRAY_MAX_DIMENSIONALITY];
  int v_point_dimensions[RRAY_MAX_DIMENSIONALITY];
  int point_dimensionality;
  bool done;

  r_ssize location1;
  r_ssize v_location1_strides[RRAY_MAX_DIMENSIONALITY];

  r_ssize location2;
  r_ssize v_location2_strides[RRAY_MAX_DIMENSIONALITY];
};
```

Set in `rray_iterator2_init()`: `false`, unless any axis has dimension `0`,
in which case the array is empty and there's nothing to iterate at all.

```c
it->done = false;
for (int i = 0; i < point_dimensionality; ++i) {
  it->v_point_dimensions[i] = v_point_dimensions[i];
  if (v_point_dimensions[i] == 0) {
    it->done = true;
  }
}
```

Without this check, an empty axis 0 combined with a huge axis 1 (e.g.
`dim = c(0, 5000000)`) would still walk axis 1 five million times, each one
immediately producing a zero length run. The check makes that O(1) instead of
O(axis 1's dimension); confirmed in Part 5.

## `rray_iterator2_finished()`

```c
static inline bool rray_iterator2_finished(const struct rray_iterator2* it) {
  return it->done;
}
```

## `rray_iterator2_next()`

```c
static inline void rray_iterator2_next(struct rray_iterator2* it) {
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

  it->done = true;
}
```

Carries into axis 1 and up, once a run is over. Same logic
`RRAY_ITERATOR_NEXT`'s `RESET` branch already had, just written out directly
instead of via the macro, and now run once per run instead of once per
element. Axis 0 itself needs no bookkeeping here: `it->v_point[0]`,
`it->location1` and `it->location2` are never touched during a run (that's
all delegated to the run cursor below), so they're still sitting at their
axis 0 start values by the time this runs. When every axis 1+ has also
wrapped, there's nothing left to carry into, and `done` is set.

## `struct rray_iterator2_run`

```c
struct rray_iterator2_run {
  r_ssize location1;
  r_ssize stride1;

  r_ssize location2;
  r_ssize stride2;

  r_ssize remaining;
};

static inline struct rray_iterator2_run rray_iterator2_run(
  const struct rray_iterator2* it
) {
  return (struct rray_iterator2_run){
    .location1 = it->location1,
    .stride1 = it->v_location1_strides[0],
    .location2 = it->location2,
    .stride2 = it->v_location2_strides[0],
    .remaining = it->v_point_dimensions[0],
  };
}

static inline bool rray_iterator2_run_finished(
  const struct rray_iterator2_run* run
) {
  return run->remaining == 0;
}

static inline r_ssize rray_iterator2_run_location1(
  const struct rray_iterator2_run* run
) {
  return run->location1;
}

static inline r_ssize rray_iterator2_run_location2(
  const struct rray_iterator2_run* run
) {
  return run->location2;
}

static inline void rray_iterator2_run_next(struct rray_iterator2_run* run) {
  run->location1 += run->stride1;
  run->location2 += run->stride2;
  --run->remaining;
}
```

`rray_iterator2_run(it)` copies the axis 0 state out of `it` by value, once
per run. It's a function sharing a name with `struct rray_iterator2_run`,
which is fine in C: struct tags and ordinary identifiers live in separate
namespaces, so `struct rray_iterator2_run` and `rray_iterator2_run()` don't
collide. From there the loop body only ever touches this small local, never
`it` directly, which is what lets the compiler keep `location1`/`location2`
in registers for the length of the run rather than reloading them from `it`
on every element. Part 4 has the numbers showing why that distinction
matters. `it` isn't touched again until `rray_iterator2_next(it)` carries
into axis 1+.

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
  while (!rray_iterator2_finished(it)) {
    struct rray_iterator2_run run = rray_iterator2_run(it);

    while (!rray_iterator2_run_finished(&run)) {
      v_out[i] = ONE(
        X_CAST(v_x[rray_iterator2_run_location1(&run)]),
        Y_CAST(v_y[rray_iterator2_run_location2(&run)]),
        error_call
      );
      rray_iterator2_run_next(&run);
      ++i;
    }

    rray_iterator2_next(it);
  }

  FREE(1);
  return out;
```

No run length, stride or `loc + k * stride` indexing left at the call site.
The nested `while` mirrors what a caller of `rray_iterator2` actually wants to
say: while there's more to do, while this run isn't finished, handle one
element and move on, then start the next run.

Only `src/arithmetic-add.c` was switched from `RRAY_ARITHMETIC` to
`RRAY_ARITHMETIC_RUNS`, all 16 type combination bodies. `subtract`,
`multiply`, `divide` and `exponentiate` are untouched and still use the
original macro.

---

# Part 4: Two designs tried, and why the second one lost to a third

The first working version exposed `rray_iterator2_run()`, `stride1()`,
`stride2()` and `advance()` directly on `it`, and the macro pulled them into
local variables itself before a `for (k in 0..run)` loop indexing
`loc + k * stride`. Fast, but every detail of how a run works — its length,
its strides, the index arithmetic — leaked into the call site.

Rewriting the macro to the nested `while` shape in Part 3, but calling
`rray_iterator2_finished_run(it)` / `rray_iterator2_next_location(it)` right
on `it` with no separate cursor, reads a lot better. It also measurably
regressed: `location1`/`location2` live in `it`, so the compiler reloaded them
from memory on every element instead of keeping them in registers for the
run. `struct rray_iterator2_run` — a small value copied out once per run —
gets the clean call site back without paying for it, since the cursor is a
local the compiler can register allocate exactly like the exposed locals
could in the first version.

Benchmark, 5 million elements, same three cases as Part 1:

| case | base R | explicit locals | straight through `it` | run cursor |
|---|---|---|---|---|
| identity | 5.89–6.64ms | 3.15–3.45ms | 3.99ms | **2.96ms** |
| size-1 broadcast | 5.99–6.31ms | 3.97–4.09ms | 7.36ms | **4.19–4.42ms** |
| general broadcast | 5.96–6.45ms | 2.79ms | 4.26–4.38ms | **3.2–3.33ms** |

The run cursor matches or beats the explicit-locals version in every case.
Calling straight through to `it` cost 30–85% more, worst on size-1 broadcast,
where it landed back around base R's own time and erased the whole win.

---

# Part 5: Results

`rray_add()` with the run cursor design, against base R, final numbers from
Part 4's table:

| case | base R | `rray_add()` before this POC | `rray_add()` now |
|---|---|---|---|
| identity | ~6ms | 10.84ms | ~3ms |
| size-1 broadcast | ~6ms | 11.24ms | ~4.3ms |
| general broadcast | ~6ms | 10.76ms | ~3.3ms |

`rray_add()` goes from ~1.8x slower than base R to roughly 1.4x–2x *faster*
than base R, across all three cases.

Correctness: the full test suite passes unchanged (1161 tests) against every
iteration of this design. Also checked by hand against a naive triple nested
loop for cases the test suite doesn't obviously stress: broadcasting on a
middle axis of a 3D array, broadcasting on two axes at once, and a fully
scalar broadcast where every axis has dimension 1. All matched, every time.

The `done` short circuit in `rray_iterator2_init()` (Part 2) was checked
against a pathological empty array: `dim = c(0, 5000000)` broadcast against a
scalar. Without the check this would walk axis 1 five million times for zero
elements of real work. With it, `rray_add()` on that input runs in about 1
microsecond.

---

# Part 6: What's not done here

This is scoped as a proof of concept for `rray_add()` only, per Part 7's
instruction to benchmark the idea as its own pull request before committing to
it everywhere. Left for a follow up if the approach is adopted:

- Apply `RRAY_ARITHMETIC_RUNS` to `subtract`, `multiply`, `divide` and
  `exponentiate`. The macro already has the right shape for all of them, no
  further design work needed there.

- The same run cursor idea for the plain `rray_iterator` (one location, not
  two), which `rray_broadcast()` and `rray_tile()` use.

- Decide whether `RRAY_ARITHMETIC` (the non-runs version) should be deleted
  once nothing uses it, or kept as the simple reference implementation the
  runs version is tested against.

- A run collapsing across axes: today a run only ever spans axis 0. Two full
  size arrays being added with matching multi-dimensional shape still pays one
  `rray_iterator2_next(it)` carry per axis 1+ combination, e.g. once per
  column in a matrix. Detecting when every axis is stride-contiguous and
  collapsing the whole thing to a single run would help the true identity
  case further, at the cost of more machinery in the iterator.
