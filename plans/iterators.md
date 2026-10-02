# Run iterators: loops in each function instead of loop macros

Today the loops that walk arrays live in macros: `RRAY_BROADCAST_ATOMIC()`, `RRAY_BINARY()` and `RRAY_REDUCE()`. Each operation passes in a one-element kernel (`ONE`). Anything an operation needs beyond that one-element kernel, like a 64-bit total or a pairwise sum, has to become a new macro parameter or a new macro. `plans/accuracy.md` shows how quickly that grows.

This plan looks at the alternative: an iterator that the operation drives itself, with the loops written in each function. We built three iterator designs into a variant of rray4 and benchmarked them against the current macros on broadcasting, addition and sums.

---

# Summary

- A per-element iterator (a `next()` that hands back `x` and `out` locations for every element) is simplest to use, but too slow. It's up to 7x slower on sums, 2x to 2.7x slower on double addition, and 1.1x to 1.5x slower on broadcasting.

- An iterator that hands back one run at a time, used with a single loop over the run, matches the macros when every operand moves along the run. When one operand stays fixed, it's 1.3x to 1.5x slower on broadcasting and addition, and 4x to 7x slower on sums.

- The same run iterator, with each function writing one loop per stride case (an operand fixed or moving), matches the macros in nearly every case we measured. The exception is very short runs: adding `[10, 1e6] + [10, 1]` (a million runs of 10 elements) is 4% to 6% slower, in every one of six runs. It's faster in one case: summing `[10, 1e6]` over axis 1 takes 2.75 ms against 3.53 ms, because the total stays in a local variable.

- That design is what NumPy does. It is also what the macros already do. The loops are the same, so the speed is the same. What changes is where the loops live: in each function, where they can be read and changed, instead of inside a macro.

- One run iterator can serve any number of inputs, replacing today's separate one-input and two-input plans. It's as fast as the specialized ones, as long as each function passes the number of inputs as a literal (`1`, `2`) and the call is inlined. With the count known only at runtime, short runs get 25% to 75% slower.

- The short-run gap goes away when the iterator does less per run: keep one location array that the iterator advances directly (no per-run copy for the caller), and advance at the end of each run instead of checking for the first run at the start, like the macros do. That version is within 2.5% of the macro on every short-run case.

- The cost is repetition. rray4 has 84 `RRAY_BINARY()` calls, 32 `RRAY_REDUCE()` calls and 5 `RRAY_BROADCAST_ATOMIC()` calls. Writing every stride case out by hand in each of them is a lot of code. How to handle the per-type repetition is the main open question.

Recommendation: adopt one run iterator for any number of inputs, in the "advance at the end" shape, with per-stride-case loops written in each function. Decide separately how to avoid writing every type out by hand.

---

# How it works today

`src/strided-iterator.h` builds a plan from a point space (the dimensions being walked) and a subspace (the strides of the array being read or written). After merging axes that can be walked as one ("coalescing"), the first axis is walked as one run, and later axes advance between runs with `RRAY_STRIDED_ITERATOR_NEXT()`.

The header documents three things that matter here:

- "For performance and flexibility, the user is responsible for managing the run loop along the first axis. We've tried many alternative approaches but they tend to tank performance quickly as you increase the level of abstractions."

- The plan holds only fixed state. The positions that change are kept by the caller, so the compiler can keep them in registers.

- When a stride along the run is 0, the macros take a separate path where the compiler can see the location doesn't change. It then loads the fixed value once and vectorizes the rest.

Each macro owns the full walk: allocate the output, set up the plan, loop over runs, branch on the stride cases, call `ONE` per element, and advance between runs. A few functions (`combine.c`, `index.c`, `permute-axes.c`, `split.c`) already write their own loops against the plan directly, without a macro.

---

# What NumPy does

This section is from knowledge of NumPy's source, not checked against it. NumPy isn't installed on the benchmark machine.

- One iterator, `nditer`, for everything. It reorders axes so the inner loop follows memory order, merges axes that can be walked as one, and hands out one run at a time: a pointer, a length, and a stride for each operand. For reductions, it walks the input in memory order no matter which axis is reduced.

- Reductions reuse the elementwise kernel. `np.add.reduce` calls the same inner loop as `np.add`, with operands `(out, in, out)`. When a whole run feeds one output, the output's stride is 0.

- Each typed inner loop checks its strides and picks a path. The `float64` add loop first checks `IS_BINARY_REDUCE` (output stride 0, output is also the first input). If so, it runs a pairwise sum over the run into one local total. Otherwise it picks a fast path for contiguous operands, a fixed scalar operand, and so on, and uses SIMD there.

- SIMD is written by hand with NumPy's own layer (and Highway for newer kernels), compiled for several CPU levels and picked when NumPy loads. rray4 can only rely on the compiler vectorizing at R's default `-O2`, since CRAN doesn't allow `-march=native`.

- When data needs a type conversion or is misaligned, `nditer` copies it in chunks (8192 elements by default) into a contiguous buffer, and the inner loop runs on that.

- The typed inner loops are generated, not written by hand: once from `.c.src` templates expanded by a Python script, now mostly C++ templates. There is one loop body per operation, expanded across types. The iterator never knows what operation it's running, and the inner loop never knows how many dimensions the array has.

So NumPy uses the "run iterator, per-stride-case loops" design, and avoids writing every type out by generating the per-type loops.

---

# The iterators

Both iterators wrap an existing `rray_strided_iterator_plan` (one subspace) or `rray_strided_iterator2_plan` (two subspaces). They hold the changing positions in a struct on the caller's stack. Because `next()` is `static inline` and the struct never leaves the function, the compiler keeps its fields in registers, so the header's concern about positions in registers doesn't apply. That held for every case below, with Apple clang 17. GCC is not checked.

## Run iterator

`next()` advances one run. The caller gets the run's range in the point space (`start` to `end`) and, for each subspace, where the run starts (`loc`) and how far to move per element (`stride`).

```c
struct rray_run_iterator {
  r_ssize start;
  r_ssize end;
  r_ssize loc;
  r_ssize stride;

  r_ssize size;
  r_ssize run_size;
  r_ssize loc_start;
  const struct rray_strided_iterator_plan* plan;
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
};

static inline struct rray_run_iterator rray_run_iterator(
  const struct rray_strided_iterator_plan* plan
) {
  struct rray_run_iterator it;

  it.plan = plan;
  it.size = rray_strided_iterator_plan_size(plan);
  it.run_size = rray_strided_iterator_plan_run_size(plan);
  it.stride = rray_strided_iterator_plan_run_stride(plan);

  it.start = 0;
  it.end = 0;
  it.loc = 0;
  it.loc_start = 0;

  rray_strided_iterator_plan_point_init(plan, it.v_point);

  return it;
}

static inline bool rray_run_iterator_next(struct rray_run_iterator* it) {
  if (it->end == it->size) {
    return false;
  }

  if (it->end != 0) {
    const struct rray_strided_iterator_plan* plan = it->plan;
    RRAY_STRIDED_ITERATOR_NEXT(it->loc_start, it->v_point, plan);
  }

  it->start = it->end;
  it->end = it->start + it->run_size;
  it->loc = it->loc_start;

  return true;
}
```

`rray_run_iterator2` is the same, with `loc1`, `loc2`, `stride1` and `stride2`, built on `rray_strided_iterator2_plan` and `RRAY_STRIDED_ITERATOR_NEXT2()`. The full code is in the appendix.

The meaning of the two spaces depends on the operation, as it does for the plan today:

- Broadcasting and binary operations walk the output. `start` to `end` indexes the output, and `loc` indexes the input.

- Reductions walk the input. `start` to `end` indexes the input, and `loc` indexes the output.

## Element iterator

`next()` advances one element and hands back `index` (in the point space) and `loc` (in the subspace). It starts at `loc = -stride` and `run_i = -1`, so the first call goes through the same "next element in this run" path as every other call, without a special first branch. The full code is in the appendix. It is here as the baseline that was ruled out.

---

# Worked examples

All three use the run iterator with a loop per stride case. These are the `run_split` variants from the benchmarks, exactly as they were compiled.

## Broadcast

One input, walking the output. When the input's stride is 0, the run repeats one value.

```c
static r_obj* rray_broadcast_dbl(
  r_obj* x,
  const struct rray_strided_iterator_plan* plan
) {
  const r_ssize size = rray_strided_iterator_plan_size(plan);

  r_obj* out = KEEP(r_alloc_double(size));
  double* v_out = r_dbl_begin(out);

  const double* v_x = r_dbl_cbegin(x);

  struct rray_run_iterator it = rray_run_iterator(plan);

  while (rray_run_iterator_next(&it)) {
    if (it.stride == 0) {
      const double x_elt = v_x[it.loc];

      for (r_ssize i = it.start; i < it.end; ++i) {
        v_out[i] = x_elt;
      }
    } else {
      r_ssize x_loc = it.loc;

      for (r_ssize i = it.start; i < it.end; ++i) {
        v_out[i] = v_x[x_loc];
        x_loc += it.stride;
      }
    }
  }

  FREE(1);
  return out;
}
```

For example, broadcasting `[1, 4000]` to `[4000, 4000]` gives 4000 runs of 4000 elements, each with input stride 0. Every run takes the first branch and fills with one value. Broadcasting `[4000, 1]` to `[4000, 4000]` gives 4000 runs with input stride 1, each copying the same 4000 contiguous values.

## Add

Two inputs, walking the output. Each input is either fixed or moving along the run, so there are four cases. The integer version passes `error_call` to its one-element kernel directly, with no `RRAY_BINARY_ARGS()` needed.

```c
static r_obj* rray_add_int_int(
  r_obj* x,
  r_obj* y,
  const struct rray_strided_iterator2_plan* plan,
  struct r_lazy error_call
) {
  const r_ssize size = rray_strided_iterator2_plan_size(plan);

  r_obj* out = KEEP(r_alloc_integer(size));
  int* v_out = r_int_begin(out);

  const int* v_x = r_int_cbegin(x);
  const int* v_y = r_int_cbegin(y);

  struct rray_run_iterator2 it = rray_run_iterator2(plan);

  while (rray_run_iterator2_next(&it)) {
    r_ssize x_loc = it.loc1;
    r_ssize y_loc = it.loc2;

    if (it.stride1 == 0 && it.stride2 == 0) {
      const int elt = rray_add_int_one(v_x[x_loc], v_y[y_loc], error_call);
      for (r_ssize i = it.start; i < it.end; ++i) {
        v_out[i] = elt;
      }
    } else if (it.stride1 == 0) {
      const int x_elt = v_x[x_loc];
      for (r_ssize i = it.start; i < it.end; ++i) {
        v_out[i] = rray_add_int_one(x_elt, v_y[y_loc], error_call);
        y_loc += it.stride2;
      }
    } else if (it.stride2 == 0) {
      const int y_elt = v_y[y_loc];
      for (r_ssize i = it.start; i < it.end; ++i) {
        v_out[i] = rray_add_int_one(v_x[x_loc], y_elt, error_call);
        x_loc += it.stride1;
      }
    } else {
      for (r_ssize i = it.start; i < it.end; ++i) {
        v_out[i] = rray_add_int_one(v_x[x_loc], v_y[y_loc], error_call);
        x_loc += it.stride1;
        y_loc += it.stride2;
      }
    }
  }

  FREE(1);
  return out;
}
```

The double version is the same with `double`, `r_alloc_double()`, `r_dbl_begin()` / `r_dbl_cbegin()` and `rray_add_dbl_one(x, y)`.

For example, `[4000, 4000] + [1, 4000]` gives 4000 runs where `x` moves (stride 1) and `y` is fixed (stride 0), so every run takes the third branch. `[4000, 4000] + [4000, 4000]` coalesces into one run of 16 million elements where both move.

## Sum

One input, walking the input and adding into the output. When the output's stride is 0, the whole run feeds one output, and the total is kept in a local variable.

```c
static r_obj* rray_sum_dbl(
  r_obj* x,
  r_ssize out_size,
  const struct rray_strided_iterator_plan* plan,
  struct r_lazy error_call
) {
  r_obj* out = KEEP(r_alloc_double(out_size));
  double* v_out = r_dbl_begin(out);

  for (r_ssize i = 0; i < out_size; ++i) {
    v_out[i] = 0.0;
  }

  const double* v_x = r_dbl_cbegin(x);

  struct rray_run_iterator it = rray_run_iterator(plan);

  while (rray_run_iterator_next(&it)) {
    if (it.stride == 0) {
      double sum = v_out[it.loc];

      for (r_ssize i = it.start; i < it.end; ++i) {
        sum = rray_sum_dbl_one(sum, v_x[i]);
      }

      v_out[it.loc] = sum;
    } else {
      r_ssize out_loc = it.loc;

      for (r_ssize i = it.start; i < it.end; ++i) {
        v_out[out_loc] = rray_sum_dbl_one(v_out[out_loc], v_x[i]);
        out_loc += it.stride;
      }
    }
  }

  FREE(1);
  return out;
}
```

For example, summing `[4000, 4000]` over axis 1 gives 4000 runs with output stride 0: each run is one column, added into one output. Summing over axis 2 gives 4000 runs with output stride 1: each run adds one column into all 4000 outputs at once, which the compiler vectorizes.

## What this means for `plans/accuracy.md`

The accuracy plan's changes become ordinary code in each function, with no new macro:

- Pairwise summation goes in the `it.stride == 0` branch of the double sum, replacing the loop with a call that sums `v_x + it.start` over `it.end - it.start` elements.

- The 64-bit integer sum allocates an `int64_t` buffer of `out_size` next to `out`, adds into it in both branches (a local `int64_t` total in the `it.stride == 0` branch), and converts and checks each total at the end.

- `RRAY_REDUCE_ACC()` and its `RUN` / `ONE` / `FINISH` hooks are no longer needed.

---

# Benchmarks

## Setup

- Apple M2 Pro, Apple clang 17.0.0, R 4.6.0 alpha, R's default `CFLAGS` (`-O2`), `bench` 1.1.4.

- Median of 20 iterations, in ms.

- Doubles from `runif()`, integers from `sample(100L, n, TRUE)`.

- The whole suite was run twice. The tables show the second run. Broadcasting and addition varied by up to about 10% between runs (the first broadcast case was 9.27 ms for the macro in the first run, 8.09 ms in the second). Sums varied by less than 3%. No conclusion below depends on a difference smaller than that.

## The variant of rray4

A copy of rray4 at `9e171e1` (`main`), with:

- `src/run-iterator.h` and `src/element-iterator.h`, as listed in the appendix.

- `src/iterator-variant.c` / `.h`: a global `rray_iterator_variant`, set from R through `ffi_rray_set_iterator_variant()` or at load time from the `RRAY_ITERATOR_VARIANT` environment variable.

- `rray_broadcast_dbl()`, `rray_add_dbl_dbl()`, `rray_add_int_int()` and `rray_sum_dbl()` each split into four functions, one per variant, plus a `switch` on `rray_iterator_variant`:

  - `macro`: the current code, unchanged.

  - `element`: the element iterator, with the one-element kernel called once per element.

  - `run`: the run iterator, with one loop over each run using the strides as runtime values.

  - `run_split`: the run iterator, with one loop per stride case, as in the worked examples.

  - `run_n` and `run_n_runtime`: added later, see "Any number of inputs".

  - `lean` and `advance`: added later, see "Removing the per-run cost".

All four variants pass `test-broadcast.R`, `test-arithmetic-add.R` and `test-reduce-sum.R` (124 tests, 262 expectations each). The benchmark script also checks that every variant returns an `identical()` result to `macro` in every case before timing anything.

## Broadcast, double

| Case | Macro | Element | Run | Run split |
|---|---|---|---|---|
| `[4000, 1]` to `[4000, 4000]` | 8.09 | 11.08 | 7.82 | 8.23 |
| `[1, 4000]` to `[4000, 4000]` | 7.99 | 10.78 | 10.26 | 7.83 |
| `[1, 1]` to `[4000, 4000]` | 7.95 | 10.78 | 10.11 | 7.99 |
| `[200, 1, 200]` to `[200, 200, 200]` | 3.97 | 5.56 | 3.91 | 3.94 |
| `[1, 200, 200]` to `[200, 200, 200]` | 4.04 | 5.94 | 5.74 | 3.94 |
| `[10, 1]` to `[10, 1e6]` | 7.19 | 7.94 | 6.19 | 6.32 |
| `[1, 1e6]` to `[10, 1e6]` | 6.63 | 7.82 | 8.13 | 6.74 |

## Add, double

| Case | Macro | Element | Run | Run split |
|---|---|---|---|---|
| `[4000, 4000] + [4000, 4000]` | 11.63 | 23.65 | 11.53 | 11.77 |
| `[4000, 4000] + [4000, 1]` | 9.91 | 23.54 | 10.12 | 10.03 |
| `[4000, 4000] + [1, 4000]` | 9.65 | 23.40 | 12.76 | 9.84 |
| `[4000, 4000] + [1, 1]` | 9.68 | 23.82 | 12.08 | 9.65 |
| `[4000, 1] + [1, 4000]` | 8.72 | 23.32 | 13.00 | 7.97 |
| `[200, 200, 200] + [1, 200, 1]` | 4.93 | 8.22 | 6.43 | 4.95 |
| `[10, 1e6] + [10, 1]` | 7.68 | 10.74 | 7.96 | 8.09 |
| `[10, 1e6] + [1, 1e6]` | 7.25 | 10.81 | 8.97 | 7.60 |

## Add, integer

| Case | Macro | Element | Run | Run split |
|---|---|---|---|---|
| `[4000, 4000] + [4000, 4000]` | 16.39 | 17.65 | 16.41 | 16.18 |
| `[4000, 4000] + [4000, 1]` | 16.29 | 17.25 | 16.26 | 16.24 |
| `[4000, 4000] + [1, 4000]` | 19.37 | 17.28 | 16.26 | 19.77 |
| `[10, 1e6] + [10, 1]` | 11.21 | 11.11 | 10.81 | 11.60 |
| `[10, 1e6] + [1, 1e6]` | 13.20 | 11.16 | 10.72 | 13.90 |

## Sum, double

| Case | Macro | Element | Run | Run split |
|---|---|---|---|---|
| `[4000, 4000]` over 1 | 13.07 | 53.76 | 53.80 | 13.05 |
| `[4000, 4000]` over 2 | 2.28 | 6.25 | 2.27 | 2.26 |
| `[4000, 4000]` over 1, 2 | 13.51 | 54.06 | 54.06 | 13.53 |
| `[200, 200, 200]` over 1 | 3.62 | 23.85 | 23.86 | 3.61 |
| `[200, 200, 200]` over 2 | 1.19 | 3.34 | 1.18 | 1.18 |
| `[200, 200, 200]` over 3 | 1.69 | 3.20 | 1.69 | 1.69 |
| `[200, 200, 200]` over 1, 3 | 3.60 | 23.83 | 23.85 | 3.59 |
| `[10, 1e6]` over 1 | 3.53 | 7.30 | 7.38 | 2.75 |
| `[10, 1e6]` over 2 | 3.77 | 4.65 | 3.77 | 3.74 |

## What the numbers say

- The element iterator is slower everywhere. "Did this run just end?" is checked on every element, which stops the compiler from vectorizing. Sums are hit hardest: an output total down a column has to be written to memory and read back on every add.

- The element iterator costs least on integer addition (about 6%), because the overflow check already stops that loop from vectorizing.

- The run iterator with one loop is as fast as the macro whenever every operand moves along the run. When an operand is fixed, it loses: up to 1.5x on addition, 1.3x on broadcasting, and 4x to 7x on sums. With a runtime stride, the compiler can't tell the location never moves, so it reloads the fixed value (or re-stores the total) on every element.

- The run iterator with one loop per stride case is within run-to-run noise of the macro nearly everywhere, because it is the same set of loops. The exception is addition with very short runs. Across six runs of the suite, `[10, 1e6] + [10, 1]` took 7.56 to 7.92 ms with the macro and 8.03 to 8.43 ms with the run iterator (integer: 10.97 to 11.29 ms against 11.56 to 11.81 ms). With a million runs of 10 elements, the iterator's per-run work shows up, most likely the `it->end != 0` check and copying the run's starting locations. It hasn't been looked into further. Summing `[10, 1e6]` over axis 1 is faster (2.75 ms against 3.53 ms): its total lives in a local variable, while the macro reads and writes `v_out[out_loc]` on each add. Runs here are only 10 elements long, so that matters more.

- Broadcasting and addition are mostly limited by writing a 128 MB output, so the differences between designs are smaller there than for sums, which write almost nothing.

- An unexpected result: for integer addition with a fixed operand, the specialized loop is about 20% slower than the plain loop (19.4 ms against 16.3 ms for `[4000, 4000] + [1, 4000]`). This affects the current macro too, not just the new design. It hasn't been looked into. A guess is that clang compiles the overflow check differently once one side is a known fixed value. Checked operations could skip the fixed-operand cases, which would also make them shorter.

---

# Any number of inputs

Today there are three plans: `rray_strided_iterator_plan` (one input), `rray_strided_iterator2_plan` (two inputs) and `rray_strided_iterator_n_plan` (any number). `src/index.c` drives the last one through a `switch` with `case 1:` to `case 4:`, passing the count to `RRAY_STRIDED_ITERATOR_NEXT_N()` as a literal so the compiler unrolls its loops over inputs.

## What NumPy does

From knowledge of `nditer_templ.c.src` and `nditer_api.c`, not checked against the source.

- `nditer` is written once for any number of operands. A template generates specialized copies of its `next()` for 1 operand, 2 operands, and any number (crossed with 1, 2, or any number of dimensions, and with its flags). `NpyIter_GetIterNext()` picks one and returns a function pointer. That is the same idea as rray4's `case 1:` to `case 4:`, generated instead of written by hand.

- Ufuncs call `next()` once per run, not per element, so the generic per-operand loops only run between runs.

- The per-element loop belongs to the ufunc, which always knows its operand count (a binary ufunc has 2 inputs and 1 output). It reads `args[0]`, `args[1]` and `args[2]` directly and never loops over operands.

## `rray_run_iterator_n`

The same as `rray_run_iterator`, with arrays of per-input locations and strides, and the number of inputs passed to both functions. When the caller passes a literal and the functions are inlined, the loops over `n` unroll away.

```c
#define RRAY_MAX_INPUTS 8

struct rray_run_iterator_n {
  r_ssize start;
  r_ssize end;
  r_ssize v_loc[RRAY_MAX_INPUTS];
  r_ssize v_stride[RRAY_MAX_INPUTS];

  r_ssize size;
  r_ssize run_size;
  r_ssize v_loc_start[RRAY_MAX_INPUTS];
  const struct rray_strided_iterator_n_plan* plan;
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
};

static inline struct rray_run_iterator_n rray_run_iterator_n(
  const struct rray_strided_iterator_n_plan* plan,
  const r_ssize n
) {
  struct rray_run_iterator_n it;

  it.plan = plan;
  it.size = rray_strided_iterator_n_plan_size(plan);
  it.run_size = rray_strided_iterator_n_plan_run_size(plan);

  it.start = 0;
  it.end = 0;

  for (r_ssize i = 0; i < n; ++i) {
    it.v_stride[i] = rray_strided_iterator_n_plan_run_stride(plan, i);
    it.v_loc[i] = 0;
    it.v_loc_start[i] = 0;
  }

  rray_strided_iterator_n_plan_point_init(plan, it.v_point);

  return it;
}

static inline bool rray_run_iterator_n_next(
  struct rray_run_iterator_n* it,
  const r_ssize n
) {
  if (it->end == it->size) {
    return false;
  }

  if (it->end != 0) {
    const struct rray_strided_iterator_n_plan* plan = it->plan;
    RRAY_STRIDED_ITERATOR_NEXT_N(it->v_loc_start, it->v_point, plan, n);
  }

  it->start = it->end;
  it->end = it->start + it->run_size;

  for (r_ssize i = 0; i < n; ++i) {
    it->v_loc[i] = it->v_loc_start[i];
  }

  return true;
}
```

Add then reads like the two-input version, with `it.v_loc[0]` / `it.v_loc[1]` and `it.v_stride[0]` / `it.v_stride[1]` in place of `loc1` / `loc2` and `stride1` / `stride2`:

```c
struct rray_run_iterator_n it = rray_run_iterator_n(plan, 2);

while (rray_run_iterator_n_next(&it, 2)) {
  r_ssize x_loc = it.v_loc[0];
  r_ssize y_loc = it.v_loc[1];

  if (it.v_stride[0] == 0 && it.v_stride[1] == 0) {
    ...
  }
}
```

In the variant package, the existing one-input and two-input plans were repackaged into a `rray_strided_iterator_n_plan` (copying their already merged dimensions and strides into the axis-major stride layout) at the top of each function. A real version would build the n plan directly.

## Benchmarks

Two more versions were added to the variant package. Both use exactly the same loops as `run_split`:

- `run_n`: `rray_run_iterator_n` with a literal count, `1` for broadcast and sum, `2` for add.

- `run_n_runtime`: the same, with the count passed through a `volatile` variable so the compiler can't see its value. This stands in for code where the count is decided at runtime, like `rray_index()`.

Both pass the same 262 test expectations, and match `macro` with `identical()` in every case.

The first attempt shared one function body between the two versions, with each wrapper passing its count. clang didn't inline that body into the literal wrapper, so the literal never reached the iterator, and `run_n` was as slow as `run_n_runtime`. Marking the shared body `__attribute__((always_inline))` fixed it. In real code each function calls the iterator directly with its literal, which is what the first `run_n` measurement did, and it matched `run_split` without any forced inlining. But it shows the speed depends on the compiler actually seeing the literal.

Median ms, with inlining forced:

| Case | Macro | Run split | Run n | Run n, runtime count |
|---|---|---|---|---|
| Broadcast `[10, 1]` to `[10, 1e6]` | 6.24 | 6.38 | 6.45 | 8.07 |
| Broadcast `[1, 1e6]` to `[10, 1e6]` | 6.68 | 7.12 | 7.25 | 8.11 |
| Broadcast `[200, 1, 200]` to `[200, 200, 200]` | 4.05 | 4.13 | 4.05 | 4.37 |
| Add `[10, 1e6] + [10, 1]` | 7.69 | 8.33 | 8.71 | 10.76 |
| Add `[10, 1e6] + [1, 1e6]` | 7.24 | 7.68 | 7.92 | 10.25 |
| Add, integer, `[10, 1e6] + [10, 1]` | 10.97 | 11.57 | 12.08 | 14.89 |
| Add, integer, `[10, 1e6] + [1, 1e6]` | 13.23 | 13.84 | 13.54 | 15.68 |
| Sum `[10, 1e6]` over 1 | 3.53 | 2.65 | 2.93 | 4.68 |
| Sum `[10, 1e6]` over 2 | 3.79 | 3.75 | 3.75 | 5.23 |
| Sum `[200, 200, 200]` over 2 | 1.18 | 1.18 | 1.18 | 1.28 |

Every other case in the suite (long runs: the `[4000, 4000]` and most `[200, 200, 200]` cases) was the same for all four versions, within noise.

With `RRAY_MAX_INPUTS` raised to 64, so `rray_index()` could use it, `run_n` was unchanged: for example 2.84 ms on the short sum along axis 1 and 6.16 ms on the short broadcast. The compiler still keeps the used slots in registers.

## What the numbers say

- With a literal count, one iterator for any number of inputs is as fast as the separate one-input and two-input versions. The small differences between `run_split` and `run_n` move in both directions between runs.

- A count known only at runtime costs 25% to 75% on short runs, and nothing on long runs. The loops over inputs run once per run, so they only matter when there are millions of runs.

- So the plan structs can be merged into one, as long as every function passes its count as a literal. Code like `rray_index()`, where the count really is decided at runtime, keeps its `switch` on the count, with a literal in each case.

---

# Removing the per-run cost

`rray_run_iterator_n` does two things per run that the macros don't:

- It keeps two location arrays. `v_loc_start` is what `next()` advances, and `v_loc` is a copy made at the start of every run for the caller. The copy only protects against a caller that changes `it.v_loc[i]` while walking the run. No caller does: every loop copies the start into a local (`r_ssize x_loc = it.v_loc[0];`) and moves the local.

- `next()` checks `it->end != 0` on every run, so that the first call doesn't advance.

## Two leaner iterators

`rray_run_iterator_n_lean` keeps one array, `v_loc`, which `next()` advances directly. The caller reads it and never writes it. C can't enforce that, since the iterator itself has to change the field. It still has the first-run check.

```c
static inline bool rray_run_iterator_n_lean_next(
  struct rray_run_iterator_n_lean* it,
  const r_ssize n
) {
  if (it->end == it->size) {
    return false;
  }

  if (it->end != 0) {
    const struct rray_strided_iterator_n_plan* plan = it->plan;
    RRAY_STRIDED_ITERATOR_NEXT_N(it->v_loc, it->v_point, plan, n);
  }

  it->start = it->end;
  it->end = it->start + it->run_size;

  return true;
}
```

`rray_run_iterator_n_advance` also drops the first-run check. Its constructor already describes the first run (`it.end = it.run_size`), and the loop advances at the end of each run. That is the same shape as the macros' `while (run_start != size) { ...; RRAY_STRIDED_ITERATOR_NEXT(); }`.

```c
static inline bool rray_run_iterator_n_advance_done(
  const struct rray_run_iterator_n_advance* it
) {
  return it->start == it->size;
}

static inline void rray_run_iterator_n_advance_next(
  struct rray_run_iterator_n_advance* it,
  const r_ssize n
) {
  const struct rray_strided_iterator_n_plan* plan = it->plan;
  RRAY_STRIDED_ITERATOR_NEXT_N(it->v_loc, it->v_point, plan, n);

  it->start = it->end;
  it->end = it->start + it->run_size;
}
```

The caller's loop becomes a `for` instead of a `while`:

```c
struct rray_run_iterator_n_advance it = rray_run_iterator_n_advance(plan, 2);

for (; !rray_run_iterator_n_advance_done(&it);
     rray_run_iterator_n_advance_next(&it, 2)) {
  r_ssize x_loc = it.v_loc[0];
  r_ssize y_loc = it.v_loc[1];
  ...
}
```

After the last run, `next()` carries the point back to zero one extra time. That only changes `v_point` and `v_loc`, which nothing reads afterwards. An empty array (`size == 0`) is done before the first run.

## Benchmarks

Both versions use exactly the same loops as `run_n`, pass the same 262 test expectations, and match `macro` with `identical()` in every case. `RRAY_MAX_INPUTS` was 64 for these runs.

These were measured in a later session, when the machine ran about 7% slower overall than for the tables above (`[4000, 4000]` summed over axis 1 took 13.8 ms instead of 13.05 ms), so compare only within this table. The long-run cases were the same for every version, within noise, across two runs of the full suite. The short-run cases were then rerun on their own: 3 rounds of 50 iterations each, with the order of the versions rotated between rounds, taking the median across rounds.

| Case | Macro | Run split | Run n | Lean | Advance |
|---|---|---|---|---|---|
| Broadcast `[10, 1]` to `[10, 1e6]` | 7.08 | 7.14 | 7.06 | 7.13 | 7.16 |
| Broadcast `[1, 1e6]` to `[10, 1e6]` | 7.00 | 7.09 | 7.39 | 7.22 | 7.32 |
| Add `[10, 1e6] + [10, 1]` | 8.36 | 9.12 | 9.60 | 8.91 | 8.57 |
| Add `[10, 1e6] + [1, 1e6]` | 7.98 | 8.28 | 8.70 | 8.08 | 7.81 |
| Add, integer, `[10, 1e6] + [10, 1]` | 12.08 | 12.58 | 13.13 | 13.18 | 12.02 |
| Add, integer, `[10, 1e6] + [1, 1e6]` | 14.46 | 15.16 | 14.50 | 14.79 | 14.36 |
| Sum `[10, 1e6]` over 1 | 3.75 | 2.82 | 3.03 | 2.91 | 3.06 |
| Sum `[10, 1e6]` over 2 | 4.02 | 3.99 | 3.99 | 3.99 | 3.92 |

## What the numbers say

- Addition is where the per-run cost shows. `advance` is within 2.5% of the macro in all four add cases, and slightly faster in three. `run_n` was 3% to 15% slower.

- Dropping the copy alone (`lean`) gets about two thirds of the way on double addition, but was mixed on integer addition. Dropping the first-run check as well is what closes the gap everywhere.

- Broadcasting and the short sum don't separate the three newer iterator versions beyond noise. All of them beat the macro on the short sum along axis 1, because the total stays in a local variable.

- Differences under about 3% shouldn't be read into, given the noise in this session.

So the recommended iterator is the `advance` shape: one location array, advanced at the end of each run.

---

# The cost: repetition

Moving the loops into each function means each function carries its own copy of the stride cases. Today:

| Macro | Call sites | Stride cases per call |
|---|---|---|
| `RRAY_BINARY()` | 84 | 4 |
| `RRAY_REDUCE()` | 32 | 2 |
| `RRAY_REDUCE_OUTER()` | 6 | 1 (plus an inner loop) |
| `RRAY_BROADCAST_ATOMIC()` | 5 | 2 |
| `RRAY_BROADCAST_BARRIER()` | 2 | 2 |

The add example above is about 50 lines. Written out by hand, 84 binary call sites is several thousand lines that differ only in types and the one-element kernel. Most of those call sites differ only by type: `rray_add_dbl_dbl()`, `rray_add_int_dbl()`, `rray_add_lgl_dbl()` and so on.

Options for the per-type repetition:

- Write every type out. Every loop is plain C you can read. The most code, and a fix to one loop has to be copied to the others.

- One loop body per operation, stamped out per type by a small macro. This is what NumPy does with templates. The macro would only fill in types and the kernel, not hide the loop structure: the loops for `add` would be written once in `arithmetic-add.c`, not in a shared header. Different operations can still choose different loops, which is the point.

- A mix: write out the operations with special needs (sums, means, anything in `plans/accuracy.md`), and keep a type-stamping macro for the many plain elementwise operations (arithmetic, comparison, equality).

---

# Open questions

- How should the per-type repetition be handled? Write it out, stamp it per type, or a mix?

- Should checked integer operations skip the fixed-operand cases, given they were slower there?

- Should the run iterators replace direct uses of `RRAY_STRIDED_ITERATOR_NEXT()` in `combine.c`, `index.c`, `permute-axes.c` and `split.c` too, so there's one way to walk an array?

- Should `rray_strided_iterator_plan` and `rray_strided_iterator2_plan` be replaced by `rray_strided_iterator_n_plan` and `rray_run_iterator_n`, so there is one plan and one iterator?

- The `advance` shape needs two calls (`done()` and `next()`) and a `for` loop instead of one `while (next())`. Is that acceptable, or is there a single-call shape that is just as fast?

- None of this has been measured with GCC or on x86_64. Is one Linux run worth doing before committing to the design?

---

# Appendix

## `src/run-iterator.h`, two subspaces

```c
struct rray_run_iterator2 {
  r_ssize start;
  r_ssize end;
  r_ssize loc1;
  r_ssize loc2;
  r_ssize stride1;
  r_ssize stride2;

  r_ssize size;
  r_ssize run_size;
  r_ssize loc1_start;
  r_ssize loc2_start;
  const struct rray_strided_iterator2_plan* plan;
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
};

static inline struct rray_run_iterator2 rray_run_iterator2(
  const struct rray_strided_iterator2_plan* plan
) {
  struct rray_run_iterator2 it;

  it.plan = plan;
  it.size = rray_strided_iterator2_plan_size(plan);
  it.run_size = rray_strided_iterator2_plan_run_size(plan);
  it.stride1 = rray_strided_iterator2_plan_run_stride1(plan);
  it.stride2 = rray_strided_iterator2_plan_run_stride2(plan);

  it.start = 0;
  it.end = 0;
  it.loc1 = 0;
  it.loc2 = 0;
  it.loc1_start = 0;
  it.loc2_start = 0;

  rray_strided_iterator2_plan_point_init(plan, it.v_point);

  return it;
}

static inline bool rray_run_iterator2_next(struct rray_run_iterator2* it) {
  if (it->end == it->size) {
    return false;
  }

  if (it->end != 0) {
    const struct rray_strided_iterator2_plan* plan = it->plan;
    RRAY_STRIDED_ITERATOR_NEXT2(
      it->loc1_start,
      it->loc2_start,
      it->v_point,
      plan
    );
  }

  it->start = it->end;
  it->end = it->start + it->run_size;
  it->loc1 = it->loc1_start;
  it->loc2 = it->loc2_start;

  return true;
}
```

## `src/element-iterator.h`, one subspace

The two subspace version is the same, with `loc1` / `loc2` and `RRAY_STRIDED_ITERATOR_NEXT2()`.

```c
struct rray_element_iterator {
  r_ssize index;
  r_ssize loc;

  r_ssize size;
  r_ssize run_i;
  r_ssize run_size;
  r_ssize run_stride;
  r_ssize start;
  const struct rray_strided_iterator_plan* plan;
  r_ssize v_point[RRAY_MAX_DIMENSIONALITY];
};

static inline struct rray_element_iterator rray_element_iterator(
  const struct rray_strided_iterator_plan* plan
) {
  struct rray_element_iterator it;

  it.plan = plan;
  it.size = rray_strided_iterator_plan_size(plan);
  it.run_size = rray_strided_iterator_plan_run_size(plan);
  it.run_stride = rray_strided_iterator_plan_run_stride(plan);

  it.index = -1;
  it.run_i = -1;
  it.start = 0;
  it.loc = -it.run_stride;

  rray_strided_iterator_plan_point_init(plan, it.v_point);

  return it;
}

static inline bool rray_element_iterator_next(
  struct rray_element_iterator* it
) {
  ++it->index;

  if (it->index == it->size) {
    return false;
  }

  ++it->run_i;

  if (it->run_i != it->run_size) {
    it->loc += it->run_stride;
    return true;
  }

  const struct rray_strided_iterator_plan* plan = it->plan;
  RRAY_STRIDED_ITERATOR_NEXT(it->start, it->v_point, plan);

  it->run_i = 0;
  it->loc = it->start;

  return true;
}
```

Its loops are one line each. Broadcast:

```c
while (rray_element_iterator_next(&it)) {
  v_out[it.index] = v_x[it.loc];
}
```

Add:

```c
while (rray_element_iterator2_next(&it)) {
  v_out[it.index] = rray_add_dbl_one(v_x[it.loc1], v_y[it.loc2]);
}
```

Sum:

```c
while (rray_element_iterator_next(&it)) {
  v_out[it.loc] = rray_sum_dbl_one(v_out[it.loc], v_x[it.index]);
}
```

## Single loop run variants

The `run` variant writes one loop per run, with the strides as runtime values. Broadcast:

```c
while (rray_run_iterator_next(&it)) {
  r_ssize x_loc = it.loc;

  for (r_ssize i = it.start; i < it.end; ++i) {
    v_out[i] = v_x[x_loc];
    x_loc += it.stride;
  }
}
```

Sum:

```c
while (rray_run_iterator_next(&it)) {
  r_ssize out_loc = it.loc;

  for (r_ssize i = it.start; i < it.end; ++i) {
    v_out[out_loc] = rray_sum_dbl_one(v_out[out_loc], v_x[i]);
    out_loc += it.stride;
  }
}
```

## Benchmark script

Run from the root of the variant package. This is the exact script behind the tables. For "Any number of inputs", the `variants` line was changed to `c(macro = 0L, run_split = 3L, run_n = 4L, run_n_runtime = 5L)`, and nothing else.

```r
devtools::load_all(quiet = TRUE)

variants <- c(macro = 0L, element = 1L, run = 2L, run_split = 3L)

set_variant <- function(variant) {
  invisible(.Call(ffi_rray_set_iterator_variant, variant))
}

set.seed(1)

dbl <- function(...) array(runif(prod(c(...))), c(...))
int <- function(...) array(sample(100L, prod(c(...)), TRUE), c(...))

m <- dbl(4000, 4000)
cube <- dbl(200, 200, 200)
wide <- dbl(10, 1e6)

mi <- int(4000, 4000)
wide_i <- int(10, 1e6)

cases <- list(
  broadcast = list(
    "[4000, 1] to [4000, 4000]" = local({
      x <- dbl(4000, 1)
      \() rray_broadcast(x, c(4000L, 4000L))
    }),
    "[1, 4000] to [4000, 4000]" = local({
      x <- dbl(1, 4000)
      \() rray_broadcast(x, c(4000L, 4000L))
    }),
    "[1, 1] to [4000, 4000]" = local({
      x <- dbl(1, 1)
      \() rray_broadcast(x, c(4000L, 4000L))
    }),
    "[200, 1, 200] to [200, 200, 200]" = local({
      x <- dbl(200, 1, 200)
      \() rray_broadcast(x, c(200L, 200L, 200L))
    }),
    "[1, 200, 200] to [200, 200, 200]" = local({
      x <- dbl(1, 200, 200)
      \() rray_broadcast(x, c(200L, 200L, 200L))
    }),
    "[10, 1] to [10, 1e6]" = local({
      x <- dbl(10, 1)
      \() rray_broadcast(x, c(10L, 1000000L))
    }),
    "[1, 1e6] to [10, 1e6]" = local({
      x <- dbl(1, 1e6)
      \() rray_broadcast(x, c(10L, 1000000L))
    })
  ),
  add_dbl = list(
    "[4000, 4000] + [4000, 4000]" = local({
      y <- dbl(4000, 4000)
      \() rray_add(m, y)
    }),
    "[4000, 4000] + [4000, 1]" = local({
      y <- dbl(4000, 1)
      \() rray_add(m, y)
    }),
    "[4000, 4000] + [1, 4000]" = local({
      y <- dbl(1, 4000)
      \() rray_add(m, y)
    }),
    "[4000, 4000] + [1, 1]" = local({
      y <- dbl(1, 1)
      \() rray_add(m, y)
    }),
    "[4000, 1] + [1, 4000]" = local({
      x <- dbl(4000, 1)
      y <- dbl(1, 4000)
      \() rray_add(x, y)
    }),
    "[200, 200, 200] + [1, 200, 1]" = local({
      y <- dbl(1, 200, 1)
      \() rray_add(cube, y)
    }),
    "[10, 1e6] + [10, 1]" = local({
      y <- dbl(10, 1)
      \() rray_add(wide, y)
    }),
    "[10, 1e6] + [1, 1e6]" = local({
      y <- dbl(1, 1e6)
      \() rray_add(wide, y)
    })
  ),
  add_int = list(
    "[4000, 4000] + [4000, 4000]" = local({
      y <- int(4000, 4000)
      \() rray_add(mi, y)
    }),
    "[4000, 4000] + [4000, 1]" = local({
      y <- int(4000, 1)
      \() rray_add(mi, y)
    }),
    "[4000, 4000] + [1, 4000]" = local({
      y <- int(1, 4000)
      \() rray_add(mi, y)
    }),
    "[10, 1e6] + [10, 1]" = local({
      y <- int(10, 1)
      \() rray_add(wide_i, y)
    }),
    "[10, 1e6] + [1, 1e6]" = local({
      y <- int(1, 1e6)
      \() rray_add(wide_i, y)
    })
  ),
  sum = list(
    "[4000, 4000] over 1" = \() rray_sum(m, 1L),
    "[4000, 4000] over 2" = \() rray_sum(m, 2L),
    "[4000, 4000] over 1, 2" = \() rray_sum(m, 1:2),
    "[200, 200, 200] over 1" = \() rray_sum(cube, 1L),
    "[200, 200, 200] over 2" = \() rray_sum(cube, 2L),
    "[200, 200, 200] over 3" = \() rray_sum(cube, 3L),
    "[200, 200, 200] over 1, 3" = \() rray_sum(cube, c(1L, 3L)),
    "[10, 1e6] over 1" = \() rray_sum(wide, 1L),
    "[10, 1e6] over 2" = \() rray_sum(wide, 2L)
  )
)

for (op in names(cases)) {
  for (case in names(cases[[op]])) {
    f <- cases[[op]][[case]]
    set.seed(2)
    set_variant(0L)
    expected <- f()
    for (variant in variants) {
      set.seed(2)
      set_variant(variant)
      stopifnot(identical(f(), expected))
    }
  }
}

results <- list()

for (op in names(cases)) {
  for (case in names(cases[[op]])) {
    f <- cases[[op]][[case]]
    times <- vapply(
      variants,
      function(variant) {
        set_variant(variant)
        median <- bench::mark(f(), iterations = 20, check = FALSE)$median
        as.numeric(median) * 1000
      },
      numeric(1)
    )
    results[[length(results) + 1L]] <- data.frame(
      op = op,
      case = case,
      t(round(times, 2))
    )
  }
}

set_variant(0L)

results <- do.call(rbind, results)
print(results, row.names = FALSE)
```

## Short-run benchmark script

The exact script behind the "Removing the per-run cost" table. Run from the root of the variant package.

```r
devtools::load_all(quiet = TRUE)

variants <- c(macro = 0L, run_split = 3L, run_n = 4L, lean = 6L, advance = 7L)

set_variant <- function(variant) {
  invisible(.Call(ffi_rray_set_iterator_variant, variant))
}

set.seed(1)

dbl <- function(...) array(runif(prod(c(...))), c(...))
int <- function(...) array(sample(100L, prod(c(...)), TRUE), c(...))

wide <- dbl(10, 1e6)
wide_i <- int(10, 1e6)
col <- dbl(10, 1)
row <- dbl(1, 1e6)
col_i <- int(10, 1)
row_i <- int(1, 1e6)

cases <- list(
  "Broadcast [10, 1] to [10, 1e6]" = \() rray_broadcast(col, c(10L, 1000000L)),
  "Broadcast [1, 1e6] to [10, 1e6]" = \() rray_broadcast(row, c(10L, 1000000L)),
  "Add [10, 1e6] + [10, 1]" = \() rray_add(wide, col),
  "Add [10, 1e6] + [1, 1e6]" = \() rray_add(wide, row),
  "Add, integer, [10, 1e6] + [10, 1]" = \() rray_add(wide_i, col_i),
  "Add, integer, [10, 1e6] + [1, 1e6]" = \() rray_add(wide_i, row_i),
  "Sum [10, 1e6] over 1" = \() rray_sum(wide, 1L),
  "Sum [10, 1e6] over 2" = \() rray_sum(wide, 2L)
)

for (case in names(cases)) {
  f <- cases[[case]]
  set_variant(0L)
  expected <- f()
  for (variant in variants) {
    set_variant(variant)
    stopifnot(identical(f(), expected))
  }
}

rounds <- 3L

times <- array(
  NA_real_,
  c(length(cases), length(variants), rounds),
  dimnames = list(names(cases), names(variants), NULL)
)

for (round in seq_len(rounds)) {
  order <- c(seq(round, length(variants)), seq_len(round - 1L))
  for (case in names(cases)) {
    f <- cases[[case]]
    for (variant in names(variants)[order]) {
      set_variant(variants[[variant]])
      median <- bench::mark(f(), iterations = 50, check = FALSE)$median
      times[case, variant, round] <- as.numeric(median) * 1000
    }
  }
}

set_variant(0L)

out <- apply(times, c(1, 2), median)
print(round(out, 2))
```
