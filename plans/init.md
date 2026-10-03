# Build run iterators only with `init()`

Today there are two ways to build a `rray_run_iterator`:

- Return it by value: `rray_run_iterator()`, `rray_run_iterator1()` and `rray_run_iterator2()`.

- Fill an existing one through a pointer: `rray_run_iterator_init()`, added for `rray_split()`.

This plan removes the first way, so `init()` is the only way to build one.

---

# Why

The iterator struct is about 34 KB. Most of that is the stride array, which has room for 64 dimensions and 64 inputs.

Building it on the line where you declare it is fast. The compiler writes the fields straight into `it`:

```c
struct rray_run_iterator it = rray_run_iterator1(v_dimensions, dimensionality, v_strides);
```

Assigning it to an iterator that already exists looks almost the same, but copies the whole 34 KB every time:

```c
it = rray_run_iterator1(v_dimensions, dimensionality, v_strides);
```

Nothing warns about this. We only found it in `rray_split()` through a benchmark. There the iterator is rebuilt whenever the dimension changes, and splitting `[1000, 1000]` with `dimensions = rep(c(1, 3), 250)` was 35% slower than `main` (0.87 ms against 0.64 ms). The disassembly showed a 34,344 byte `memcpy()` on each rebuild. Switching to `init()` brought it back to `main`.

With `init()` as the only way to build an iterator, that line can't be written. The only copy left is `it = other_it`, which nobody writes by accident. Taking a pointer also says what the iterator is: a large object that is never passed or returned by value.

---

# The change

## Header

In `src/strided-iterator2.h`:

- Remove `rray_run_iterator()`, `rray_run_iterator1()` and `rray_run_iterator2()`.

- Keep `rray_run_iterator_init()`, for any number of inputs.

- Add `rray_run_iterator_init1()` and `rray_run_iterator_init2()`, which build the `v_v_strides` array and call `init()` with a literal `n`, like the wrappers they replace. Done.

```c
static inline void rray_run_iterator_init1(
  struct rray_run_iterator* it,
  const int* v_dimensions,
  int dimensionality,
  const r_ssize* v_strides
) {
  const r_ssize* v_v_strides[] = {v_strides};
  rray_run_iterator_init(it, v_dimensions, dimensionality, v_v_strides, 1);
}
```

## Call sites

Each one goes from one statement to two:

```c
struct rray_run_iterator it;
rray_run_iterator_init2(&it, v_dimensions, dimensionality, v_x_strides, v_y_strides);
```

| File | Today | After |
|---|---|---|
| `src/binary.h` | `rray_run_iterator2()` | `rray_run_iterator_init2()` |
| `src/reduce.h` | `rray_run_iterator1()` | `rray_run_iterator_init1()` |
| `src/broadcast.c` (2) | `rray_run_iterator1()` | `rray_run_iterator_init1()` |
| `src/combine.c` (2) | `rray_run_iterator2()` | `rray_run_iterator_init2()` |
| `src/index.c` (2) | `rray_run_iterator()` | `rray_run_iterator_init()` |
| `src/permute-axes.c` (2) | `rray_run_iterator1()` | `rray_run_iterator_init1()` |
| `src/split.c` | `rray_run_iterator_init1()` | Done |

`src/reduce-mean.c` and `src/reduce.c` still use the old `src/strided-iterator.h` and aren't touched.

---

# Costs

- Two lines per call site instead of one.

- The iterator exists briefly before it's built. `init()` always comes right after the declaration, and clang's `-Wuninitialized` catches use before initialization in simple cases.

---

# Checking it

- Speed should not change. When `rray_run_iterator()` became a wrapper around `init()`, the code for add, broadcast, combine, compare, permute-axes and sum stayed the same, apart from one equivalent compare instruction (`cmp #16; b.hs` became `cmp #15; b.hi`).

- Confirm that rather than assume it: disassemble every object file that includes `src/strided-iterator2.h` before and after, and compare.

- Run the benchmarks in `bench/` only for files whose code changed in a way that isn't trivially equivalent.

- Run the full test suite.

---

# Scope

One mechanical commit, separate from the split work, so the review is only about the API change.
