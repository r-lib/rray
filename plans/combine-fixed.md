# A fixed input path for `rray_combine()`

Future work. `rray_combine()` copies each input into its slice of the output
with one loop, using the input's stride along the run as a runtime value. When
an input is broadcast along the run, that stride is 0, and the loop reloads the
same value for every element.

Broadcast already splits this case out (`RRAY_BROADCAST_LOOP()` in
`src/broadcast.c`). This document measures doing the same in combine.

---

# The change

In `RRAY_COMBINE_FILL_LOOP()` in `src/combine.c`, load the value once when
`x_stride == 0`:

```c
if (x_stride == 0) {
  const CTYPE x_elt = v_x[x_loc];
  for (r_ssize i = start; i < end; ++i) {
    POKE(out, out_loc, x_elt);
    out_loc += out_stride;
  }
} else {
  for (r_ssize i = start; i < end; ++i) {
    POKE(out, out_loc, v_x[x_loc]);
    out_loc += out_stride;
    x_loc += x_stride;
  }
}
```

The macro gains a `CTYPE` argument: the element type for atomic types, and
`r_obj*` for character and list.

An input is fixed along the run when it is broadcast along the leading axes.
For example, `rray_combine(x, y, .axis = 2)` with `x` of `[1000, 1000]` and `y`
of `[1, 1000]` copies `y` into 1000 runs of 1000 elements, each with stride 0.

A scalar is broadcast along every axis, but every axis also coalesces into one
run, so it's a single run with stride 0. That's one fixed run per call, which is
too little work to show up.

---

# Benchmarks

- Apple M2 Pro, Apple clang 17, `-O2`, through `devtools::load_all()`.

- Median of 200 iterations, then the median of 5 rounds, alternating between the
  two builds. Ranges are the lowest and highest round.

- The variant passes `test-combine.R`, and is `identical()` to `main` on 3000
  random inputs across all seven types.

| Case | Today (ms) | Fixed path (ms) | Ratio |
|---|---|---|---|
| `[1000, 1000]` + `[1000, 1000]` on axis 1 | 1.67 | 1.67 | 1.00 |
| `[1000, 1000]` + `[1000, 1000]` on axis 2 | 1.26 | 1.26 | 1.00 |
| `[10, 1e5]` + `[10, 1e5]` on axis 1 | 1.86 | 1.89 | 1.02 |
| `[1000, 1000]` + `[1, 1]` on axis 2 | 0.62 | 0.61 | 0.98 |
| `[10, 1e5]` + `[1, 1]` on axis 2 | 0.60 | 0.62 | 1.03 |
| `[1000, 1000]` + `[1, 1000]` on axis 2 | 1.35 | 1.15 | 0.86 |
| `[10, 1e5]` + `[1, 1e5]` on axis 2 | 1.48 | 1.33 | 0.90 |
| Integer `[1000, 1000]` + `[1, 1000]` on axis 2 | 0.87 | 0.59 | 0.68 |
| Character `[1000, 100]` + `[1, 100]` on axis 2 | 0.78 | 0.84 | 1.09 |

- Inputs broadcast along the run get 10% to 32% faster. Only half of each output
  comes from the broadcast input, so the gain on that half is larger.

- Inputs that aren't broadcast are unchanged.

- Character is within noise. Its rounds ranged from 0.73 to 0.91 ms today and
  0.76 to 0.95 ms with the fixed path. Each element is an `r_chr_poke()` call,
  which costs far more than the load it saves.

---

# Recommendation

Add the fixed path. It is one extra loop in one macro, and matches what
broadcast already does.
