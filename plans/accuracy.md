# Floating point accuracy in reductions

rray4 accumulates every floating point reduction in `double`. Base R uses `long double` where the platform has one, so rray4 can differ from base R in the last bits. This document records why we chose `double`, what base R, matrixStats and NumPy do, what we measured, and the plan for getting more accuracy out of `double` in `rray_mean()`, `rray_sum()` and `rray_prod()`.

---

# Decision: accumulate in `double` everywhere

`double` is the only floating point type that behaves the same on every platform R runs on. `long double` means something different on each one:

| Platform | `long double` | Extra precision? |
|---|---|---|
| macOS arm64 | 64-bit, same as `double` | No |
| Windows arm64 | 64-bit, same as `double` | No |
| Windows x86_64, MSVC | 64-bit, same as `double` | No |
| Windows x86_64, Rtools (MinGW GCC) | 80-bit x87 | Yes |
| Linux and macOS x86_64 | 80-bit x87 | Yes |
| Linux arm64 | 128-bit, done in software | Yes, but slow |

So `double` is the minimum that every platform gives us, and two of the platforms we care about most, Apple silicon Macs and Windows on ARM, never get anything more. Targeting that minimum everywhere gives us:

- The same output on every OS. For a fixed order of operations, `double` math gives the same bits on every platform. Compilers never reorder floating point adds unless `-ffast-math` is set.

- One code path, with no `long double` branches to reason about or test.

- No hidden performance cliffs. On x86_64, `long double` math runs on the x87 unit, which can't vectorize. On Linux arm64, every `long double` add is a call into a software library.

- No dependence on where the hardware market goes. Windows is adding ARM as a second platform alongside x86, not replacing x86, and Linux arm64 is growing on servers (AWS Graviton, Azure Cobalt, Google Axion). All of these will matter for years, and each one has a different `long double`.

The cost is that results can differ from base R in the last bits on x86_64 and Linux arm64, where base R gets extra precision. One shared docs section should say this, linked from `rray_sum()`, `rray_prod()` and `rray_mean()`.

This rule is about floating point. 64-bit integer totals (`int64_t`) are exact and behave the same everywhere, so they are allowed. Several of the plans below use them.

One caveat on "same bits everywhere". GCC defaults to `-ffp-contract=fast`, which may fuse `a * b + c` into a single fused multiply-add (FMA) instruction on arm64. That changes rounding. Keep `a * b + c` patterns out of accumulation kernels so this can't happen.

---

# What other implementations do

## Base R

- `sum()` (`rsum()` in `src/main/summary.c`) adds doubles into a `LDOUBLE`, then sends values beyond `±DBL_MAX` to `±Inf` and casts to `double`.

- `prod()`, `mean()` and `cumsum()` / `cumprod()` (`src/main/cum.c`) also accumulate in `LDOUBLE`.

- `LDOUBLE` is `long double` unless R is configured with `--disable-long-double` (`src/include/Defn.h`).

- Integer `sum()` (`isum()`) adds into a 64-bit `LONG_INT` and only checks the final total. A result outside the `int` range gives `NA` with a warning. Very long vectors that might overflow 64 bits switch to `risum()`, which adds in `LDOUBLE`.

- Integer `cumsum()` (`icumsum()`) adds in `double` and checks every running total, since each one is an output.

## matrixStats

- `colSums2()` keeps one local `long double` total per column, reading down the column in memory order.

- `rowSums2()` allocates a `long double` buffer with one slot per row, then walks the matrix in memory order and adds into it. This is the "flat walk plus scratch buffer" shape that this plan proposes.

- `rowMeans2()` finishes one row before starting the next, jumping `nrow` elements per read. It has the same cache problem as rray4's nested reducer.

- `rowCumsums()` / `colCumsums()` add in `long double` but store a `double` at every step, so the extra precision is thrown away each time.

## NumPy

- Sums of `float64` add in `float64`. Accuracy comes from pairwise summation, not a wider type.

- Pairwise summation is only used along a contiguous axis. NumPy splits the run into blocks of 128, sums each block with 8 running totals, and combines the blocks as a tree. The error grows like `log(n)` instead of `n`, and the 8 running totals let the loop vectorize.

- Along any other axis, NumPy walks memory in order and adds each element into its output, one at a time. So accuracy depends on memory layout. rray4's flat `RRAY_REDUCE()` works the same way.

- `cumsum()` is plain one-at-a-time summation.

- `long double` is only used when asked for with `dtype = np.longdouble`.

- Integer sums promote to `int64` and overflow silently wraps around.

- `mean()` is the sum divided by the count, with no correction pass.

---

# Benchmarks

All numbers are medians from `bench::mark()` on Apple silicon (arm64), where `long double` is the same as `double`. Inputs are `runif()` doubles. None of this has been measured on x86_64 or Linux arm64.

These were measured before #124 moved `rray_mean()` to the flat walk, so the `rray_mean()` numbers are out of date.

## Flat against nested

`rray_sum()` used the flat walk (`RRAY_REDUCE()`). `rray_mean()` used the nested walk (`RRAY_REDUCE_OUTER()` / `RRAY_REDUCE_INNER()`). The integer mean makes one pass, so it isolates the cost of the nested walk.

| Input | Axes | Flat double sum | Nested integer mean | Nested double mean (2 passes) |
|---|---|---|---|---|
| 4000 x 4000 | 1 | 13.1 ms | 13.4 ms | 26.6 ms |
| 4000 x 4000 | 2 | 2.3 ms | 40.9 ms | 61.7 ms |
| 200 x 200 x 200 | 3 | 1.7 ms | | 12.8 ms |
| 200 x 200 x 200 | 1, 3 | 3.6 ms | | 13.6 ms |

- The nested walk costs nothing when the reduced axis is axis 1, since the inner loop then reads memory in order.

- It is about 18x slower when the reduced axis isn't axis 1, because every read jumps through memory and misses the cache.

- The double mean pays twice, once for the sum and once for its correction pass.

So moving `rray_sum()` to the nested walk to get a local accumulator is ruled out.

## Against matrixStats and base R, 4000 x 4000

| Reducing over | rray4 | matrixStats | Base R |
|---|---|---|---|
| Rows, sum | 13.1 ms | 13.6 ms `colSums2()` | 13.1 ms `colSums()` |
| Columns, sum | 2.3 ms | 9.1 ms `rowSums2()` | 2.3 ms `rowSums()` |
| Rows, mean | 26.6 ms | 27.0 ms `colMeans2()` | 13.1 ms `colMeans()` |
| Columns, mean | 61.9 ms | 79.2 ms `rowMeans2()` | 2.3 ms `rowMeans()` |
| Rows, mean, one pass | 13.4 ms (integer) | 13.5 ms `colMeans2(refine = FALSE)` | |
| Columns, mean, one pass | 40.9 ms (integer) | 41.2 ms `rowMeans2(refine = FALSE)` | |

`rowSums2()` is 4x slower than rray4 even though its buffer is plain `double` on this machine. The likely cause is the `byrow`, `narm` and missing index branches inside its inner loop, not the buffer.

## A flat sum with a `long double` buffer

A prototype of `rray_sum_dbl()` that walks `x` exactly like `RRAY_REDUCE()`, but adds into an `R_alloc()` buffer of `out_size` `long double` values and converts to `double` at the end:

| Input | Axes | Current | Buffer | Memory (current / buffer) |
|---|---|---|---|---|
| 4000 x 4000 | 1 | 13.06 ms | 13.06 ms | 31 KB / 63 KB |
| 4000 x 4000 | 2 | 2.26 ms | 2.25 ms | 31 KB / 63 KB |
| 4000 x 4000 | 1, 2 | 13.52 ms | 13.52 ms | 0 / 0 |
| 200 x 200 x 200 | 1 | 3.63 ms | 3.65 ms | 313 KB / 625 KB |
| 200 x 200 x 200 | 3 | 1.70 ms | 1.74 ms | 313 KB / 625 KB |
| 200 x 200 x 200 | 1, 3 | 3.61 ms | 3.59 ms | 2 KB / 3 KB |
| 10 x 1e6 | 1 | 3.51 ms | 3.94 ms | 7.6 MB / 15.3 MB |
| 10 x 1e6 | 2 | 3.78 ms | 3.75 ms | 0 / 0 |

On arm64 this measures only the buffer, since the compiled loop is identical. The buffer is close to free, apart from about 10% when the output is large. This matters for the plans below, which all use a buffer of `out_size` elements.

It also shows that summing along axis 1 is about 6x slower than along axis 2. Down a column, every add waits for the one before it. Along axis 2, there are 4000 separate totals and the compiler vectorizes across them.

---

# Plan

Integer and logical sums, and the decision to write reducers as hand written loops instead of a `RRAY_REDUCE_ACC()` macro, are in `plans/sum-accurate.md`.

## 1. `rray_mean()` accumulates in `double`

Done in #124: the double mean adds in `double`, and the integer and logical means add into an `int64_t`, with a fallback past 2^32 elements per output.

One test change is left:

- `"matches mean() over every combination of axes"` and `"a second pass corrects the rounding error of the first sum"` compare against base `mean()` with `expect_identical()`. Now that rray4 uses `double`, they can fail on x86_64, where base R uses `long double`. CI wouldn't notice, since it only runs on macOS arm64. Replace the base R comparison with expected values computed by hand, so the tests don't depend on the platform.

## 2. Double and complex sum

### Pairwise summation along runs

The `out_stride == 0` loop of `rray_sum_dbl()` becomes NumPy's pairwise sum. Split the run into blocks of 128, sum each block with 8 running totals, and combine the blocks as a tree.

- Accuracy: error grows like `log(n)` instead of `n` along the run.

- Speed: the 8 running totals break the chain of adds that each wait on the one before. This should make summing along axis 1 much faster than the current 13 ms, closer to the 2.3 ms of axis 2. That is expected, not measured.

- `na_rm`: replace `NaN` elements with `0` before adding. This keeps the loop free of branches.

- `NA` against `NaN`: which one wins is already implementation defined for `rray_sum()` (see `rray_add_dbl_one()`), so reordering the adds doesn't change any promise.

- Complex: run the same pairwise sum on the real and imaginary parts separately.

When `out_run_stride != 0`, each output gets one element per run, so there is no run to sum pairwise. These outputs keep one-at-a-time summation, as in NumPy.

### Undecided: compensated summation everywhere else

To make accuracy independent of memory layout, each output could keep a struct holding a total and a compensation term, using Neumaier's variant of Kahan summation:

```c
struct rray_sum_dbl_acc {
  double sum;
  double compensation;
};
```

- Each element is added with the compensation update. A run adds its pairwise sum the same way. The result is `sum + compensation`.

- The error then stays at a few units in the last place regardless of how many elements are added, along any axis.

- If `sum` becomes infinite or `NaN`, `compensation` turns into `NaN` (from `Inf - Inf`). The result must be `sum` alone when it isn't finite.

- Costs: the buffer doubles to 16 bytes per output, and each element takes about 4 floating point operations instead of 1. The axis 2 loop is limited by memory speed, so this may be nearly free there. Not measured.

This is an idea to keep in mind, not part of the work yet.

## 3. Product

Multiplication doesn't have the cancellation problem that addition has. Each multiply adds at most half a unit of rounding error, in relative terms, regardless of order. So there is no pairwise or compensated version worth building.

The real gap is range. With `long double` on x86_64, base R can hold intermediate products up to about 10^4932. In `double`, `prod(c(1e300, 1e300, 1e-300))` overflows to `Inf` partway through, though the answer is `1e300`. That already happens in base R on Apple silicon, so base R itself differs by platform here.

Undecided. If we do it, each output keeps a mantissa and a separate exponent in a buffer, and it waits on a benchmark.

```c
struct rray_prod_dbl_acc {
  double mantissa;
  int64_t exponent;
};
```

- Each step multiplies the mantissa by the element's mantissa and adds the exponents, then renormalizes the mantissa into `[0.5, 1)`. The mantissa can then never overflow or underflow, and the exponent is applied once at the end with `ldexp()`.

- Rounding is the same as a plain multiply. Only the range changes.

- Zeros, infinities and `NaN` skip the split and are tracked with flags, so IEEE rules still apply. `0 * Inf` is `NaN`, but `c(1e300, 1e300, 0)` gives `0` instead of today's `NaN`. That matches the math, and base R on x86_64.

- Splitting a double into mantissa and exponent (`frexp()`) can be done with bit operations, without a call into the math library.

- Renormalizing at every step may be too slow. If so, renormalize only when the mantissa leaves a safe range. Prototype both and measure against the current `rray_prod()` before committing.

Integer and logical products already accumulate in `double` and return `double`. They get the same accumulator. Products above 2^53 still round, as they do today. Base R on x86_64 is exact up to 2^64 there, which is not worth chasing.

Complex products stay on `RRAY_REDUCE()` as they are.

## 4. Cumulative functions

The cumulative plan (`plans/cumulative.md` on `feature/cumulative-plan`) is already consistent with this one.

- Every running total is an output, so there is nothing to keep in a wider type. Cumulative sums and products stay plain one-at-a-time `double` math.

- It already says rray4 won't promise bit-for-bit matches with base `cumsum()` for doubles.

- Integer cumulative sums check every running total. `rray_sum()` checks only its final total. That is the same rule, "error when an output doesn't fit in `int`", applied to two different sets of outputs.

---

# Order of work

1. Integer, logical, double and complex sums on hand written loops (`plans/sum-accurate.md`).

2. The `rray_mean()` test change (plan 1).

3. Pairwise double and complex sums (plan 2).

4. One shared docs section saying rray4 accumulates in `double`, linked from `rray_sum()`, `rray_prod()` and `rray_mean()`.

Compensated summation (plan 2) and the product's range (plan 3) are not scheduled. Each would be its own pull request once decided.

---

# Open questions

- Should the double sum use compensated summation outside of runs (plan 2), or accept that accuracy depends on memory layout, as NumPy does?

- Is extending the product's range (plan 3) worth a second piece of state per output, if the benchmark comes back acceptable?

- CI runs only on macOS arm64, where `double` and `long double` agree, so it can't catch platform differences. No CI changes for now. Revisit adding x86_64 and arm64 Linux later.
