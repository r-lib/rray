# Optimizing `rray_index()`, part 2

## Summary

This branch implements steps 1 through 4 of `plans/index-optimize.md`:

1. Specialize the loop on 1, 2, 3 and 4 coordinate arrays.

2. Walk the first axis as a run, with a literal stride 1 path.

3. Merge (coalesce) adjacent axes across all coordinate arrays.

4. Skip the missing value check when validation saw no `NA`.

It is 50% to 78% faster than `feature/index` on every large case except character arrays, which are 30% faster. Results are bit identical on every case measured.

We also built and measured a different algorithm, one pass per coordinate array. It lost to steps 1-4 on every case with two or more coordinate arrays, and it needs a scratch buffer as large as the result or larger. We recommend steps 1-4.

## The two algorithms

Both algorithms compute, for every point of the result, a flat location into `x`:

```c
location = (i1 - 1) * x_stride1 + (i2 - 1) * x_stride2 + ...
```

Steps 1-4 walk all K coordinate arrays together. Each output element reads one value from each array, sums the location, and reads `x` once. The only state is one position per coordinate array, which lives on the stack.

The per-array idea walks one coordinate array at a time with the 1 operand strided iterator. Each pass adds that array's part of the location into an `r_ssize` buffer the size of the result:

```c
// first array
v_locations[i] = (v_index[loc] - 1) * x_stride;

// middle arrays
v_locations[i] += (v_index[loc] - 1) * x_stride;

// last array, also reading from `x`
v_out[i] = v_x[v_locations[i] + (v_index[loc] - 1) * x_stride];
```

It looked attractive for three reasons. Each array gets its own coalescing, so crossed strides no longer block a merge. It reuses the existing 1 operand fast paths. And it needs no per-count specialization, so it is much less code. With one coordinate array there is no buffer at all.

Missing values were handled the way we planned for this branch. Validation records whether each array holds an `NA`. Arrays without one are visited first with no check. Arrays with one are visited last, and mark a missing location with `-1`.

## How it was measured

Five builds, all installed with `R CMD INSTALL` at `-O2`. `devtools::load_all()` compiles at `-O0`, so it is not usable for timing.

| Column | Build |
|---|---|
| base | `feature/index` at `ab6fd54` |
| plan 1-3 | steps 1-3 rebuilt from the sketches in `plans/index-optimize.md` |
| plan 1-4 | the same, plus step 4 |
| per-array | the per-array algorithm described above |
| this branch | the cleaned up steps 1-4 committed here |

The plan 1-3 and plan 1-4 builds were throwaway prototypes. The original steps 1-3 proof of concept was discarded before this work, so it was rebuilt from the plan to get numbers from the same session.

Every build ran the full harness twice, interleaved as base, plan 1-3, plan 1-4, per-array, this branch, then again. Each cell below is the faster of the two runs. The largest run to run difference for any build on any row was 5.4%. All five builds returned identical results on all 23 cases.

Every large case produces 1048576 output elements. "Small src" means `x` is tiny (10 elements per axis) so that it stays in cache and the loop cost is visible. The harness is at the end of this document.

## Results

All values are ns per output element, except the two small call rows, which are us per call.

| Case | base | plan 1-3 | plan 1-4 | per-array | this branch |
|---|---|---|---|---|---|
| 1d/1 small src | 2.79 | 1.27 | 1.09 | 1.09 | 1.08 |
| 1d/1 large src | 2.97 | 1.37 | 1.37 | 1.36 | 1.37 |
| 2d/2 small src | 4.02 | 1.97 | 1.85 | 2.37 | 1.85 |
| 2d/2 large src | 4.30 | 2.03 | 1.98 | 2.66 | 1.96 |
| 2d/2 flat coords | 4.00 | 1.95 | 1.84 | 2.46 | 1.84 |
| 2d/2 cartesian | 2.87 | 0.82 | 0.72 | 1.15 | 0.71 |
| 3d/3 small src | 5.83 | 2.76 | 2.66 | 3.41 | 2.66 |
| 4d/4 small src | 6.94 | 3.68 | 3.46 | 4.38 | 3.45 |
| 2d/2 coords 10 axes | 4.65 | 1.95 | 1.86 | 2.46 | 1.85 |
| 2d/2 coords 20 axes | 6.21 | 1.96 | 1.84 | 2.45 | 1.86 |
| 2d/2 `[1, N]` | 6.00 | 1.97 | 1.87 | 2.43 | 1.86 |
| 2d/2 `[1, 1, N]` | 8.66 | 1.97 | 1.86 | 2.41 | 1.87 |
| 2d/2 `[1, 1024, 1024]` | 5.99 | 1.96 | 1.86 | 2.41 | 1.86 |
| 2d/2 `[2, N/2]` | 4.88 | 1.96 | 1.85 | 2.37 | 1.86 |
| 2d/2 mixed `[2, N/2]` `[1, N/2]` | 4.58 | 2.39 | 1.96 | 2.48 | 1.99 |
| take first axis, J = 2 | 4.09 | 1.84 | 1.45 | 2.72 | 1.45 |
| take first axis, J = 8 | 3.14 | 1.12 | 0.93 | 1.40 | 0.89 |
| take 3d axis 2 | 4.76 | 1.67 | 1.55 | 2.04 | 1.54 |
| 2d/2 small src, NA in one array | 4.15 | 1.99 | 2.02 | 2.60 | 2.01 |
| 2d/2 small src, double | 4.29 | 2.24 | 2.05 | 2.53 | 2.05 |
| 2d/2 small src, character | 6.38 | 4.74 | 4.50 | 5.08 | 4.49 |
| small call | 1.97 | 1.97 | 1.93 | 1.97 | 1.97 |
| small call 2d | 2.21 | 2.21 | 2.21 | 2.25 | 2.21 |

Relative to base, this branch is:

| Case | change |
|---|---|
| 1d/1 small src | -61% |
| 2d/2 small src | -54% |
| 4d/4 small src | -50% |
| 2d/2 cartesian | -75% |
| 2d/2 coords 20 axes | -70% |
| 2d/2 `[1, 1, N]` | -78% |
| take first axis, J = 8 | -72% |
| take 3d axis 2 | -68% |
| 2d/2 small src, character | -30% |

## Why we recommend steps 1-4

### It is faster

Steps 1-4 beat the per-array algorithm by 19% to 47% on every case with two or more coordinate arrays, and by 12% on character arrays, likely because writing into a character vector costs more than the rest of the loop in both. With one coordinate array they tie, because both reduce to the same loop: walk one array, read `x`, no buffer.

The reason is the buffer. Walking all arrays together reads each coordinate once and writes each output once. The per-array algorithm also writes an 8 byte location per element on every pass but the last, and reads it back on the next pass.

Each extra coordinate array cost the per-array build about 0.95 ns/elt. Two measurements split that cost:

- **Validation is about 0.57 ns/elt of it.** `rray_as_index_array()` alone on a 1-d array measured 0.59 ns/elt at 16384 elements and 0.56 ns/elt at 1048576 elements. Every build pays this, once per coordinate array.

- **The build pass is the other 0.36 ns/elt or so.**

The per-array cost did not depend on whether the buffer fit in cache. Timing each coordinate count across output sizes, with one coordinate array per axis of a small `x`, in an earlier run of the same session:

| Output size | base, K = 1 to 4 | per-array, K = 1 to 4 |
|---|---|---|
| 4096 | 3.33, 4.53, 6.47, 7.67 | 1.67, 3.20, 4.21, 5.19 |
| 16384 | 2.93, 4.12, 6.04, 7.11 | 1.29, 2.66, 3.56, 4.47 |
| 65536 | 2.82, 3.99, 5.82, 6.93 | 1.16, 2.37, 3.28, 4.17 |
| 262144 | 2.77, 3.96, 5.82, 6.90 | 1.09, 2.33, 3.27, 4.18 |
| 1048576 | 2.79, 3.96, 5.77, 6.93 | 1.10, 2.06, 2.99, 4.11 |
| 4194304 | 2.68, 4.01, 5.80, 6.93 | 1.09, 2.38, 3.32, 4.29 |

The step from K to K + 1 is roughly constant from 16384 elements (a 128 KB buffer) up to 4194304 elements (a 32 MB buffer). Memory traffic is not the limit, so processing the result in cache sized chunks would not have rescued it.

### It needs no scratch memory

Steps 1-4 allocate nothing beyond the result. The per-array algorithm allocates 8 bytes per result element whenever there are two or more coordinate arrays, and that buffer is alive at the same time as the result:

| Storage type | Buffer size relative to the result |
|---|---|
| raw | 8x |
| logical, integer | 2x |
| double, character, list | 1x |
| complex | 0.5x |

A 1e8 element result would need 800 MB of scratch on top of the result. That can fail where steps 1-4 would succeed.

### What the per-array algorithm did better

Only code size. It needs no per-count specialization, and only the final pass depends on the storage type. That is not worth a 20% to 45% slowdown and a scratch buffer.

## Findings that change `plans/index-optimize.md`

- **Take shapes do benefit.** The earlier plan said the take-along-axis shapes would not gain from step 3. That is true of coalescing, but the run loop from step 2 still cuts them by 55% to 65%, and step 4 takes a further 7% to 21% off.

- **Step 4 is measured now.** It is worth 5% to 8% on most cases, and 12% to 21% on the Cartesian, mixed and take first axis shapes. With one coordinate array it is worth 15%, bringing that case to 1.08 ns/elt. It does nothing when an `NA` is present, as expected. The earlier plan quoted numpy at 0.94 ns/elt for this case, but that number is from a different session, so treat the gap as approximate.

- **Coalescing still does exactly what the earlier plan said.** `[1, N]`, `[1, 1, N]`, `[1, 1024, 1024]`, 10 axes and 20 axes all cost the same as flat coordinates.

- **Short runs are sensitive to how run strides are read.** Reading each run stride through `rray_strided_iterator_n_plan_run_stride()` on every element was 6% to 7% slower on the mixed and J = 2 take shapes, where every run is 2 elements long. Copying the run strides into a local array before the loop removed the difference.

- **A literal stride 1 path did little for the per-array algorithm.** It was worth about 3%, close to noise. Steps 1-4 keep theirs, as the earlier plan describes, but its share of the step 2 gain was not measured separately.

## What changed in this branch

- **`src/strided-iterator.h`**: `rray_strided_iterator_n_plan()` now merges adjacent axes through a new `rray__strided_iterator_axes_coalescen()`, the N operand sibling of `rray__strided_iterator_axes_coalesce2()`. The plan still points at caller owned strides, so N stays unbounded. The builder takes `r_ssize* v_strides` and merges the axes in place. `RRAY_STRIDED_ITERATOR_NEXT_N()` now starts at axis 1 like the other iterators, since the caller owns the first axis run, and takes the array count as a parameter so it can be a literal.

- **`src/index.c`**: the loop walks the first axis as a run, with a literal stride 1 path. It is specialized on 1 to 4 coordinate arrays, with a general fallback. When validation saw no `NA`, it runs a copy with no missing value check.

- **`rray_as_index_array()`**: takes a `bool* p_any_missing` and reports whether it saw an `NA`, at no extra cost since it already visits every value.

- **Tests**: every coordinate count from 1 to 5, with and without `NA`, against `index_base()`. Coordinates reshaped as `[1, n]`, `[1, 1, n]`, `[2, n/2]` and `[2, 3, 4]` giving the same values. Character and list results from one and two coordinate arrays, with and without `NA`. A missing value in an early coordinate array surviving later arrays.

## Next

- **Validation is now the largest cost per coordinate array.** At 0.57 ns/elt it is over half of the one array case. The loop has two data dependent branches per element that each lead to an error. Checking the range without branching, and only finding the bad value once we know there is one, is the obvious next step.

- **Small call overhead is unchanged** at about 2 us per call. The earlier plan's analysis still applies. The loop is not involved.

## Harness

```r
args <- commandArgs(trailingOnly = TRUE)
lib <- args[[1]]
label <- args[[2]]
library(rray4, lib.loc = lib)

set.seed(1)
n <- 1048576L
N <- n
s10 <- function(size) sample(10L, size, replace = TRUE)

x1_small <- array(1:10, 10L)
x1_large <- array(seq_len(n), n)
x2_small <- array(1:100, c(10L, 10L))
x2_large <- array(seq_len(n), c(1024L, 1024L))
x3_small <- array(1:1000, c(10L, 10L, 10L))
x4_small <- array(1:10000, c(10L, 10L, 10L, 10L))
x2_dbl <- array(as.double(1:100), c(10L, 10L))
x2_chr <- array(as.character(1:100), c(10L, 10L))

sq <- c(1024L, 1024L)
a <- s10(n)
b <- s10(n)
c3 <- s10(n)
d4 <- s10(n)
a_na <- a
a_na[seq(1L, n, by = 100L)] <- NA_integer_

take3_x <- array(seq_len(64L * 10L * 64L), c(64L, 10L, 64L))

cases <- list(
  "1d/1 small src" = list(x1_small, array(a, n)),
  "1d/1 large src" = list(x1_large, array(sample(n), n)),
  "2d/2 small src" = list(x2_small, array(a, sq), array(b, sq)),
  "2d/2 large src" = list(x2_large, array(sample(1024L, n, TRUE), sq), array(sample(1024L, n, TRUE), sq)),
  "2d/2 flat coords" = list(x2_small, array(a, n), array(b, n)),
  "2d/2 cartesian" = list(x2_small, array(s10(1024L), c(1024L, 1L)), array(s10(1024L), c(1L, 1024L))),
  "3d/3 small src" = list(x3_small, array(a, sq), array(b, sq), array(c3, sq)),
  "4d/4 small src" = list(x4_small, array(a, sq), array(b, sq), array(c3, sq), array(d4, sq)),
  "2d/2 coords 10 axes" = list(x2_small, array(a, rep(4L, 10L)), array(b, rep(4L, 10L))),
  "2d/2 coords 20 axes" = list(x2_small, array(a, rep(2L, 20L)), array(b, rep(2L, 20L))),
  "2d/2 [1, N]" = list(x2_small, array(a, c(1L, N)), array(b, c(1L, N))),
  "2d/2 [1, 1, N]" = list(x2_small, array(a, c(1L, 1L, N)), array(b, c(1L, 1L, N))),
  "2d/2 [1, 1024, 1024]" = list(x2_small, array(a, c(1L, sq)), array(b, c(1L, sq))),
  "2d/2 [2, N/2]" = list(x2_small, array(a, c(2L, N / 2L)), array(b, c(2L, N / 2L))),
  "2d/2 mixed [2, N/2] [1, N/2]" = list(x2_small, array(a, c(2L, N / 2L)), array(b[seq_len(N / 2L)], c(1L, N / 2L))),
  "take first axis J = 2" = list(array(1:(10L * (N / 2L)), c(10L, N / 2L)), array(s10(2L), c(2L, 1L)), array(seq_len(N / 2L), c(1L, N / 2L))),
  "take first axis J = 8" = list(array(1:(10L * (N / 8L)), c(10L, N / 8L)), array(s10(8L), c(8L, 1L)), array(seq_len(N / 8L), c(1L, N / 8L))),
  "take 3d axis 2" = list(take3_x, array(seq_len(64L), c(64L, 1L, 1L)), array(s10(n), c(64L, 256L, 64L)), array(seq_len(64L), c(1L, 1L, 64L))),
  "2d/2 small src, NA in 1" = list(x2_small, array(a_na, sq), array(b, sq)),
  "2d/2 small src, double" = list(x2_dbl, array(a, sq), array(b, sq)),
  "2d/2 small src, character" = list(x2_chr, array(a, sq), array(b, sq)),
  "small call" = list(x1_small, array(s10(10L), 10L)),
  "small call 2d" = list(x2_small, array(s10(10L), 10L), array(s10(10L), 10L))
)

rows <- lapply(names(cases), function(name) {
  case <- cases[[name]]
  f <- function() do.call(rray_index, case)
  size <- length(f())
  bm <- bench::mark(f(), min_iterations = 30, max_iterations = 2000, min_time = 1, check = FALSE, filter_gc = FALSE)
  med <- as.numeric(bm$median)
  data.frame(
    case = name,
    value = if (size < 100) med * 1e6 else med * 1e9 / size,
    unit = if (size < 100) "us/call" else "ns/elt"
  )
})
print(do.call(rbind, rows), digits = 3)
```

Run it once per build with `Rscript bench.R <library> <label>`, where each library holds one build installed with `R CMD INSTALL -l <library> .`.
