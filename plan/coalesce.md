# Coalescing iterator axes

## Status

This is a plan for a separate pull request. No production implementation was
kept while writing this document.

The investigation was performed on 2026-09-08 from `feature/runs` at
`3890127fb26b9e84f6df976c813d8eb024715afe`. That branch was PR #41, "POC:
Iterator performance". Its base at the time was `main` at
`44007650b7ee9b6d2f9d184509af698e124456bc`.

Start the implementation pull request from a revision that already contains
PR #41. If PR #41 has merged, start from the latest `main`. Do not recreate the
older per-element iterator or the earlier run cursor design.

## Decision

Implement adjacent-axis coalescing for `rray_iterator` and `rray_iterator2`.
Treat the leading-size-one case as the required outcome. Keep
`rray_point_iterator` unchanged.

The minimum useful version removes leading axes whose dimension is one from
the location-only iterators. A full stride-aware version is preferred because
it follows the same compact algorithm, passed the full test suite in a
prototype, and gives the iterator a proper private computation shape.

Do not reorder axes. Only combine adjacent axes. Adjacent coalescing preserves
the current column-major traversal order. Reordering can change output order,
the first error encountered, and other visible behavior.

## Result in one paragraph

The current fast loop works one first-axis run at a time. When the first axis
has dimension one, each run contains one element, so every element executes
the outer carry loop. Coalescing a leading size-one axis into the next axis
turns those one-element runs into long inner loops again. A temporary
implementation improved the affected arithmetic cases by 2.3 to 3.2 times over
the already optimized PR. Direct broadcast, reduction, and split cases
improved by 3.6 to 5.2 times. The full package test suite passed, and 42 extra
results from mixed arithmetic, broadcast, reduction, and named split cases were
identical between the current and coalesced builds.

## Current design

All relevant code is in `src/iterator.h` and
`src/decl/iterator-decl.h`.

There are three iterator types.

### `rray_point_iterator`

This iterator exposes the current multidimensional point. Its loop body can
read every coordinate through `V_POINT`. `rray_split_names()` uses this to
select the names for each output array.

The current inner-loop length is:

```c
const r_ssize rows = v_point_dimensions[0];
```

This iterator cannot use ordinary axis coalescing because its consumer can see
the coordinates. A computation axis no longer has a one-to-one relationship
with a logical axis after two axes are combined.

### `rray_iterator`

This iterator walks one point space and reports one flat location in another
space. Broadcasting and reduction use it.

Initialization copies the point dimensions and computes a location stride for
each point axis. A location dimension of one produces a zero stride. A missing
trailing location dimension is also treated as one and produces a zero stride.

The loop reads the first point dimension into `rows`. It then advances
`LOCATION` by the first location stride inside the inner loop. The remaining
axes are maintained by the carry loop.

### `rray_iterator2`

This iterator walks one point space and reports two flat locations. Binary
arithmetic and split use it. It follows the same loop shape as
`rray_iterator`, but an axis may only be coalesced when both location mappings
allow it.

### Computation shape

The supplied point dimensions are the logical shape of the operation. The
arrays stored inside an iterator can instead describe a private computation
shape. Consumers of `rray_iterator` and `rray_iterator2` only observe flat
locations and the flat output index. They do not observe the private
coordinates.

Coalescing changes only this private shape:

```text
logical point dimensions:      [1, 1000, 1000]
current computation dimensions:[1, 1000, 1000]
coalesced dimensions:          [1000, 1000]
```

The public dimensions, names, and element order remain unchanged.

## Why a first dimension of one is slow

Consider adding two arrays with common dimensions `[1, 1000, 1000]`.

The current iterator chooses `rows = 1`. It executes one million inner loops,
each containing one element. Between those loops it resets the first-axis
location and runs the carry logic for axes two and three.

After removing the leading unit axis, the private dimensions are
`[1000, 1000]`. The iterator chooses `rows = 1000`. It executes one thousand
inner loops. Each loop has enough work for Clang to keep counters and strides
in registers and vectorize cheap elementwise operations.

For the crossed broadcast below, only the leading unit axis can be combined.
The next boundary cannot be combined because the two operands broadcast along
different axes.

```text
point dimensions: [1, 1000, 1000]
x dimensions:     [1,    1, 1000]
y dimensions:     [1, 1000,    1]
```

That case still improved from 2.205 to 0.720 nanoseconds per element. This is
important evidence: the measured gain does not depend on flattening the whole
operation. Removing the leading unit axis is enough.

## Safe coalescing rule

R arrays are column-major. Axis zero is the fastest moving axis in the C code.
Let the left axis be the faster axis and the right axis be its next adjacent
axis.

For one location mapping, two adjacent axes can be combined when at least one
of these conditions is true:

1. The left dimension is one.
2. The right dimension is one.
3. `right_stride == left_dimension * left_stride`.

For `rray_iterator2`, the rule must hold for both location mappings.

The flat output index needs no extra check. It already advances by one in
column-major point order.

### Resulting dimension and stride

The combined dimension is:

```text
combined_dimension = left_dimension * right_dimension
```

If the left dimension is one, use the right stride as the combined stride for
that location mapping. The left axis never advances, so its stored stride says
nothing about how the combined axis must move.

Otherwise, keep the left stride. This includes the case where the right
dimension is one.

Apply that stride choice separately to both mappings in `rray_iterator2`.

### Broadcast stride cases

Broadcasting uses zero strides. The compatibility rule handles them without a
special broadcast branch.

| Left stride | Right stride | Non-unit dimensions | Can combine? | Reason |
|---:|---:|---|---|---|
| `0` | `0` | any | yes | The mapped location is constant. |
| nonzero | expected contiguous stride | any | yes | The mapping is linear. |
| `0` | nonzero | both non-unit | no | A repeated block is followed by movement. |
| nonzero | `0` | both non-unit | no | Each outer step resets the mapped location. |
| any | any | left is one | yes | The left coordinate never advances. |
| any | any | right is one | yes | The right coordinate never advances. |

For example, point dimensions `[1000, 1000]` and location strides `[0, 1]`
cannot become one linear axis. The first location is repeated for one thousand
points, then the location advances. No single constant stride represents that
pattern.

### Adjacent only

Try to combine axes from fastest to slowest. If one boundary cannot be
combined, keep both computation axes and continue with the next boundary.
Never combine non-adjacent axes across an incompatible boundary.

### No axis reordering

NumPy and PyTorch can reorder axes before coalescing when their iteration
contracts allow it. Do not do that here. The current iterator has a fixed R
column-major order and a flat `INDEX` that writes outputs in that order.

## Prior art

This is a standard tensor-iterator optimization.

NumPy's iterator design creates broadcast strides, uses zero for a broadcast
axis, and combines dimensions when every operand has compatible adjacent
strides. It also states that tracking multidimensional coordinates prevents
coalescing into larger inner loops:

<https://numpy.org/neps/nep-0010-new-iterator-ufunc.html>

PyTorch's `TensorIterator` keeps a computation shape that may differ from the
user-visible shape. It reorders dimensions when allowed and then coalesces
adjacent dimensions to reduce explicit iteration work:

<https://github.com/pytorch/pytorch/blob/main/aten/src/ATen/TensorIterator.h>

`rray4` needs only the adjacent coalescing part. Its arrays are ordinary
column-major R arrays, and its current traversal order should stay fixed.

## Prototype

The temporary prototype added one compatibility helper and one in-place
coalescing helper. It called the coalescing helper after location strides were
computed in `rray_iterator_init()` and `rray_iterator2_init()`.

The prototype did not change `rray_point_iterator`.

### Suggested helper interface

The second stride pointer can be `NULL` for `rray_iterator`.

```c
static inline bool rray__iterator_axes_coalescible(
  int left_dimension,
  r_ssize left_stride,
  int right_dimension,
  r_ssize right_stride
);

static inline int rray__iterator_axes_coalesce(
  int* v_dimensions,
  r_ssize* v_strides1,
  r_ssize* v_strides2,
  int dimensionality
);
```

These helpers belong in `src/iterator.h`. Add their declarations to
`src/decl/iterator-decl.h` so the file can keep its current top-down order.

The repository rules prohibit adding comments to C or R code. Do not copy the
explanatory prose from this document into the implementation.

### Algorithm

The helper can compact the arrays in place with one read index and one write
index.

```text
out_axis = 0

for in_axis from 1 through dimensionality - 1:
  left_dimension = dimensions[out_axis]
  right_dimension = dimensions[in_axis]
  merged_dimension = left_dimension * right_dimension

  merge when:
    merged_dimension fits in the stored dimension type
    and mapping one is compatible
    and mapping two is absent or compatible

  if merging:
    when left_dimension is one:
      strides1[out_axis] = strides1[in_axis]
      strides2[out_axis] = strides2[in_axis], when present
    dimensions[out_axis] = merged_dimension
  otherwise:
    increment out_axis
    copy the input dimension and strides to out_axis

return out_axis + 1
```

The compatibility check is:

```text
left_dimension == 1
or right_dimension == 1
or right_stride == left_dimension * left_stride
```

Update `it->point_dimensionality` with the helper's return value. The existing
`v_point` prefix is already zeroed and remains valid for the compacted axes.
Trailing entries no longer matter.

### Minimal alternative

If review favors the smallest possible change, remove only leading unit axes.
Find the first non-unit dimension, but always leave at least one dimension.
Move that suffix and its one or two stride arrays to the start of the iterator
storage. Reduce `point_dimensionality` by the number removed.

This smaller version captures all gains in the targeted benchmark table below.
Those cases were chosen so no boundary after the leading unit axis was also
compatible.

The full rule remains the preferred implementation because it is standard,
small, and already passed the validation described here.

## Integer width

The iterator currently stores point dimensions as `int`. The product of two
valid R dimensions can exceed `INT_MAX` even when each dimension fits in an
integer.

Do not assign a larger product back into `int`.

The least invasive choice is to calculate the product in `r_ssize` and decline
that merge when it exceeds `INT_MAX`:

```text
merged = (r_ssize) left_dimension * right_dimension
merge only when merged <= INT_MAX
```

This keeps correctness for long arrays but may leave an otherwise compatible
boundary uncombined. That is acceptable. The common performance cases fit
comfortably within `INT_MAX`.

A wider change could store private computation dimensions as `r_ssize`, but it
is not required for this optimization and should not be mixed into the same PR
without a demonstrated need.

## Zero-size and all-unit shapes

The public API permits zero dimensions in valid zero-size arrays.

Required behavior:

- A zero-size iterator must still execute no loop body.
- Never reduce dimensionality to zero. Keep at least one computation axis.
- `[1, 1, 1]` may become `[1]`.
- `[1, 0, 3]` may become `[0, 3]`, or it may remain unchanged. Its size is
  zero either way.
- A dimension product that would exceed `INT_MAX` must remain split even when a
  later zero dimension makes the total array size zero.

A useful no-allocation overflow test shape is `[50000, 50000, 0]`. It has zero
elements, but the first adjacent product exceeds `INT_MAX`.

It is also reasonable to return from the coalescing helper without changing
anything when the iterator size is zero. If that route is used, pass `size` to
the helper or perform the check in each initializer.

## Why `rray_point_iterator` stays unchanged

`RRAY_POINT_ITERATOR_FOR_EACH()` gives its loop body `V_POINT`. The consumer
uses the coordinate at logical axis `j` to select names for that same axis.

After combining `[2, 3]` into `[6]`, a single computation coordinate does not
provide both original coordinates. Recovering them requires division and
modulo, or an axis map plus extra carry state. That adds complexity and can
remove the performance benefit.

Leading unit axes could be handled with an axis map because their coordinate
is always zero. The measured named-split workload was already dominated by
allocation and name construction, and the location-only coalescing prototype
improved it only slightly. Leave point iteration out of scope unless a separate
benchmark shows a worthwhile opportunity.

This matches NumPy's design: requesting coordinates prevents the iterator from
coalescing axes into larger inner loops.

## Measured performance

### Environment

- Apple M2 Pro, 12 cores, 32 GB memory.
- R 4.6.0 alpha, arm64.
- Apple Clang 17.0.0.
- Normal package flags, including `-O2`.
- Current build and prototype build were installed into isolated libraries.
- Two targeted passes were run in opposite build order.
- Each table cell is the median from `bench::mark()`.
- The displayed value is the median of the two pass medians.

### Targeted first-axis-size-one cases

The timing unit is nanoseconds per input or output element, as appropriate.
Speedup is current PR time divided by coalesced time. Values above one favor
coalescing.

| Case | Current PR | Coalesced | Speedup |
|---|---:|---:|---:|
| Add, identity `[1, 1e6]` | 2.187 | 0.721 | 3.03 times |
| Add, crossed broadcast `[1, 1000, 1000]` | 2.205 | 0.720 | 3.06 times |
| Broadcast `[1, 1, 1000]` to `[1, 1000, 1000]` | 2.014 | 0.508 | 3.96 times |
| Sum axis 3 of `[1, 1000, 1000]` | 1.887 | 0.363 | 5.20 times |
| Split axis 3 of `[1, 1000, 1000]` | 2.203 | 0.614 | 3.59 times |

The crossed arithmetic case, broadcast case, reduction case, and split case
cannot combine the two non-unit axes after the leading unit axis is removed.
They isolate the benefit of leading-unit-axis handling.

### Dimensionality sweep

One million-element `rray_add()` calls used a scalar second operand and forced
the first dimension to one.

| Point dimensions | Current PR | Coalesced | Speedup |
|---|---:|---:|---:|
| `[1, 1000000]` | 2.181 | 0.716 | 3.05 times |
| `[1, 100, 100, 100]` | 2.297 | 0.719 | 3.19 times |
| `[1, 10, 10, 10, 10, 100]` | 1.657 | 0.722 | 2.29 times |

The coalesced build returns each case to about 0.72 nanoseconds per element,
which is the normal vector-throughput range on this machine.

### Cases without a short first axis

The wider benchmark grid covered all arithmetic operations, ordinary scalar
and axis broadcasts, direct broadcast, reductions, split, named split, and
inputs from 1 through 1000 elements.

Cases that could not gain a longer first-axis run were effectively unchanged.
Small differences moved in both directions and were consistent with run noise.
Cheap one-dimensional arithmetic remained near 0.7 nanoseconds per element.
Dependency-bound reductions remained near 3.3 nanoseconds per element.

Do not claim that general coalescing makes every operation faster. The clear
result is narrower: it removes the severe first-axis-size-one weakness without
hurting the ordinary cases tested.

## Targeted benchmark code

Save this as a temporary script outside the package. Run it once against an
isolated install of the current code and once against an isolated install of
the implementation. Repeat in the opposite order.

```r
library(rray4)

label <- Sys.getenv("BENCH_LABEL")
output <- Sys.getenv("BENCH_OUTPUT")

time_ns <- function(fn, n, iterations = 20) {
  gc()
  result <- bench::mark(
    fn(),
    iterations = iterations,
    check = FALSE,
    memory = FALSE
  )
  as.numeric(result$median) * 1e9 / n
}

rows <- list()

record <- function(name, fn, n, iterations = 20) {
  rows[[length(rows) + 1L]] <<- data.frame(
    revision = label,
    case = name,
    ns_per_element = time_ns(fn, n, iterations),
    stringsAsFactors = FALSE
  )
}

n <- 1e6

x <- array(seq(1, 2, length.out = n), c(1L, 1e6L))
y <- array(seq(2, 3, length.out = n), c(1L, 1e6L))
record("add_identity", function() rray_add(x, y), n)

x <- array(seq(1, 2, length.out = 1000), c(1L, 1L, 1000L))
y <- array(seq(2, 3, length.out = 1000), c(1L, 1000L, 1L))
record("add_crossed_broadcast", function() rray_add(x, y), n)

x <- array(seq_len(1000), c(1L, 1L, 1000L))
record(
  "broadcast",
  function() rray_broadcast(x, c(1L, 1000L, 1000L)),
  n
)

x <- array(seq(1, 2, length.out = n), c(1L, 1000L, 1000L))
record("sum_axis3", function() rray_sum(x, 3L), n)

x <- array(seq_len(n), c(1L, 1000L, 1000L))
record("split_axis3", function() rray_split(x, 3L), n, iterations = 10)

write.csv(do.call(rbind, rows), output, row.names = FALSE)
```

Use isolated installs so the two builds cannot share a loaded DLL:

```sh
R CMD INSTALL --preclean --library=/tmp/rray4-current /path/to/current
R CMD INSTALL --preclean --library=/tmp/rray4-coalesced /path/to/coalesced

BENCH_LABEL=current \
BENCH_OUTPUT=/tmp/current.csv \
R_LIBS=/tmp/rray4-current \
Rscript /tmp/coalesce-benchmark.R

BENCH_LABEL=coalesced \
BENCH_OUTPUT=/tmp/coalesced.csv \
R_LIBS=/tmp/rray4-coalesced \
Rscript /tmp/coalesce-benchmark.R
```

For a final PR report, use at least five alternating rounds, randomize the case
order identically within each pair, warm each case before timing, and report the
median paired speedup plus its range. The shorter two-pass run above was enough
to establish the size of the opportunity, but the implementation PR should
produce the stronger result.

## Correctness validation already completed

The temporary full-coalescing prototype completed all package tests with:

```sh
NOT_CRAN=true R_LIBS=/tmp/coalesced-library Rscript -e \
  'library(testthat); library(rray4); test_package("rray4", reporter = "summary")'
```

No test failed or skipped.

An extra equivalence script produced 42 result objects with the current build
and the prototype build. The two saved lists were `identical()`. Coverage
included:

- All five arithmetic operations.
- Identity, crossed broadcast, internal unit axes, and six-dimensional
  broadcast shapes.
- Direct broadcast with leading and internal unit axes.
- Sum and product over each individual axis, alternating axes, and all axes.
- Named split over individual and alternating axes.

This validation is evidence for the design. It does not replace focused tests
in the implementation PR.

## Tests required in the implementation PR

Add focused public-API tests beside existing tests. Do not expose a test-only C
entry point for the helper.

### Arithmetic

Add cases to `tests/testthat/test-arithmetic-add.R` for:

- Equal inputs with dimensions `[1, 3, 4]`.
- Crossed inputs `[1, 1, 4]` and `[1, 3, 1]`.
- Internal unit axes such as `[2, 1, 4]` and `[1, 3, 4]`.
- Several leading unit axes.
- A 64-dimensional input containing unit dimensions.

Use `expect_identical()` with an explicit expected array or with the existing
test helper that implements the expected broadcast.

One arithmetic operation is sufficient to exercise the shared iterator macro,
but run the full suite because all arithmetic files instantiate it with
different type combinations and scalar functions.

### Broadcast

Add cases to `tests/testthat/test-broadcast.R` for:

- `[1, 1, 4]` to `[1, 3, 4]`.
- `[1, 1, 4]` to `[1, 3, 4, 2]`.
- A scalar source over several dimensions.
- Internal alternating broadcast axes.
- Zero-size point spaces with leading unit axes.

### Reduction

Add cases to `tests/testthat/test-reduce-sum.R` for a source shaped
`[1, 3, 2, 4]` and reduce:

- Each axis individually.
- Axes one and three.
- Axes two and four.
- All axes.

At least one case should combine reduced axes with zero strides. At least one
must stop at a zero-stride to nonzero-stride boundary.

Run the product tests too. Sum and product share the same reduction iterator
but have different scalar dependencies and types.

### Split

Add cases to `tests/testthat/test-split.R` for a named and unnamed source shaped
`[1, 3, 2, 4]`. Split every axis individually and at least two alternating
axes. Check both values and names.

`rray_split()` exercises both location mappings. Named split also confirms that
leaving `rray_point_iterator` unchanged preserves coordinate-based name
selection.

### Boundary cases

Cover these explicitly:

- All dimensions equal one.
- Maximum supported dimensionality, 64.
- A zero dimension before and after unit dimensions.
- A zero-size shape such as `[50000, 50000, 0]` whose adjacent dimension
  product exceeds `INT_MAX`.
- One-dimensional arrays, which should remain one-dimensional internally.
- A location with fewer dimensions than the point space.

## Implementation order

1. Create a new branch from a revision containing PR #41.
2. Save a clean baseline benchmark in an isolated package library.
3. Add the focused public-API tests.
4. Add helper declarations to `src/decl/iterator-decl.h`.
5. Add the compatibility and in-place coalescing helpers to `src/iterator.h`.
6. Call the helper in `rray_iterator_init()` after location strides are built.
7. Call it in `rray_iterator2_init()` after both location stride arrays are
   built.
8. Leave `rray_point_iterator` and its macro unchanged.
9. Run `clang-format -i src/*.c src/*.h` over every C source and header.
10. Run `air format .`.
11. Run the full tests with `Rscript -e "devtools::test()"`.
12. Run the test suite once more with `NOT_CRAN=true` if the normal command
    skips any cases.
13. Run `Rscript -e "devtools::check()"`.
14. Build isolated current and changed libraries and run at least five paired,
    alternating benchmark rounds.
15. Confirm saved outputs are identical between builds.
16. Review the diff for accidental source comments. The repository prohibits
    new C and R comments.
17. Commit with one sentence, no period and no attribution trailer. A suitable
    message is `Coalesce compatible iterator axes`.
18. Push the branch and open a separate pull request. Include the paired
    benchmark table, compiler and hardware details, test results, and the safe
    stride rule in the PR body.

## Files expected to change

Required:

- `src/iterator.h`
- `src/decl/iterator-decl.h`
- `tests/testthat/test-arithmetic-add.R`
- `tests/testthat/test-broadcast.R`
- `tests/testthat/test-reduce-sum.R`
- `tests/testthat/test-split.R`

Possible only if focused coverage shows a gap:

- `tests/testthat/test-reduce-prod.R`
- Other arithmetic test files

Do not change:

- `src/rlang/`
- Public R APIs
- Public dimension or name behavior
- `rray_point_iterator` in this PR

## Review checklist

Before committing, verify each item.

- The implementation mutates only iterator-owned copies of dimensions and
  strides.
- Both stride mappings approve every `rray_iterator2` merge.
- The stride from the right axis replaces the left stride when the left
  dimension is one.
- The left stride is retained when the right dimension is one.
- Zero-stride and nonzero-stride boundaries are not combined unless a
  dimension is one.
- The merged product is computed in `r_ssize`.
- A product above `INT_MAX` is not stored in an `int` dimension.
- At least one computation axis remains.
- Zero-size arrays execute no loop body.
- Logical traversal order is unchanged.
- The point iterator is unchanged.
- No new C or R comments were added.
- `clang-format` ran over all C files and headers.
- `air format .` ran.
- Full tests and package check pass.
- Benchmarks use isolated builds and alternating order.
- Output equivalence is checked independently from timing.

## Acceptance criteria

The pull request is ready when all of these are true:

1. First-axis-size-one arithmetic returns to about the normal contiguous
   throughput on the benchmark machine.
2. The crossed broadcast case improves by at least two times over the current
   PR baseline.
3. Broadcast, reduction, and split first-axis-size-one cases show clear gains.
4. Ordinary long-first-axis cases have no repeatable regression above 2%.
5. Tiny inputs have no repeatable regression above 5% or 100 nanoseconds per
   call, whichever is larger.
6. Every output-equivalence check passes.
7. The full test suite and `devtools::check()` pass.
8. The PR documents any result that differs materially from this investigation
   instead of hiding it.

## Suggested pull request summary

The future PR can use this outline:

```text
Coalesce adjacent iterator axes into a private computation shape when every
mapped location has compatible strides. This turns a leading dimension of one
from a one-element inner loop into a long vectorizable run without changing
logical traversal order.

The change applies to the one-location and two-location iterators. The point
iterator remains unchanged because its consumers observe logical coordinates.

Include the paired before-and-after table here, followed by test and check
results.
```

Link back to the original performance comparison on PR #41:

<https://github.com/DavisVaughan/rray4/pull/41#issuecomment-5585109147>
