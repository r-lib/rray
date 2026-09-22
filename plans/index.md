# Integer coordinate indexing plan

## Recommendation

Add one fully specified coordinate function and one directional one-axis
specialization:

```r
rray_index(x, ...)
rray_index_assign(x, ..., value)

rray_index_axis(x, i, axis)
rray_index_assign_axis(x, i, axis, value)
```

`rray_index()` requires exactly one integer coordinate array for every source
axis. The arrays broadcast to common dimensions and are read pointwise. The
common dimensions are the result dimensions.

`rray_index_axis()` accepts one integer array for the selected source axis and
supplies identity coordinates for every other axis. `i` must broadcast to the
dimensions of `x` outside `axis`. The result has the same dimensionality as
`x`, every unselected axis keeps its source dimension, and only the selected
axis can change.

Every source coordinate is explicit. Different partial indexing behaviors are
represented by shaping full coordinate arrays in different ways.

The complete family is:

| Operation | Read | Assign |
|---|---|---|
| Orthogonal subscripts | `rray_slice()` | `rray_slice_assign()` |
| One-axis subscript | `rray_slice_axis()` | `rray_slice_assign_axis()` |
| One-axis coordinate array | `rray_index_axis()` | `rray_index_assign_axis()` |
| Full coordinate arrays | `rray_index()` | `rray_index_assign()` |
| Flat subscript or point matrix | `rray_extract()` | `rray_extract_assign()` |

Do not add replacement functions. Every `_assign()` function returns a
modified copy of `x`.

## Subscripts and coordinates

A subscript describes a set of positions. It can be negative, logical,
character, missing, or `NULL`. Subscript normalization produces a
one-dimensional sequence of positive or missing locations. Subscripts on
different axes form a Cartesian product.

A coordinate array has a narrower meaning. Each non-missing element is one
positive, one-based position on one source axis. Its dimensions connect that
coordinate to an output point. Coordinates on different source axes broadcast
and pair pointwise.

```r
rray_slice_axis(x, -1L, axis = 2L)
# Complement selection

rray_index_axis(x, array(-1L, rray_dimensions(x)), axis = 2L)
# Error: coordinates must be positive
```

The function name determines the input rules. The shape or class of `i` never
changes a slice into coordinate indexing.

## Full coordinate model

Let:

- `D` be the source dimensions.
- `n` be the dimensionality of `x`.
- `L[a]` be the coordinate array supplied for source axis `a`.
- `G` be the common dimensions of all `L[a]`.

`rray_index()` requires exactly `n` coordinate arrays. At every output point
`g` in `G`, each broadcast coordinate array supplies one component of the
source coordinate:

```text
source[a] = broadcast(L[a])[g]
out[g] = x[source[1], source[2], ..., source[n]]
```

The result dimensions are exactly `G`:

```text
out_dimensions = common_dimensions(L[1], ..., L[n])
```

The output can have lower, equal, or greater dimensionality than `x`. There is
no source-axis placement rule because every output axis belongs to the common
coordinate space.

Singleton dimensions and missing trailing dimensions follow rray's
left-aligned broadcasting rules.

## Coordinate validation

Each argument in `...` and `i` in `rray_index_axis()` must be a bare integer
vector or array. Bare vectors are one-dimensional arrays. Factors and other
classed integer objects are errors.

Every non-missing coordinate must be positive, one based, and no greater than
the dimension of its source axis:

- Positive integers identify one source position.
- Zero and negative integers are errors.
- Missing coordinates produce missing output when reading.
- Missing coordinates are errors when assigning.
- Out-of-bounds coordinates are errors.

Logical, character, and double inputs are errors. Logical values are masks,
character values require name matching, and doubles weaken the strict
coordinate contract. Those inputs belong to slicing or extraction.

Validation happens before allocating or copying the result. Every coordinate
array is validated even when the common result has size zero.

## `rray_index()`

```r
rray_index(x, ...)
rray_index_assign(x, ..., value)
```

### Argument rules

- `x` must be an array or bare vector supported by rray.
- Bare vectors normalize to one-dimensional arrays.
- `...` must contain exactly one coordinate array per source axis.
- Argument position identifies the source axis.
- Coordinate arguments must be unnamed.
- Dynamic splicing is supported.
- All coordinate arrays broadcast to common dimensions.
- The result has the same storage type as `x`.
- The resulting dimensionality must not exceed rray's supported maximum.

```r
coordinates <- list(rows, columns)
rray_index(x, !!!coordinates)
```

Missing arguments, omitted trailing arguments, and `NULL` are errors.

`value` follows `...` in `rray_index_assign()`, so it must be supplied by
name.

### Missing coordinates

If any coordinate at an output point is missing, the result at that point is
missing:

| Type | Missing output |
|---|---|
| logical | `NA` |
| integer | `NA_integer_` |
| double | `NA_real_` |
| complex | `NA_complex_` |
| character | `NA_character_` |
| raw | `as.raw(0)` |
| list | `NULL` |

### Zero dimensions

Zero dimensions follow the existing common-dimension rules:

- Equal zero dimensions are compatible.
- Zero and one combine to zero.
- Zero and a dimension greater than one are incompatible.
- Every coordinate is still validated before returning an empty result.

## `rray_index_axis()`

```r
rray_index_axis(x, i, axis)
rray_index_assign_axis(x, i, axis, value)
```

`rray_index_axis()` supplies coordinates for one source axis while every other
axis receives an implicit identity coordinate. It is the rray form of a
directional take-along-axis operation.

`i` must have the same dimensionality as `x`. Let `D` be the source dimensions
and `I` be the dimensions of `i`. For every unselected axis `a`:

```text
I[a] must be 1 or D[a]
```

The selected dimension `I[axis]` can be any valid dimension `J`. The result
dimensions are:

```text
out_dimensions = D
out_dimensions[axis] = J
```

For source dimensions `(A, B, C)` and `axis = 2`:

```text
i dimensions      = (A or 1, J, C or 1)
result dimensions = (A, J, C)
```

The direction matters. `i` broadcasts to `x` outside the selected axis, but
`x` never expands to dimensions introduced by `i`. If `D[a]` is one and
`I[a]` is greater than one on an unselected axis, the call errors. Users can
explicitly broadcast `x` first or use full coordinate indexing when expansion
is intended.

At an output point `h`:

```text
source[axis] = broadcast(i)[h]
source[a] = h[a] for every unselected axis a
out[h] = x[source]
```

Only the selected axis dimension can change. This invariant distinguishes
`rray_index_axis()` from the unrestricted common dimensions of
`rray_index()`.

### Connection to full coordinate indexing

For `x` dimensions `(A, B, C)`, `axis = 2`, and `i` dimensions `(1, J, C)`, the
axis operation is equivalent in values and dimensions to:

```r
axis1 <- array(seq_len(A), c(A, 1L, 1L))
axis2 <- i
axis3 <- array(seq_len(C), c(1L, 1L, C))

rray_index(x, axis1, axis2, axis3)
```

All three arrays broadcast to `(A, J, C)`. The specialized function does not
need to allocate the identity arrays.

Errors from the public axis function should use the argument names `i` and
`axis`.

## Worked examples

### 1. Full pointwise indexing

```r
x <- matrix(1:12, nrow = 3)

rows <- c(1L, 3L, 2L)
columns <- c(4L, 1L, 3L)

out <- rray_index(x, rows, columns)

out
# [1] 10 3 8

dim(out)
# [1] 3
```

The calculation is:

```text
out[1] = x[rows[1], columns[1]] = x[1, 4] = 10
out[2] = x[rows[2], columns[2]] = x[3, 1] = 3
out[3] = x[rows[3], columns[3]] = x[2, 3] = 8
```

Every source axis is explicit and the result dimensions are the common
coordinate dimensions `(3)`.

### 2. Cartesian indexing through an open mesh

Coordinate arrays always pair through broadcasting. Shape them onto different
output axes to request a Cartesian product:

```r
rows <- array(c(3L, 1L), c(2L, 1L))
columns <- array(c(4L, 2L, 1L), c(1L, 3L))

out <- rray_index(x, rows, columns)

dim(out)
# [1] 2 3
```

The common dimensions are `(2, 3)`. Every row coordinate pairs with every
column coordinate because their singleton dimensions form an open mesh.

Ordinary vectors of lengths two and three do not silently form a Cartesian
product:

```r
rray_index(x, c(1L, 2L), c(1L, 2L, 3L))
# Error: dimensions 2 and 3 cannot broadcast
```

### 3. One-axis coordinate indexing

```r
x <- matrix(
  c(
    10, 20, 30, 40,
    50, 60, 70, 80
  ),
  nrow = 2,
  byrow = TRUE
)

i <- rbind(
  c(4L, 1L),
  c(2L, 3L)
)

out <- rray_index_axis(x, i, axis = 2)

out
#      [,1] [,2]
# [1,]   40   10
# [2,]   60   70
```

The row coordinate is an implicit identity:

```text
out[row, column] = x[row, i[row, column]]
```

Only the selected column dimension can change.

The same operation with full coordinates is:

```r
rows <- array(seq_len(2L), c(2L, 1L))
rray_index(x, rows, i)
```

### 4. Independent source rows through full coordinates

The same `i` can be crossed with every source row by making the row identity
an independent output axis:

```r
rows <- array(seq_len(2L), c(2L, 1L, 1L))
columns <- array(i, c(1L, 2L, 2L))

out <- rray_index(x, rows, columns)

dim(out)
# [1] 2 2 2
```

The first output axis is the source row. The next two axes are the dimensions
of `i`:

```text
out[a, j, k] = x[a, i[j, k]]
```

For `k = 1`:

```r
out[, , 1]
#      [,1] [,2]
# [1,]   40   20
# [2,]   80   60
```

For `k = 2`:

```r
out[, , 2]
#      [,1] [,2]
# [1,]   10   30
# [2,]   50   70
```

Nothing in `rray_index()` decides whether rows align with or cross the column
coordinates. The explicit shapes decide.

### 5. Directional broadcasting

```r
x <- matrix(
  c(
    10, 20, 30, 40,
    50, 60, 70, 80,
    90, 100, 110, 120
  ),
  nrow = 3,
  byrow = TRUE
)

i <- matrix(c(4L, 1L), nrow = 1)

out <- rray_index_axis(x, i, axis = 2)

out
#      [,1] [,2]
# [1,]   40   10
# [2,]   80   50
# [3,]  120   90

dim(out)
# [1] 3 2
```

The one row of `i` broadcasts to the three source rows. An array with
dimensions `(4, 2)` errors because its first dimension cannot broadcast to the
source row dimension three.

If `x` instead had first dimension one, an `i` dimension greater than one
would still error. `rray_index_axis()` does not expand unselected source axes.

### 6. Explicit axis 2 coordinates for axes 1 and 3

Let `x` have dimensions `(A, B, C)` and let row and depth coordinates describe
an output space `(J, B, K)`. Axis 2 is explicit even though it is an identity:

```text
row coordinates dimensions   = (J, B or 1, K)
axis 2 coordinates dimensions = (1, B, 1)
depth coordinates dimensions = (J, B or 1, K)
common dimensions            = (J, B, K)
```

The result is:

```text
out[j, b, k] =
  x[row_coordinates[j, b, k], b, depth_coordinates[j, b, k]]
```

The output arrangement can instead put a shared location space `(J, K)` first
and the independent axis 2 last:

```text
row coordinates dimensions    = (J, K, 1)
axis 2 coordinates dimensions = (1, 1, B)
depth coordinates dimensions  = (J, K, 1)
common dimensions             = (J, K, B)
```

These are not two modes of `rray_index()`. They are two explicit coordinate
layouts.

A concrete example makes the difference visible:

```r
x <- array(1:24, dim = c(2, 3, 4))
rows <- c(2L, 1L)
depths <- c(4L, 2L)

axis1 <- array(rows, c(2L, 1L, 1L))
axis2 <- array(1:3, c(1L, 3L, 1L))
axis3 <- array(depths, c(1L, 1L, 2L))

aligned <- rray_index(x, axis1, axis2, axis3)
dim(aligned)
# [1] 2 3 2
```

```r
axis1 <- array(rows, c(2L, 1L, 1L))
axis2 <- array(1:3, c(1L, 1L, 3L))
axis3 <- array(depths, c(1L, 2L, 1L))

independent <- rray_index(x, axis1, axis2, axis3)
dim(independent)
# [1] 2 2 3
```

Both calls provide every source coordinate. Only the coordinate shapes differ.

### 7. One-dimensional arrays

For one-dimensional `x`, the axis and full forms are directly equivalent:

```r
x <- array(c(10, 20, 30, 40), 4L)
i <- array(c(4L, 1L), 2L)

rray_index_axis(x, i, axis = 1L)
rray_index(x, i)
# Both return array(c(40, 10), 2L)
```

Negative coordinates remain invalid. Ordinary complement selection belongs to
`rray_slice_axis()`.

### 8. Explicit identity indexing

`rray_index()` never infers missing coordinates. To reproduce `x`, provide an
identity coordinate array for every source axis. For a matrix:

```r
rows <- array(seq_len(nrow(x)), c(nrow(x), 1L))
columns <- array(seq_len(ncol(x)), c(1L, ncol(x)))

out <- rray_index(x, rows, columns)
```

`out` has the same values and dimensions as `x`. It has no names because the
general coordinate operation drops all names.

```r
rray_index(x)
# Error: one coordinate array is required for every source axis
```

## Names

### Full coordinate indexing

`rray_index()` drops all names from the result. No output axis necessarily
corresponds to one source axis. Even when a particular call uses identity
coordinates, the general contract does not inspect coordinate values to infer
axis provenance.

Names on coordinate arrays are also dropped. They may label the coordinate
space for a particular call, but combining them would add another inference
rule to the general primitive.

### One-axis coordinate indexing

`rray_index_axis()` preserves source names on every unselected axis and drops
names on the selected axis:

- Every unselected result axis is the same size and position as its source
  axis.
- The selected coordinate can vary across lanes, so one output position may
  refer to different source names in different lanes.
- Names on `i` do not affect the result names.

This is one reason for directional broadcasting. Because `x` cannot expand on
an unselected axis, every preserved name vector remains valid.

Assignment always returns the original dimensions and names of `x` unchanged.

## Assignment

```r
rray_index_assign(x, ..., value)
rray_index_assign_axis(x, i, axis, value)
```

Assignment follows the same coordinate plan as reading:

1. Normalize and validate `x` and every coordinate.
2. Compute the indexing result dimensions.
3. Reject missing target coordinates.
4. Cast `value` losslessly to the type of `x`.
5. Broadcast the cast value to the indexing result dimensions.
6. Copy `x`.
7. Write in column-major result order.

The operation cannot change the type, size, dimensions, or names of `x`.
Repeated source coordinates use deterministic final-write-wins behavior. The
last value visited in column-major result order wins.

An error occurs before copying or writing. Casting and broadcasting `value`
before copying also handles `value` equal to `x`.

`NULL` is not an array and cannot be an assignment value. To assign `NULL` into
a list array, use a list array containing `NULL`.

### Assignment with repeated points

```r
x <- matrix(
  c(
    10, 20, 30, 40,
    50, 60, 70, 80
  ),
  nrow = 2,
  byrow = TRUE
)

rows <- c(1L, 1L, 2L)
columns <- c(2L, 2L, 4L)

out <- rray_index_assign(
  x,
  rows,
  columns,
  value = c(100, 200, 300)
)

out
#      [,1] [,2] [,3] [,4]
# [1,]   10  200   30   40
# [2,]   50   60   70  300
```

The point `(1, 2)` appears twice. The second value wins.

## Connections to the full coordinate primitive

The full coordinate operation is the semantic foundation for the entire
family. These lowerings are reference properties and testing tools. Public
functions can use specialized implementations.

### `rray_slice()`

Normalize one subscript per source axis to positive locations, then shape each
location vector onto its source axis:

```text
axis 1 coordinates: (I, 1, 1)
axis 2 coordinates: (1, J, 1)
axis 3 coordinates: (1, 1, K)
```

For example:

```r
rray_slice(x, i, j, k)
```

has the same values and dimensions as:

```r
rray_index(
  x,
  array(normalize(i), c(I, 1L, 1L)),
  array(normalize(j), c(1L, J, 1L)),
  array(normalize(k), c(1L, 1L, K))
)
```

`rray_slice()` remains separate because it accepts ordinary subscripts,
subsets source names, guarantees one output axis per source axis, and can use
an affine strided path.

### `rray_slice_axis()`

After normalizing a one-axis subscript to `J` positive locations, supply it on
the selected output axis and supply identity coordinates elsewhere. For
`axis = 2` and source dimensions `(A, B, C)`:

```r
rray_index(
  x,
  array(seq_len(A), c(A, 1L, 1L)),
  array(normalize(i), c(1L, J, 1L)),
  array(seq_len(C), c(1L, 1L, C))
)
```

This has the values and dimensions of `rray_slice_axis(x, i, axis = 2)`.
Slicing additionally keeps selected source names.

### `rray_index_axis()`

Broadcast `i` directionally over the unchanged source dimensions and provide
an identity coordinate array for every unselected axis. For `axis = 2`:

```r
rray_index(
  x,
  array(seq_len(A), c(A, 1L, 1L)),
  i,
  array(seq_len(C), c(1L, 1L, C))
)
```

The specialized function preserves names on the identity axes and avoids
allocating them.

### `rray_extract()`

`rray_extract()` accepts flat positions or a coordinate point matrix. Both
forms lower to full coordinate indexing and return a one-dimensional array.

For flat positions, normalize and unravel each position into one source
coordinate per axis. Every coordinate vector has the same one-dimensional
shape, so the full index result is also one dimensional.

```text
extract = full coordinate indexing after column-major unravelling
```

Logical masks first become their selected or missing flat positions. The
implementation keeps a direct flat path rather than allocating all coordinate
vectors.

### Point matrices

A point matrix with one column per source axis already contains full
coordinates:

```r
points <- rbind(
  c(1L, 4L),
  c(3L, 1L),
  c(2L, 3L)
)

rray_extract(x, points)

rray_index(x, points[, 1], points[, 2])
```

These calls have the same values and dimensions. Every column has dimensions
`(P)`, so the result has dimensions `(P)` and row coordinates remain paired.
Numeric point coordinates are validated directly. Character point coordinates
are matched against source axis names before lowering to integer coordinates.

### One-dimensional `take()`

The Array API standard's `take()` accepts a one-dimensional integer input and
changes only the selected axis. After converting its indexing convention, it
has the shape of `rray_slice_axis()` and the full-coordinate lowering shown
above.

The public rray slice remains broader because it accepts ordinary R
subscripts, including negative complements, logical masks, and names.

### Multidimensional NumPy `take()`

NumPy permits `indices` to have dimensions `(J, K)`. Taking from axis 2 of an
array with dimensions `(A, B, C)` returns dimensions `(A, J, K, C)`. This is
also fully specified coordinate indexing:

```r
axis1 <- array(seq_len(A), c(A, 1L, 1L, 1L))
axis2 <- array(indices, c(1L, J, K, 1L))
axis3 <- array(seq_len(C), c(1L, 1L, 1L, C))

rray_index(x, axis1, axis2, axis3)
```

The calculation is:

```text
out[a, j, k, c] = x[a, indices[j, k], c]
```

This operation can increase dimensionality, so it is not
`rray_slice_axis()`. It needs no separate primitive because full coordinate
indexing represents it directly.

### `take_along_axis()`

The Array API standard's `take_along_axis()` supplies one coordinate array for
one source axis while other coordinates are identities. This is the model for
`rray_index_axis()`.

rray uses a directional rule outside `axis`: `i` may broadcast to the source
dimensions, but it cannot expand `x`. This is stricter than taking the full
common dimensions. The restriction guarantees that only the selected axis can
change and that names on every unselected axis remain valid.

## Connection to location-producing functions

Location-producing functions should return bare integer arrays shaped for
`rray_index_axis()`:

```r
i <- rray_locate_max(x, axis = 2L)
rray_index_axis(x, i, axis = 2L)
```

If `x` has dimensions `(A, B, C)`, locating along axis 2 returns dimensions
`(A, 1, C)`. Indexing those coordinates returns dimensions `(A, 1, C)` and one
maximum per lane.

Assignment through the same coordinates updates one position per lane:

```r
rray_index_assign_axis(x, i, axis = 2L, value = 0)
```

If a lane has tied extrema, the locating function chooses according to its tie
rule. Replacing every tied value uses a logical flat mask with
`rray_extract_assign()` instead.

## Implementation design

### Coordinate normalization

Build coordinate validation separately from ordinary subscript normalization.
The normalizer owns:

- Bare integer vector and array validation.
- One-based bounds checks against a specific source axis.
- Missing detection.
- Conversion to zero-based coordinate reads for the iterator.
- Common-dimension and broadcast-stride construction.
- Directional dimension checks for `rray_index_axis()`.

Do not send coordinates through ordinary subscript normalization. Zero removal
and negative complement have no meaning for a point coordinate.

### Full index plan

Build one plan containing:

- Source dimensions and column-major strides.
- One normalized coordinate array per source axis.
- Common result dimensions `G`.
- Broadcast strides for every coordinate array over `G`.
- Whether any coordinate is missing.

The iterator walks `G` in column-major order. At each output point, it reads
one broadcast coordinate per source axis and combines them into a flat source
offset:

```text
offset = sum((coordinate[axis] - 1) * source_stride[axis])
```

Read and assignment use the same source-offset order.

### Axis index plan

The axis shell validates that `i` has the same dimensionality as `x`, checks
directional compatibility outside `axis`, and replaces the selected source
dimension with `dim(i)[axis]`.

The implementation represents unselected identities in the plan rather than
allocating coordinate arrays. One iterator component tracks the source base
from identity axes and another reads the possibly broadcast coordinate from
`i`.

The axis plan can lower to the general internal iterator initially. A
specialized iterator can be added without changing public behavior.

### Typed cores

Read and assignment share one source-location order:

- Atomic cores write through data pointers.
- Character and list cores use the write barrier.
- Missing reads write the storage-specific missing value.
- Assignment rejects missing coordinates before copying `x`.
- Assignment writes in column-major result order.

### Protection review

Before calling any C change done, perform the explicit protection review from
`AGENTS.md`. In particular, protect:

- Normalized `x` before normalizing coordinate arrays.
- Every normalized coordinate array stored across later allocations.
- Common dimensions before allocating the result.
- Result dimensions across result allocation.
- Cast assignment values across broadcasting and copying.

Name the next function that receives each touched `r_obj*` and check whether
that function allocates before it protects or consumes the value.

### Files

Add:

- `R/index.R`
- `src/index.c`
- `src/index.h`
- `src/decl/index-decl.h`
- `R/index-axis.R`
- `src/index-axis.c`
- `src/index-axis.h`
- `src/decl/index-axis-decl.h`
- `tests/testthat/test-index.R`
- `tests/testthat/test-index-axis.R`

The index header declares internal entry points that the axis specialization
can reuse. FFI declarations remain in `src/init.c`.

Keep each `.c` file in top-down order. Include its declaration header last.
Do not add comments to C or R source files.

## Test plan

### Full index validation

- Bare integer vectors and arrays work.
- Doubles, logicals, characters, factors, and other classed inputs error.
- Exactly one coordinate argument is required per source axis.
- Missing, omitted, extra, `NULL`, and named coordinate arguments error.
- Dynamic splicing preserves positional source-axis mapping.
- Zero, negative, and out-of-bounds coordinates error.
- Missing coordinates work for reads and error for assignment.
- Validation still occurs for empty results.

### Full coordinate broadcasting

- Equal dimensions pair pointwise.
- Singleton dimensions broadcast.
- Singleton dimensions on different axes produce Cartesian products.
- Missing trailing dimensions follow rray broadcasting.
- Incompatible dimensions error.
- Zero dimensions combine according to existing rules.
- The result dimensions are exactly the common coordinate dimensions.
- Results can have lower, equal, and greater dimensionality than `x`.

### Axis indexing

- `i` must be a bare integer array with the same dimensionality as `x`.
- On unselected axes, dimensions of one and exact source dimensions work.
- An `i` dimension cannot expand an unselected source dimension of one.
- Only the selected result dimension can differ from `x`.
- Coordinates can vary across every lane.
- Unselected source names are preserved.
- Selected source names and all names on `i` are dropped.
- Axis results equal full indexing with explicit identity arrays.
- One-dimensional `x` needs no identity coordinates.

### Values

- All seven native storage types.
- One through the maximum supported dimensionality.
- Paired coordinates.
- Cartesian coordinates formed through broadcasting.
- Repeated coordinates.
- Missing coordinates.
- Empty indexing results.

Build a small R oracle that walks every output point, constructs its complete
source coordinate, and reads `x` through base R. Compare values and dimensions.

### Names

- Every full coordinate result is unnamed.
- Identity-looking full coordinates do not retain source names.
- Axis indexing keeps every unselected source name vector.
- Axis indexing always drops the selected source names.
- Assignment returns the original names of `x`.

### Assignment

- Lossless casts succeed and lossy casts fail.
- Scalar and array broadcasting to result dimensions.
- Arbitrary recycling fails.
- Repeated targets use final-write-wins order.
- Missing targets fail before copying.
- Empty targets validate and return an unchanged copy.
- `value` equal to `x` works.
- Type, dimensions, and names of `x` are unchanged.
- Reading selected targets agrees with assignment target order.

### Family equivalences

- Compare positive orthogonal slicing with open-mesh full coordinates.
- Compare one-axis slicing with normalized coordinates and explicit identities.
- Compare axis indexing with full coordinates and explicit identities.
- Compare flat extraction with unravelled full coordinates.
- Compare `rray_extract()` point matrices with one-dimensional full
  coordinates.
- Compare multidimensional NumPy-style take with expanded full coordinates.

Names are compared according to the stronger specialized contracts rather than
the unnamed general index result.

### Required checks

After each C change:

1. Run the explicit protection review required by `AGENTS.md`.
2. Run `clang-format -i src/*.c src/*.h`.
3. Run `air format .`.
4. Run focused tests, then all tests.
5. Run `devtools::check()` for the final pull request.

## Delivery order

### 1. Full coordinate reads

Implement `rray_index()` with exact argument counts, strict integer
coordinates, common broadcasting, missing reads, unnamed results, and every
storage type.

Use the loop oracle and worked examples as the first tests.

### 2. Axis reads

Implement `rray_index_axis()` with directional broadcasting, fixed
unselected dimensions, selected-axis name removal, and unselected-axis name
preservation.

### 3. Assignment

Implement both assignment functions with casting, value broadcasting, missing
rejection, copying, and deterministic final-write-wins behavior.

### 4. Share lower-level machinery

Share coordinate validation, source-offset iteration, and typed cores with
flat extraction where doing so leaves each public contract clear. Keep
slicing's affine path and extraction's compact flat iterator.

### 5. Main implementation plan

Update the corresponding section of `plans/implementation.md`. Keep
`plans/slice.md` as the source of truth for ordinary subscripts and flat
extraction, and this file as the source of truth for integer coordinates.

## Research notes

### Python Array API standard

The standard requires full coordinate tuples to contain one integer or integer
array per source axis. The entries broadcast to common dimensions and pair
pointwise. The result dimensions are the common coordinate dimensions. It
deliberately leaves mixed slices and integer arrays unspecified.

This is the direct model for `rray_index()`. rray differs by using one-based,
positive coordinates, left-aligned broadcasting, missing reads, strict bounds
checks, and unnamed results.

The standard's `take()` and `take_along_axis()` provide useful specialized
shape contracts. rray assigns ordinary subscript selection to
`rray_slice_axis()` and directional per-lane coordinates to
`rray_index_axis()`.

Useful sources:

- [Integer array indexing](https://data-apis.org/array-api/2024.12/API_specification/indexing.html#integer-array-indexing)
- [`take()`](https://data-apis.org/array-api/2024.12/API_specification/generated/array_api.take.html)
- [`take_along_axis()`](https://data-apis.org/array-api/2024.12/API_specification/generated/array_api.take_along_axis.html)

### NumPy

NumPy advanced indexing supplies the pointwise broadcast model. NumPy's
conditional placement rules are unnecessary here because `rray_index()` has
no unselected source axes. Every output axis belongs to the common coordinate
space.

NumPy's multidimensional `take()` demonstrates that replacing one source axis
with an arbitrary index-array shape is still a full coordinate gather when
identity coordinates are supplied for the other axes.

- [`numpy.take()`](https://numpy.org/doc/stable/reference/generated/numpy.take.html)
- [`numpy.take_along_axis()`](https://numpy.org/doc/stable/reference/generated/numpy.take_along_axis.html)
- [NumPy advanced indexing](https://numpy.org/doc/stable/user/basics.indexing.html#advanced-indexing)
- [NEP 21](https://numpy.org/neps/nep-0021-advanced-indexing.html)

### Base R

Base R point-matrix indexing is the one-dimensional case of full coordinate
indexing. `rray_extract()` retains this input form. Its point-matrix columns
can be passed directly as the coordinate arguments to `rray_index()` after
validation and name matching.

- [Extract or Replace Parts of an Object](https://stat.ethz.ch/R-manual/R-patched/library/base/html/Extract.html)
