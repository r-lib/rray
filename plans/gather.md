# Vectorized gather plan

## Recommendation

Add a general vectorized indexing family:

```r
rray_gather(x, indices, axes = NULL)
rray_gather_assign(x, indices, axes = NULL, value)
```

`rray_gather()` accepts one coordinate array for each selected source axis. The
coordinate arrays broadcast to common dimensions and are read pointwise. Source
axes that are not selected remain in the result and cross with every point in
the common index space.

Also separate lane slicing from ordinary one-axis slicing:

```r
rray_slice_by_lane(x, i, axis)
rray_slice_assign_by_lane(x, i, axis, value)
```

`rray_slice_axis()` should accept only an ordinary shared subscript.
`rray_slice_by_lane()` should accept only a same-dimensionality integer
coordinate array. This gives both functions one stable input contract,
including for one-dimensional arrays.

This plan builds on `plans/slice.md`. Where the two plans differ, this plan
supersedes these parts of `slice.md`:

- Do not dispatch `rray_slice_axis()` between shared and per-lane modes.
- Do not use the dimensionality of `i` to choose subscript semantics.
- Do not treat a multidimensional shared index as invalid. It belongs to
  `rray_gather()` and can add dimensions to the result.
- Implement per-lane indexing as `rray_slice_by_lane()`.

The complete indexing family becomes:

| Operation | Read | Assign |
|---|---|---|
| Orthogonal slice | `rray_slice()` | `rray_slice_assign()` |
| Shared one-axis slice | `rray_slice_axis()` | `rray_slice_assign_axis()` |
| Vectorized coordinates | `rray_gather()` | `rray_gather_assign()` |
| Per-lane axis slice | `rray_slice_by_lane()` | `rray_slice_assign_by_lane()` |
| Flat positions or point matrix | `rray_extract()` | `rray_extract_assign()` |

Do not add replacement functions. Every `_assign()` function returns a modified
copy of `x`.

## Why gather is a separate operation

An ordinary subscript describes a set of locations on one axis. Subscripts on
different axes form a Cartesian product. Negative integers mean complement,
logical values act as masks, and character values match axis names.

A gather index has a different job. Each element supplies exactly one
coordinate on one source axis. Multiple coordinate arrays are broadcast and
read at the same output point. Zero and negative values cannot mean complement
because a coordinate must identify one position.

Keeping the functions separate makes their accepted inputs reliable:

```r
rray_slice_axis(x, -1L, axis = 1L)
# Complement selection

rray_gather(x, list(-1L), axes = 1L)
# Error: a gather coordinate must be positive
```

This distinction matters most for one-dimensional arrays. A one-dimensional
integer array can no longer be interpreted as either a shared subscript or a
per-lane coordinate depending on context. The function name determines the
rules.

`gather` is preferable to `slice` for the general operation because index
dimensions can replace multiple source axes, add dimensions, or reduce
dimensionality. It does not promise to preserve the shape of `x`.

## The vectorized model

### Coordinate arrays

`indices` is a nonempty list. Each element is a bare integer vector or array.
Each element supplies coordinates for one selected source axis.

```r
rray_gather(
  x,
  indices = list(row_index, column_index),
  axes = c(1L, 2L)
)
```

`axes` must contain unique source axes in strictly increasing order. Its length
must equal `length(indices)`. Requiring increasing axes gives every call one
canonical representation and makes the output placement rule easy to read.

When `axes = NULL`, `indices` must contain one coordinate array per source
axis. They map to `seq_len(rray_dimensionality(x))`.

Bare integer vectors normalize to one-dimensional arrays. Factors and other
classed integer objects are errors.

The first version should accept only integer coordinates. Logical values are
masks, character values require name matching, and doubles weaken the contract
without adding addressing power. Those forms can be considered later as
explicit extensions.

### Coordinate validation

Every non-missing coordinate must be positive, one based, and no greater than
the dimension of its source axis.

- Positive integers identify one position.
- Zero and negative integers are errors.
- Missing coordinates produce missing output when reading.
- Missing coordinates are errors when assigning.
- Out-of-bounds coordinates are errors.
- Validation happens before allocating or copying the result.

Every coordinate is validated even when another result axis has dimension
zero. This keeps invalid input from becoming valid only because the result is
empty.

### Broadcasting

All coordinate arrays broadcast to common dimensions using rray's existing
left-aligned broadcasting rules:

- Matching dimensions are compatible.
- A dimension of one can expand.
- Missing trailing axes act as dimensions of one.
- Any other combination is an error.

Call the common index dimensions `G`.

There is no separate paired mode or Cartesian mode. Both follow from the same
rule:

> Broadcast every coordinate array to `G`, then read all coordinates at the
> same point in `G`.

Equal index dimensions produce paired coordinates. Singleton dimensions on
different axes produce a Cartesian product.

```r
rows <- array(c(1L, 3L), c(2L, 1L))
columns <- array(c(4L, 2L, 1L), c(1L, 3L))

rray_dimensions_common(rows, columns)
# c(2L, 3L)
```

Ordinary vectors of sizes two and three do not silently form a Cartesian
product. Their dimensions are `2L` and `3L`, which cannot broadcast.

```r
rray_gather(x, list(c(1L, 2L), c(1L, 2L, 3L)))
# Error: dimensions 2 and 3 cannot broadcast
```

The user must shape them onto different axes to request a Cartesian product.

### Untouched source axes

A source axis not listed in `axes` survives unchanged. Its positions cross
with every point in `G`.

For a matrix, indexing only the second axis with a two-dimensional coordinate
array gives:

```text
out[a, j, k] = x[a, i[j, k]]
```

The source row `a` is independent of the point `(j, k)`. If `x` has two rows
and `i` has four points, the result has eight values.

Supplying an identity row coordinate changes the row axis from untouched to
indexed:

```text
out[j, k] = x[rows[j, k], i[j, k]]
```

The row coordinate is now paired with `i` instead of crossing with it. This is
the basis of lane indexing.

Index dimensions never implicitly correspond to source axes just because
their dimensions happen to match. `axes` says which source coordinate each
index controls. An explicit identity coordinate establishes a connection
between a source axis and an index dimension.

## Result dimensions

Let:

- `D` be the source dimensions.
- `S` be the selected source axes.
- `p` be the first selected source axis.
- `G` be the common coordinate dimensions.
- `U_before` be the unselected source axes before `p`.
- `U_after` be the unselected source axes after `p`.

The result dimensions are:

```text
D[U_before] + G + D[U_after]
```

In words, remove every selected source axis and insert the common index
dimensions where the first selected axis occurred. All unselected source axes
keep their relative order.

Examples:

```text
D = (A, B, C)
S = (2)
G = (J, K)
out = (A, J, K, C)
```

```text
D = (A, B, C, D, E)
S = (2, 4)
G = (J, K)
out = (A, J, K, C, E)
```

```text
D = (A, B, C)
S = (1, 2, 3)
G = (J, K)
out = (J, K)
```

This is one rule for adjacent and nonadjacent selected axes. Do not copy
NumPy's legacy rule where vectorized dimensions move depending on whether
advanced indices are separated by basic slices.

### General read calculation

An output point consists of three pieces:

```text
o = (u_before, g, u_after)
```

Construct one source coordinate `s[a]` for every source axis `a`:

```text
if a is selected:
  s[a] = broadcast(indices[[j]])[g]
else:
  s[a] = the matching coordinate from u_before or u_after
```

The result is:

```text
out[o] = x[s]
```

For column-major storage, the zero-based source location is:

```text
location = sum((s[a] - 1) * source_stride[a])
```

The coordinate calculation is independent of storage order. Storage order
only determines the flat location and the order in which output points are
visited.

## Fully worked examples

### 1. Paired point indexing

```r
x <- matrix(1:12, nrow = 3)

x
#      [,1] [,2] [,3] [,4]
# [1,]    1    4    7   10
# [2,]    2    5    8   11
# [3,]    3    6    9   12

rows <- c(1L, 3L, 2L)
columns <- c(4L, 1L, 3L)

out <- rray_gather(x, list(rows, columns))
```

Both coordinate arrays have dimensions `3L`, so:

```text
G = (3)
S = (1, 2)
out dimensions = (3)
```

The general calculation is:

```text
out[p] = x[rows[p], columns[p]]
```

The complete calculation is:

```text
out[1] = x[rows[1], columns[1]] = x[1, 4] = 10
out[2] = x[rows[2], columns[2]] = x[3, 1] = 3
out[3] = x[rows[3], columns[3]] = x[2, 3] = 8
```

```r
out
# [1] 10 3 8

dim(out)
# [1] 3
```

Pairing is not a special dispatch rule. It is the result of pointwise reading
after two arrays with the same dimensions broadcast to those dimensions.

### 2. Cartesian product through broadcasting

```r
rows <- array(c(3L, 1L), c(2L, 1L))
columns <- array(c(4L, 2L, 1L), c(1L, 3L))

dim(rows)
# [1] 2 1

dim(columns)
# [1] 1 3

out <- rray_gather(x, list(rows, columns))
```

The coordinate arrays broadcast to `c(2L, 3L)`:

```text
rows after broadcasting:
     [,1] [,2] [,3]
[1,]    3    3    3
[2,]    1    1    1

columns after broadcasting:
     [,1] [,2] [,3]
[1,]    4    2    1
[2,]    4    2    1
```

The general calculation is:

```text
out[j, k] = x[rows[j, k], columns[j, k]]
```

The complete coordinate grid is:

```text
(3, 4)  (3, 2)  (3, 1)
(1, 4)  (1, 2)  (1, 1)
```

The complete value calculation is:

```text
x[3, 4]  x[3, 2]  x[3, 1]     12   6   3
x[1, 4]  x[1, 2]  x[1, 1]  =  10   4   1
```

```r
out
#      [,1] [,2] [,3]
# [1,]   12    6    3
# [2,]   10    4    1

dim(out)
# [1] 2 3
```

This is how positive orthogonal slice locations can lower to the gather
engine. Each normalized axis subscript is reshaped onto its own index axis.

### 3. Vectorized indexing with an untouched axis

```r
x <- array(1:24, c(2, 3, 4))

second_axis <- array(c(1L, 3L), c(2L, 1L))
third_axis <- array(c(4L, 2L), c(1L, 2L))

out <- rray_gather(
  x,
  indices = list(second_axis, third_axis),
  axes = c(2L, 3L)
)
```

The coordinate arrays broadcast to `G = c(2L, 2L)`. Source axis 1 is
untouched and occurs before the first selected axis:

```text
D = (2, 3, 4)
S = (2, 3)
G = (2, 2)
out dimensions = (2, 2, 2)
```

The general calculation is:

```text
out[a, j, k] = x[a, second_axis[j, k], third_axis[j, k]]
```

For `k = 1`, the selected coordinates are `(1, 4)` and `(3, 4)`:

```text
out[1, 1, 1] = x[1, 1, 4] = 19
out[2, 1, 1] = x[2, 1, 4] = 20
out[1, 2, 1] = x[1, 3, 4] = 23
out[2, 2, 1] = x[2, 3, 4] = 24
```

```r
out[, , 1]
#      [,1] [,2]
# [1,]   19   23
# [2,]   20   24
```

For `k = 2`, the selected coordinates are `(1, 2)` and `(3, 2)`:

```text
out[1, 1, 2] = x[1, 1, 2] = 7
out[2, 1, 2] = x[2, 1, 2] = 8
out[1, 2, 2] = x[1, 3, 2] = 11
out[2, 2, 2] = x[2, 3, 2] = 12
```

```r
out[, , 2]
#      [,1] [,2]
# [1,]    7   11
# [2,]    8   12
```

The untouched first axis crosses with both selected coordinate pairs. It is
not paired with the first dimension of either index merely because both happen
to have dimension two.

### 4. A multidimensional shared index

```r
x <- matrix(
  c(
    10, 20, 30, 40,
    50, 60, 70, 80
  ),
  nrow = 2,
  byrow = TRUE
)

x
#      [,1] [,2] [,3] [,4]
# [1,]   10   20   30   40
# [2,]   50   60   70   80

i <- matrix(
  c(
    4L, 2L,
    1L, 3L
  ),
  nrow = 2,
  byrow = TRUE
)

i
#      [,1] [,2]
# [1,]    4    2
# [2,]    1    3

shared <- rray_gather(x, list(i), axes = 2L)
```

Only source axis 2 is indexed. Source axis 1 remains untouched:

```text
D = (2, 4)
S = (2)
G = (2, 2)
shared dimensions = (2, 2, 2)
```

The general calculation is:

```text
shared[a, j, k] = x[a, i[j, k]]
```

For `k = 1`:

```text
shared[1, 1, 1] = x[1, i[1, 1]] = x[1, 4] = 40
shared[2, 1, 1] = x[2, i[1, 1]] = x[2, 4] = 80
shared[1, 2, 1] = x[1, i[2, 1]] = x[1, 1] = 10
shared[2, 2, 1] = x[2, i[2, 1]] = x[2, 1] = 50
```

```r
shared[, , 1]
#      [,1] [,2]
# [1,]   40   10
# [2,]   80   50
```

For `k = 2`:

```text
shared[1, 1, 2] = x[1, i[1, 2]] = x[1, 2] = 20
shared[2, 1, 2] = x[2, i[1, 2]] = x[2, 2] = 60
shared[1, 2, 2] = x[1, i[2, 2]] = x[1, 3] = 30
shared[2, 2, 2] = x[2, i[2, 2]] = x[2, 3] = 70
```

```r
shared[, , 2]
#      [,1] [,2]
# [1,]   20   30
# [2,]   60   70
```

Each point in `i` selects an entire column because source axis 1 is untouched.
This is equivalent to a multidimensional shared take along axis 2.

### 5. The same index used by lane

Lane indexing supplies an identity coordinate for every non-selected source
axis. For the matrix above, the row coordinate is:

```r
rows <- array(1:2, c(2L, 1L))

rows
#      [,1]
# [1,]    1
# [2,]    2
```

The conceptual gather is:

```r
lane <- rray_gather(x, list(rows, i))
```

Both source axes are selected. `rows` broadcasts from `c(2L, 1L)` and `i`
already has dimensions `c(2L, 2L)`:

```text
rows after broadcasting:
     [,1] [,2]
[1,]    1    1
[2,]    2    2

G = (2, 2)
S = (1, 2)
lane dimensions = (2, 2)
```

The general gather calculation is:

```text
lane[j, k] = x[rows[j, k], i[j, k]]
```

Because `rows[j, k] = j`, the lane calculation is:

```text
lane[j, k] = x[j, i[j, k]]
```

The complete calculation is:

```text
lane[1, 1] = x[1, i[1, 1]] = x[1, 4] = 40
lane[2, 1] = x[2, i[2, 1]] = x[2, 1] = 50
lane[1, 2] = x[1, i[1, 2]] = x[1, 2] = 20
lane[2, 2] = x[2, i[2, 2]] = x[2, 3] = 70
```

```r
lane
#      [,1] [,2]
# [1,]   40   20
# [2,]   50   70
```

The public lane call is:

```r
rray_slice_by_lane(x, i, axis = 2L)
```

It should not allocate `rows`. The identity coordinate is part of the lane
plan.

The difference between the shared and lane results is structural:

- Shared gather leaves rows untouched, so rows cross with every point in `i`.
- Lane indexing supplies row coordinates, so rows pair pointwise with `i`.

### 6. Lane broadcasting

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

dim(x)
# [1] 3 4

dim(i)
# [1] 1 2

out <- rray_slice_by_lane(x, i, axis = 2L)
```

The row dimension of `i` broadcasts from one to three. The broadcast index is:

```text
i* =
     [,1] [,2]
[1,]    4    1
[2,]    4    1
[3,]    4    1
```

The general calculation is:

```text
out[row, column] = x[row, i*[row, column]]
```

The complete calculation is:

```text
out[1, 1] = x[1, 4] = 40
out[2, 1] = x[2, 4] = 80
out[3, 1] = x[3, 4] = 120
out[1, 2] = x[1, 1] = 10
out[2, 2] = x[2, 1] = 50
out[3, 2] = x[3, 1] = 90
```

```r
out
#      [,1] [,2]
# [1,]   40   10
# [2,]   80   50
# [3,]  120   90

dim(out)
# [1] 3 2
```

### 7. One-dimensional lane behavior

```r
x <- array(c(10, 20, 30, 40), 4L)
i <- array(c(4L, 1L), 2L)

out <- rray_slice_by_lane(x, i, axis = 1L)
```

There are no non-selected axes, so lane indexing is a full gather:

```text
out[p] = x[i[p]]
```

```text
out[1] = x[4] = 40
out[2] = x[1] = 10
```

```r
out
# [1] 40 10

dim(out)
# [1] 2
```

Negative coordinates always error:

```r
rray_slice_by_lane(x, -1L, axis = 1L)
# Error: lane coordinates must be positive
```

Ordinary complement selection remains available through the shared subscript
function:

```r
rray_slice_axis(x, -1L, axis = 1L)
# [1] 20 30 40
# dimensions: 3L
```

This is why lane indexing must not dispatch from the shape of `i` inside
`rray_slice_axis()`.

### 8. Assignment with repeated coordinates

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
value <- c(100, 200, 300)

out <- rray_gather_assign(
  x,
  indices = list(rows, columns),
  value = value
)
```

The gather target dimensions are `3L`. Writes occur in column-major target
order:

```text
target 1: x[1, 2] <- 100
target 2: x[1, 2] <- 200
target 3: x[2, 4] <- 300
```

The second write to `x[1, 2]` wins:

```r
out
#      [,1] [,2] [,3] [,4]
# [1,]   10  200   30   40
# [2,]   50   60   70  300
```

The returned array keeps the dimensions and names of `x`.

## `rray_gather()`

```r
rray_gather(x, indices, axes = NULL)
```

### Argument rules

- `x` is normalized with `arg_as_array()`.
- `indices` must be a nonempty list.
- Every index must be a bare integer vector or array.
- `axes = NULL` requires exactly one index per source axis.
- Otherwise, `axes` must be a strictly increasing integer vector with one axis
  per index.
- Every axis must be valid for `x`.
- Every coordinate array participates in common broadcasting.

Do not accept missing list elements, `NULL`, logical masks, negative
complements, character matching, or arbitrary classed objects. Those belong to
other indexing functions.

### Missing values

If any selected coordinate is missing at a point in `G`, every output value at
that gather point is missing across the untouched source axes.

Use the same storage-specific missing values as `slice.md`:

| Type | Missing output |
|---|---|
| logical | `NA` |
| integer | `NA_integer_` |
| double | `NA_real_` |
| complex | `NA_complex_` |
| character | `NA_character_` |
| raw | `as.raw(0)` |
| list | `NULL` |

### Names

Selected source axes do not survive as output axes, so their names are dropped.
Unselected source axes keep their names and carry them to their new positions.

The axes of `G` take names from the coordinate arrays using
`rray_broadcast_names_common()`. An index whose dimension expands cannot supply
names for that result axis. Coordinate values never become result names.

When all source axes are selected and no coordinate array has names, the result
has no names.

### Zero dimensions

Coordinate arrays can broadcast to dimensions containing zero. The result is
empty even when untouched source axes are nonempty. Every coordinate array
must still pass type, dimensionality, and bounds validation.

## `rray_gather_assign()`

```r
rray_gather_assign(x, indices, axes = NULL, value)
```

Assignment follows the same selection plan as reading:

1. Normalize and validate `x`, `indices`, and `axes`.
2. Compute `G` and the gather result dimensions.
3. Validate every coordinate and reject missing values.
4. Cast `value` losslessly to the type of `x`.
5. Broadcast the cast value to the gather result dimensions.
6. Copy `x`.
7. Write in column-major gather result order.

Repeated source locations use deterministic last-write-wins behavior. The
operation never changes the type, dimensions, or names of `x`.

An error must occur before `x` is copied or any writes happen.

## `rray_slice_by_lane()`

```r
rray_slice_by_lane(x, i, axis)
rray_slice_assign_by_lane(x, i, axis, value)
```

Lane indexing is a constrained full gather. It preserves the dimensionality of
`x` and changes only the selected axis dimension.

### Input contract

- `i` must be a bare integer array.
- `i` must have the same dimensionality as `x`.
- On `axis`, the dimension of `i` becomes the result dimension.
- On every other axis, the dimension of `i` must be one or equal the matching
  dimension of `x`.
- A dimension of one in `i` broadcasts on a non-selected axis.
- `i` can never enlarge a non-selected axis, including an axis of `x` whose
  dimension is one.
- Coordinates must be positive and in bounds for the selected source axis.
- Missing coordinates produce missing values when reading and are errors when
  assigning.

The result dimensions are:

```text
out_dimensions = dimensions(x)
out_dimensions[axis] = dimensions(i)[axis]
```

### General lane calculation

Let `i*` be `i` directionally broadcast to the result dimensions. For an
output point `p`:

```text
source = p
source[axis] = i*[p]
out[p] = x[source]
```

Equivalently:

```text
out[p1, ..., pn] =
  x[p1, ..., p(axis - 1), i*[p1, ..., pn], p(axis + 1), ..., pn]
```

Every non-selected source coordinate equals its output coordinate. Only the
selected coordinate comes from `i`.

### Connection to gather

Conceptually, create one coordinate array per source axis:

```text
coordinates[axis] = i
```

For every other axis `a`, create an identity coordinate with the shape:

```text
(1, ..., dimension(x)[a], ..., 1)
```

Then gather every source axis:

```r
rray_gather(x, coordinates)
```

The identity coordinates force the common index dimensions to retain every
non-selected dimension of `x`. `i` supplies the selected dimension. The
implementation must represent these identity coordinates in the plan and must
not allocate them as R arrays.

### Lane names

The selected axis loses its names because different lanes can select different
source names. Every non-selected axis keeps the names from `x` because its
dimension and identity mapping are unchanged. Names on `i` do not supply
result names.

Assignment returns the original dimensions and names of `x`.

## Relationship to the other indexing forms

### Orthogonal slicing

After ordinary subscripts are normalized to positive locations, orthogonal
slicing is a full gather whose coordinate arrays form an open mesh.

For three axes:

```text
i shape: (I, 1, 1)
j shape: (1, J, 1)
k shape: (1, 1, K)
```

They broadcast to `(I, J, K)`, which is the Cartesian product.

This is an implementation relationship, not a reason to merge the public
functions. `rray_slice()` accepts richer subscript forms, has a fixed
dimension-preserving contract, carries selected names, and can use an affine
strided fast path.

### Point matrices

A point matrix with `P` rows and one column per source axis is equivalent to a
full gather over its columns:

```r
points <- rbind(
  c(1L, 4L),
  c(3L, 1L),
  c(2L, 3L)
)

rray_extract(x, points)

rray_gather(
  x,
  list(points[, 1], points[, 2])
)
```

Both return a one-dimensional result of size `P`. `rray_extract()` keeps the
matrix representation because it is the ordinary R representation of point
coordinates and because it also owns flat extraction.

### Flat positions

Flat extraction remains separate. A flat position can be converted to a full
coordinate tuple, but doing so expands one compact location into one coordinate
per source axis. There is no benefit in lowering the flat path through gather.

### A multidimensional shared take

A gather over one source axis applies the same coordinate array to every
combination of untouched source axes:

```r
rray_gather(x, list(i), axes = axis)
```

If `i` is one dimensional, this has the same values as a positive integer
shared slice. If `i` has multiple dimensions, all of its dimensions replace
the selected source axis. This is not an alias for `rray_slice_axis()` because
the result dimensionality can change.

Do not add `rray_take()` initially. The one-axis gather call is explicit and
keeps the public family smaller. A convenience wrapper can be considered after
real use shows that it is common.

## Implementation design

### Shared coordinate normalization

Build gather coordinate validation on the point-coordinate layer from
`slice.md`, not on ordinary subscript normalization.

The gather normalizer owns:

- Bare integer vector and array validation.
- One-based bounds checks against a specific source axis.
- Missing detection.
- Conversion to zero-based coordinate reads for the iterator.
- Common dimension and broadcast-stride construction.

Do not send gather coordinates through `vec_as_location()` semantics. In
particular, zero removal and negative complement have no meaning here.

### Gather plan

Build one plan from source dimensions, selected axes, coordinate arrays, and
their common dimensions. The plan contains:

- Source dimensions and column-major strides.
- Selected source axes.
- Common index dimensions `G`.
- Gather result dimensions.
- The output position where `G` begins.
- Broadcast strides for every coordinate array.
- A mapping from every unselected source axis to its output axis.
- Whether any coordinate is missing.

Start with a correctness-first indexed path:

1. Walk `G` in column-major order.
2. At each point, read one coordinate from every broadcast index array.
3. Validate the coordinates and compute their combined selected-axis source
   offset.
4. Store one `r_ssize` offset per point in `G`. Use a sentinel for a missing
   coordinate.
5. Walk the full output dimensions. Add the stored gather offset to the source
   offset contributed by untouched axes.

Precomputing gather offsets avoids rereading every coordinate array for each
combination of untouched axes. It also gives read and assignment exactly the
same target order.

If the offset storage becomes material for full gathers, add an on-the-fly
path later. Do not complicate the first implementation before profiling shows
the need.

### Lane plan

The lane wrapper should reuse gather coordinate validation and typed cores, but
it should build a specialized plan:

1. Validate equal dimensionality.
2. Validate directional broadcasting on non-selected axes.
3. Build the result dimensions by replacing only the selected dimension.
4. Give `x` its ordinary stride on non-selected axes and stride zero on the
   selected axis.
5. Give `i` its broadcast stride on every result axis.
6. At each result point, add `(i_location - 1) * x_axis_stride` to the source
   base location.

This is the two-location iterator described in `slice.md`. It is the efficient
specialization of full gather with implicit identity coordinates.

### Typed cores

Gather read and assignment should share one source-location order.

- Atomic cores write through data pointers.
- Character and list cores use the write barrier.
- Missing reads write the storage-specific missing value.
- Assignment rejects missing coordinates before copying `x`.
- Assignment writes in column-major gather result order.

The generic gather and lane plans can call the same typed copy helpers once
they expose the next source location through a small common interface.

### Protection review

Before calling any C change done, perform the explicit protection review from
`AGENTS.md`. In particular, protect:

- Normalized `x` before normalizing any index.
- Every normalized index stored across later allocations.
- Common dimensions before building result dimensions.
- Result dimensions and names across result allocation.
- Cast assignment values across broadcasting and copying.

Name the next function that receives each touched `r_obj*` and check whether
that function allocates before it protects or consumes the value.

### Files

Add:

- `R/gather.R`
- `src/gather.c`
- `src/gather.h`
- `src/decl/gather-decl.h`
- `R/slice-by-lane.R`
- `src/slice-by-lane.c`
- `src/slice-by-lane.h`
- `src/decl/slice-by-lane-decl.h`
- `tests/testthat/test-gather.R`
- `tests/testthat/test-slice-by-lane.R`

The gather header declares internal C entry points that lane slicing can reuse.
FFI declarations remain in `src/init.c`.

Keep each `.c` file in top-down order. Include its declaration header last.
Do not add comments to C or R source files.

## Test plan

### Gather validation

- `indices` must be a nonempty list.
- Bare integer vectors and arrays work.
- Doubles, logicals, characters, factors, and other classed inputs error.
- `axes = NULL` requires one index per source axis.
- Explicit `axes` must have one entry per index.
- Axes must be valid, unique, and strictly increasing.
- Zero, negative, and out-of-bounds coordinates error.
- Missing coordinates work for reads and error for assignment.
- Validation still occurs for empty results.

### Broadcasting

- Equal coordinate dimensions pair pointwise.
- Scalar dimensions broadcast.
- Singleton dimensions on different axes produce Cartesian products.
- Missing trailing axes follow rray broadcasting.
- Incompatible dimensions error.
- Zero dimensions combine according to the existing common-dimension rules.
- Index names coalesce with the existing broadcasting rules.

### Output dimensions

- All source axes selected returns exactly `G`.
- One selected axis replaces that axis with all dimensions of `G`.
- Multiple adjacent axes are replaced by one `G` block.
- Multiple nonadjacent axes use the same placement rule.
- Unselected axes before and after the first selected axis keep their order.
- Maximum supported dimensionality errors before allocation when exceeded.

### Values

- All seven native storage types.
- One through several selected axes.
- One through several untouched axes.
- Paired coordinates.
- Cartesian coordinates.
- Repeated coordinates.
- Missing coordinates.
- Empty gathers.

Build a small R oracle that walks every output point, constructs its complete
source coordinate, and reads `x` through a point matrix. Compare values and
dimensions.

### Names

- Selected source axis names are dropped.
- Unselected source axis names follow their axes.
- Index array names populate `G` through coalescing.
- Broadcast index axes lose names from an input whose dimension changed.
- A full unnamed gather has no names.
- Assignment returns the original names of `x`.

### Assignment

- Lossless casts succeed and lossy casts fail.
- Scalar and array broadcasting to gather result dimensions.
- Arbitrary recycling fails.
- Repeated targets use last-write-wins order.
- Missing targets fail before copying.
- Empty targets validate and return a copy with unchanged contents.
- `value` equal to `x` works.
- Type, dimensions, and names of `x` are unchanged.
- Reading the selected targets agrees with the assignment target order.

### Lane behavior

- `i` must be a bare integer array with the same dimensionality as `x`.
- One-dimensional `x` uses strict lane coordinates.
- Positive coordinates work and zero or negative coordinates error.
- Exact non-selected dimensions work.
- Dimensions of one broadcast on non-selected axes.
- `i` cannot enlarge a non-selected axis whose source dimension is one.
- Only the selected result dimension changes.
- The selected axis loses names and all other axes keep names.
- Lane results equal generic full gather results built with explicit identity
  coordinates.
- Assignment agrees with the same explicit gather targets.

### Reference properties

For positive integer orthogonal subscripts:

```r
rray_slice(x, i, j, k)
```

must equal a full gather over open-mesh versions of `i`, `j`, and `k`, including
dimensions and values. Compare names separately because the slice wrapper owns
source-name selection.

For a point matrix:

```r
rray_extract(x, points)
```

must equal a full gather over the columns of `points`.

For lane indexing:

```r
rray_slice_by_lane(x, i, axis)
```

must equal a full gather over `i` and explicit identity coordinates on every
other source axis.

For NumPy comparisons, account for one-based coordinates, rray's left-aligned
broadcasting, and R's column-major display order. Use only cases where the
broadcast shapes are equivalent. Do not use NumPy's mixed-index axis-placement
rule as the rray result-shape oracle.

### Required checks

After each C change:

1. Run the explicit protection review required by `AGENTS.md`.
2. Run `clang-format -i src/*.c src/*.h`.
3. Run `air format .`.
4. Run focused tests, then all tests.
5. Run `devtools::check()` for the final pull request.

## Delivery order

### 1. Correct the slice-axis split

Update `slice.md` and the implementation so `rray_slice_axis()` has only
ordinary shared subscript semantics. Add `rray_slice_by_lane()` as the strict
per-lane operation.

This should happen before the old shape dispatch becomes public behavior.

### 2. Implement generic gather

Implement read-only `rray_gather()` with integer coordinates, common
broadcasting, untouched axes, result-shape placement, names, and all storage
types.

Use the loop oracle and the fully worked examples above as the first tests.

### 3. Implement gather assignment

Add casting, value broadcasting, missing rejection, copying, and deterministic
last-write-wins behavior.

### 4. Share lower-level machinery

Once all families have tests, share coordinate validation, gather offsets, and
typed copy helpers where doing so leaves each public contract clear. Keep the
orthogonal affine fast path and the specialized lane iterator.

### 5. Update the main plans

Update the recommendation, API table, implementation files, tests, and
delivery order in `plans/slice.md`. Then update the corresponding section of
`plans/implementation.md`.

## Research notes

### NumPy integer array indexing

NumPy integer index arrays broadcast and are iterated pointwise. When every
source axis has an integer index array, the result dimensions are the common
index dimensions. A basic slice leaves a source axis independent of the
advanced index space.

Useful sources:

- [Integer array indexing](https://numpy.org/doc/stable/user/basics.indexing.html#integer-array-indexing)
- [Combining advanced and basic indexing](https://numpy.org/doc/stable/user/basics.indexing.html#combining-advanced-and-basic-indexing)
- [`numpy.take()`](https://numpy.org/doc/stable/reference/generated/numpy.take.html)
- [`numpy.take_along_axis()`](https://numpy.org/doc/stable/reference/generated/numpy.take_along_axis.html)

NumPy's bracket syntax changes output-axis placement depending on whether
advanced indices are adjacent. rray4 should not copy that rule. The gather
block always replaces the selected source axes at the first selected axis.

### NumPy indexing proposal

NEP 21 describes NumPy's broadcasted advanced indexing as vectorized indexing
and proposes explicit `vindex` and `oindex` APIs. The proposal is deferred, so
these are useful terms rather than stable NumPy APIs.

- [NEP 21: Simplified and explicit advanced indexing](https://numpy.org/neps/nep-0021-advanced-indexing.html)

### Base R

Base R point-matrix indexing is the one-dimensional, all-axes-selected case of
gather. Base R does not expose arbitrary broadcast index dimensions through a
separate function.

- [Extract or Replace Parts of an Object](https://stat.ethz.ch/R-manual/R-patched/library/base/html/Extract.html)
