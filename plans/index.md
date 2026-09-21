# Location indexing plan

## Recommendation

Add one general location-indexing function and one single-axis convenience
function:

```r
rray_index(x, locations = list(), axes = NULL, cross = FALSE)
rray_index_assign(
  x,
  locations = list(),
  axes = NULL,
  value,
  cross = FALSE
)

rray_index_axis(x, locations, axis, cross = FALSE)
rray_index_assign_axis(x, locations, axis, value, cross = FALSE)
```

Each supplied location array controls one source axis. Supplied location arrays
broadcast to common dimensions and are read pointwise. The `cross` argument
controls only the source axes that do not have a supplied location array:

- With `cross = FALSE`, unspecified axes receive implicit identity locations.
  Their source positions pair with matching output positions.
- With `cross = TRUE`, unspecified axes remain independent. They cross with
  every point in the common supplied-location space.

Supplied location arrays always pair through broadcasting. `cross` never
changes them from paired to Cartesian indexing. Users request a Cartesian
product among supplied arrays by shaping them as an open mesh.

The full indexing family is:

| Operation | Read | Assign |
|---|---|---|
| Orthogonal subscripts | `rray_slice()` | `rray_slice_assign()` |
| One-axis subscript | `rray_slice_axis()` | `rray_slice_assign_axis()` |
| Location arrays | `rray_index()` | `rray_index_assign()` |
| One-axis location array | `rray_index_axis()` | `rray_index_assign_axis()` |
| Flat positions or point matrix | `rray_extract()` | `rray_extract_assign()` |

Do not add replacement functions. Every `_assign()` function returns a modified
copy of `x`.

## Subscripts and locations

A subscript describes a set of positions on an axis. It can be negative,
logical, character, missing, or `NULL`. Subscript normalization produces a
one-dimensional sequence of positive or missing locations. Subscripts on
different axes form a Cartesian product.

A location array has a narrower meaning. Each non-missing element is one
positive, one-based position on one source axis. Its dimensions connect that
position to a point in the result. Zero and negative locations cannot mean
complement because every element must identify one source position.

This gives the public functions stable contracts:

```r
rray_slice_axis(x, -1L, axis = 2L)
# Complement selection

rray_index_axis(x, array(-1L, rray_dimensions(x)), axis = 2L)
# Error: locations must be positive
```

The shape or class of an argument never changes an ordinary subscript into a
location array. The function name determines the rules.

## The location model

Let:

- `D` be the source dimensions.
- `n` be the dimensionality of `x`.
- `S` be the selected source axes named by `axes`.
- `U` be the source axes not in `S`.
- `L[j]` be the location array for `S[j]`.
- `G` be the common dimensions of all supplied location arrays.

Every supplied array gives one source coordinate at each point in `G`:

```text
source[S[j]] = broadcast(L[j])[g]
```

The selected coordinates are always paired at the same point `g`. Singleton
dimensions and missing trailing dimensions follow rray's left-aligned
broadcasting rules.

### Location validation

Each element of `locations` must be a bare integer vector or array. Bare
vectors normalize to one-dimensional arrays. Factors and other classed integer
objects are errors.

Every non-missing location must be positive, one based, and no greater than the
dimension of its source axis.

- Positive integers identify one position.
- Zero and negative integers are errors.
- Missing locations produce missing output when reading.
- Missing locations are errors when assigning.
- Out-of-bounds locations are errors.

Validation happens before allocating or copying the result. Every supplied
location is validated even when another result axis has dimension zero.

The first version should accept only integer locations. Logical values are
masks, character values require name matching, and doubles weaken the contract
without adding addressing power. Those inputs already belong to slicing or
extraction.

### Axis selection

`locations` is a list and `axes` identifies the source axis controlled by each
element.

- `axes` must have the same size as `locations`.
- Explicit axes must be valid, unique, and strictly increasing.
- With a nonempty `locations` and `axes = NULL`, there must be one location
  array per source axis. They map to all source axes in order.
- With `locations = list()` and `axes = NULL`, no source axes are selected.
  The result is `x`.
- Selecting only some source axes requires explicit `axes`.

Requiring increasing axes gives every call one canonical representation and
makes crossed result placement deterministic.

## `cross = FALSE`

With `cross = FALSE`, every unspecified source axis receives an implicit
identity location array. For an unselected axis `a`, its identity dimensions
are:

```text
(1, ..., D[a], ..., 1)
```

and its values are:

```text
1, 2, ..., D[a]
```

The implementation represents identity locations in the plan. It does not
allocate them as R arrays.

The supplied location arrays and all implicit identity arrays broadcast to
common dimensions `H`. Every source axis then has a coordinate at every point
in `H`, so the result dimensions are exactly `H`:

```text
out_dimensions = H
```

For an output point `h`:

```text
if a is selected:
  source[a] = broadcast(L[j])[h]
else:
  source[a] = identity[a][h]

out[h] = x[source]
```

### Shape requirements

When at least one but not every source axis is selected, every supplied
location array must have the same dimensionality as `x`. Its axes then align
unambiguously with the source axes that receive identities.

On an unspecified source axis `a`, every supplied location dimension must be
one or `D[a]`. The identity array forces the result dimension to `D[a]`.
Therefore:

- The result has the same dimensionality as `x`.
- Every unspecified axis keeps its source dimension.
- Selected-axis dimensions come from the common location dimensions.
- A location array cannot enlarge an unspecified source axis, including an
  axis of `x` whose dimension is one.

When every source axis is selected, there are no identities to align. Supplied
location arrays can have any dimensionality and the result dimensions are
their common dimensions `G`.

When no source axes are selected, every axis is an identity. The result has
dimensions `D` and values identical to `x`.

### Single-axis invariant

For `rray_index_axis(x, locations, axis, cross = FALSE)`, `locations` must have
the same dimensionality as `x` unless `x` is one dimensional.

- On `axis`, the dimension of `locations` becomes the result dimension.
- On every other axis, its dimension must be one or equal the matching source
  dimension.
- Dimensions of one broadcast across matching source lanes.
- The result has the same dimensionality as `x`.
- Only the selected axis dimension can change.

For source dimensions `(A, B, C)` and `axis = 2`:

```text
locations dimensions = (A or 1, J, C or 1)
result dimensions    = (A, J, C)
```

This is the natural consumer of locations returned by operations such as
`rray_locate_min()` and `rray_locate_max()`.

## `cross = TRUE`

With `cross = TRUE`, unspecified source axes do not receive coordinates. They
remain independent and cross with every point in `G`.

Let:

- `p` be the first selected source axis.
- `U_before` be unselected source axes before `p`.
- `U_after` be unselected source axes after `p`.

The result dimensions are:

```text
D[U_before] + G + D[U_after]
```

In words, remove every selected source axis and insert the common supplied
location dimensions where the first selected axis occurred. All unselected
source axes keep their relative order.

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

For an output point:

```text
o = (u_before, g, u_after)
```

construct one source coordinate for every source axis:

```text
if a is selected:
  source[a] = broadcast(L[j])[g]
else:
  source[a] = the matching coordinate from u_before or u_after
```

Then:

```text
out[o] = x[source]
```

### Shape requirements

Supplied location arrays can have any dimensionality. Their common dimensions
replace the selected source axes as one block. The result dimensionality is:

```text
n - length(S) + dimensionality(G)
```

This is the central crossed-shape invariant. Unlike `cross = FALSE`, a
single-axis location array does not need to align with the dimensionality of
`x`.

For source dimensions `(A, B, C)`, `axis = 2`, and location dimensions
`(J, K)`:

```text
result dimensions = (A, J, K, C)
```

Each point in `(J, K)` selects an entire `(A, C)` combination because those
source axes remain independent.

## Cases where `cross` has no effect

When every source axis is selected, no axis remains to cross or receive an
identity. Both values of `cross` return common supplied-location dimensions
`G`.

When no source axis is selected, there is no supplied-location space to cross.
Both values return `x`.

The flag still belongs in these calls so a program can forward one indexing
choice without inspecting `axes` first.

## `rray_index()`

```r
rray_index(x, locations = list(), axes = NULL, cross = FALSE)
```

`rray_index()` implements the complete model above.

### Argument rules

- `x` must be an array or bare vector supported by rray.
- Bare vectors normalize to one-dimensional arrays.
- `locations` must be a list.
- Every location input must be a bare integer vector or array.
- `axes` follows the selection rules above.
- `cross` must be one non-missing logical value.
- The resulting dimensionality must not exceed rray's supported maximum.

The function returns the same storage type as `x`.

### Missing locations

If any selected coordinate at an output point is missing, that output value is
missing. The storage-specific results are:

| Type | Missing output |
|---|---|
| logical | `NA` |
| integer | `NA_integer_` |
| double | `NA_real_` |
| complex | `NA_complex_` |
| character | `NA_character_` |
| raw | `as.raw(0)` |
| list | `NULL` |

With `cross = TRUE`, a missing point in `G` produces missing values across all
combinations of the independent source axes. With `cross = FALSE`, it affects
only the matching identity-paired output point.

### Zero dimensions

Zero dimensions follow the existing common-dimension rules.

- Equal zero dimensions are compatible.
- Zero and one combine to zero.
- Zero and a dimension greater than one are incompatible.
- A crossed unselected zero dimension makes the result empty.
- An identity zero dimension also makes the common result dimension zero.

All coordinates are still validated before returning an empty result.

## `rray_index_axis()`

```r
rray_index_axis(x, locations, axis, cross = FALSE)
```

The axis function is exactly the one-location-array form of `rray_index()`:

```r
rray_index_axis(x, locations, axis, cross = cross)
```

is equivalent to:

```r
rray_index(
  x,
  locations = list(locations),
  axes = axis,
  cross = cross
)
```

It does not add another addressing mode or another set of validation rules.
Errors should use the `locations` and `axis` argument names from the public
axis call.

The implementation can use a specialized iterator for the common
`cross = FALSE` case. That optimization must preserve exact equivalence with
the general function.

## Worked examples

### 1. Full pointwise indexing

```r
x <- matrix(1:12, nrow = 3)

rows <- c(1L, 3L, 2L)
columns <- c(4L, 1L, 3L)

out <- rray_index(
  x,
  locations = list(rows, columns)
)

out
# [1] 10 3 8

dim(out)
# [1] 3
```

Both source axes are selected, so `cross` has no effect. The calculation is:

```text
out[1] = x[rows[1], columns[1]] = x[1, 4] = 10
out[2] = x[rows[2], columns[2]] = x[3, 1] = 3
out[3] = x[rows[3], columns[3]] = x[2, 3] = 8
```

### 2. Cartesian indexing among supplied arrays

Supplied arrays always pair through broadcasting. Shape them onto different
axes to request a Cartesian product:

```r
rows <- array(c(3L, 1L), c(2L, 1L))
columns <- array(c(4L, 2L, 1L), c(1L, 3L))

out <- rray_index(
  x,
  locations = list(rows, columns)
)

dim(out)
# [1] 2 3
```

The common dimensions are `(2, 3)`. Every row location pairs with every column
location because their singleton dimensions form an open mesh.

Ordinary vectors of lengths two and three do not silently form a Cartesian
product:

```r
rray_index(x, list(c(1L, 2L), c(1L, 2L, 3L)))
# Error: dimensions 2 and 3 cannot broadcast
```

### 3. Identity-paired single-axis indexing

```r
x <- matrix(
  c(
    10, 20, 30, 40,
    50, 60, 70, 80
  ),
  nrow = 2,
  byrow = TRUE
)

locations <- rbind(
  c(4L, 1L),
  c(2L, 3L)
)

out <- rray_index_axis(
  x,
  locations,
  axis = 2L,
  cross = FALSE
)

out
#      [,1] [,2]
# [1,]   40   10
# [2,]   60   70
```

The row axis receives identity locations:

```text
out[row, column] = x[row, locations[row, column]]
```

Only the selected column dimension changes.

### 4. Crossed single-axis indexing

Using the same `locations` with `cross = TRUE` gives:

```r
out <- rray_index_axis(
  x,
  locations,
  axis = 2L,
  cross = TRUE
)

dim(out)
# [1] 2 2 2
```

The first output axis is the independent source row. The next two axes are the
location dimensions:

```text
out[a, j, k] = x[a, locations[j, k]]
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

The location row `j` is unrelated to the source row `a`. Equal dimensions do
not establish a connection. Only `cross = FALSE` supplies that identity
connection.

### 5. Identity broadcasting

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

locations <- matrix(c(4L, 1L), nrow = 1)

out <- rray_index_axis(x, locations, axis = 2L)
```

The one row of `locations` broadcasts across the identity row dimension:

```r
out
#      [,1] [,2]
# [1,]   40   10
# [2,]   80   50
# [3,]  120   90

dim(out)
# [1] 3 2
```

An array with dimensions `(4, 2)` would be an error because its first
dimension cannot broadcast with the source row dimension three.

### 6. Multiple selected axes with identities

Let `x` have dimensions `(A, B, C)` and select axes 1 and 3 with
`cross = FALSE`. Both supplied location arrays must have dimensionality three:

```text
row_locations dimensions   = (J, B or 1, K)
depth_locations dimensions = (J, B or 1, K)
```

Axis 2 receives identity locations with dimensions `(1, B, 1)`. The result has
dimensions `(J, B, K)` and:

```text
out[j, b, k] =
  x[row_locations[j, b, k], b, depth_locations[j, b, k]]
```

With `cross = TRUE`, axis 2 instead remains independent. If the common supplied
dimensions are `(J, K)`, the result dimensions are `(J, K, B)` because the
common block replaces selected axes 1 and 3 at the position of the first
selected axis.

### 7. One-dimensional arrays

For one-dimensional `x`, selecting its only axis leaves no unspecified axes.
The value of `cross` has no effect:

```r
x <- array(c(10, 20, 30, 40), 4L)
locations <- array(c(4L, 1L), 2L)

rray_index_axis(x, locations, axis = 1L, cross = FALSE)
rray_index_axis(x, locations, axis = 1L, cross = TRUE)
# Both return array(c(40, 10), 2L)
```

Negative locations remain invalid. Ordinary complement selection belongs to
`rray_slice_axis()`.

### 8. Empty location list

```r
rray_index(x)
```

No source axes are selected. Both values of `cross` return a normalized copy
of `x` with the same dimensions and names.

## Names

Supplied location arrays carry dimension names through the existing common-name
broadcasting rules.

With `cross = FALSE`:

- Implicit identity arrays carry source names on their non-singleton axes.
- Unspecified source axes therefore keep their names.
- Selected source axis names are dropped.
- Supplied location names can name selected result dimensions.
- Names on broadcast dimensions coalesce with the ordinary broadcasting rules.

With `cross = TRUE`:

- Unselected source axes keep their names and their relative order.
- The inserted common location dimensions use names coalesced from the supplied
  arrays.
- Selected source axis names are dropped.

When every source axis is selected, result names come only from the common
supplied-location dimensions.

Assignment always returns the original dimensions and names of `x` unchanged.

## Assignment

```r
rray_index_assign(
  x,
  locations = list(),
  axes = NULL,
  value,
  cross = FALSE
)

rray_index_assign_axis(x, locations, axis, value, cross = FALSE)
```

Assignment follows the same indexing plan as reading:

1. Normalize and validate `x`, `locations`, `axes`, and `cross`.
2. Compute the indexing result dimensions.
3. Validate every location and reject missing values.
4. Cast `value` losslessly to the type of `x`.
5. Broadcast the cast value to the indexing result dimensions.
6. Copy `x`.
7. Write in column-major indexing-result order.

The operation cannot change the type, size, dimensions, or names of `x`.
Broadcasting can repeat dimensions of one but cannot recycle an arbitrary
size.

Repeated source locations use deterministic last-write-wins behavior. The
final value visited in column-major result order wins.

An error must occur before `x` is copied or any writes happen. Casting and
broadcasting `value` before copying also handles `value` equal to `x`.

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
  locations = list(rows, columns),
  value = c(100, 200, 300)
)

out
#      [,1] [,2] [,3] [,4]
# [1,]   10  200   30   40
# [2,]   50   60   70  300
```

The point `(1, 2)` appears twice. The second value wins.

## Connection to slicing

Slicing accepts ordinary subscripts and forms a Cartesian product. Indexing
accepts strict location arrays and reads them pointwise. They are separate
public contracts, but positive slicing can be expressed through indexing after
subscript normalization.

### Orthogonal slicing

After normalizing one subscript per source axis to positive locations, reshape
each location vector onto its source axis:

```text
axis 1 locations: (I, 1, 1)
axis 2 locations: (1, J, 1)
axis 3 locations: (1, 1, K)
```

The arrays broadcast to `(I, J, K)`. Because every source axis is selected,
`cross` has no effect:

```r
rray_slice(x, i, j, k)
```

has the same values and dimensions as:

```r
rray_index(
  x,
  locations = list(
    array(normalize(i), c(I, 1L, 1L)),
    array(normalize(j), c(1L, J, 1L)),
    array(normalize(k), c(1L, 1L, K))
  )
)
```

`rray_slice()` remains separate because it accepts richer subscript forms,
subsets source names, guarantees one result axis per source axis, and can use
an affine strided fast path.

### One-axis slicing

After normalizing `i` to a one-dimensional location vector:

```r
rray_slice_axis(x, i, axis)
```

has the same values and dimensions as:

```r
rray_index_axis(
  x,
  locations = normalize(i),
  axis = axis,
  cross = TRUE
)
```

The unspecified source axes cross with the shared one-dimensional locations.
The slice wrapper owns ordinary subscript rules and selected-axis name
subsetting.

## Connection to extraction

Extraction always returns a one-dimensional array. Point matrices and flat
positions both lower to indexing every source axis.

### Point matrices

For a point matrix with one row per requested point and one column per source
axis:

```r
points <- rbind(
  c(1L, 4L),
  c(3L, 1L),
  c(2L, 3L)
)
```

this:

```r
rray_extract(x, points)
```

has the same values and dimensions as:

```r
rray_index(
  x,
  locations = list(points[, 1], points[, 2])
)
```

Every source axis is selected and every location vector has dimensions `P`, so
the result has dimensions `P`. Numeric and character point matrices are first
normalized to integer locations.

`rray_extract()` retains the matrix representation because it is the ordinary
R representation of point coordinates and because the function also owns flat
extraction.

### Flat positions

A flat position can be unravelled into one coordinate for every source axis in
R's column-major order:

```text
flat locations
  -> unravel to one location vector per source axis
  -> rray_index() over every source axis
```

This gives the same values, dimensions, missing behavior, and duplicate order
as flat extraction after the flat subscript has been normalized.

The implementation should keep a direct flat path. Unravelling expands one
compact location into one coordinate per source axis and adds no value to the
hot path.

Logical masks follow the same semantic lowering after their selected flat
positions are found. Negative, zero, and double flat subscripts are normalized
before unravelling.

### One-dimensional result invariant

Point extraction supplies one one-dimensional coordinate vector per source
axis. Flat extraction can be converted to the same representation. Therefore:

```text
extract = full indexing with one-dimensional common locations
```

This is a semantic and testing relationship. It does not require extraction to
allocate coordinate arrays internally.

## Connection to location-producing functions

Location-producing functions should return bare integer arrays shaped for
`cross = FALSE` indexing.

```r
locations <- rray_locate_max(x, axis = 2L)
rray_index_axis(x, locations, axis = 2L)
```

If `x` has dimensions `(A, B, C)`, locating along axis 2 returns dimensions
`(A, 1, C)`. Indexing those locations returns dimensions `(A, 1, C)` and one
maximum per lane.

Assignment through the same locations updates one position per lane:

```r
rray_index_assign_axis(
  x,
  locations,
  axis = 2L,
  value = 0
)
```

If a lane has tied extrema, the locating function chooses according to its tie
rule. Replacing every tied value uses a logical mask with
`rray_extract_assign()` instead.

## Implementation design

### Location normalization

Build location validation on the point-coordinate layer used by extraction,
not on ordinary subscript normalization.

The normalizer owns:

- Bare integer vector and array validation.
- One-based bounds checks against a specific source axis.
- Missing detection.
- Conversion to zero-based coordinate reads for the iterator.
- Common dimension and broadcast-stride construction.
- The conditional dimensionality rule for `cross = FALSE`.

Do not send locations through ordinary subscript normalization. In particular,
zero removal and negative complement have no meaning here.

### Index plan

Build one plan containing:

- Source dimensions and column-major strides.
- Selected and unselected source axes.
- Normalized supplied location arrays.
- Common supplied-location dimensions `G`.
- Result dimensions.
- Broadcast strides for every supplied location array.
- A mapping from source axes to result axes.
- Identity descriptors for unspecified axes when `cross = FALSE`.
- Independent-axis descriptors when `cross = TRUE`.
- Whether any supplied location is missing.

The plan exposes one source offset for every result point. Read and assignment
use the same source-offset order.

### Identity path

For `cross = FALSE`:

1. Add one implicit identity descriptor for each unspecified axis.
2. Merge supplied and identity dimensions into result dimensions `H`.
3. Give each supplied location array its broadcast strides over `H`.
4. Give each identity descriptor its source stride on its matching result axis
   and zero elsewhere.
5. Walk `H` once and combine every selected location with every identity
   coordinate into a source offset.

For the single-axis case, the existing two-location strided iterator is a good
fit. One location tracks the source base from identities while the other tracks
the possibly broadcast supplied location array.

### Crossed path

For `cross = TRUE`:

1. Compute `G` from the supplied location arrays.
2. Precompute the selected-axis source offset for each point in `G`.
3. Use a sentinel for a missing supplied coordinate.
4. Walk the full result dimensions.
5. Add the precomputed selected offset to the source offset contributed by
   independent axes.

Precomputing selected offsets avoids rereading every supplied array for each
combination of independent axes. If the offset storage becomes material, add
an on-the-fly path only after profiling.

### Typed cores

Read and assignment share one source-location order.

- Atomic cores write through data pointers.
- Character and list cores use the write barrier.
- Missing reads write the storage-specific missing value.
- Assignment rejects missing locations before copying `x`.
- Assignment writes in column-major indexing-result order.

The identity and crossed plans can call the same typed cores once they expose
the next source offset through a small common interface.

### Protection review

Before calling any C change done, perform the explicit protection review from
`AGENTS.md`. In particular, protect:

- Normalized `x` before normalizing any location array.
- Every normalized location array stored across later allocations.
- Common dimensions before building result dimensions.
- Result dimensions and names across result allocation.
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

### Validation

- `locations` must be a list for `rray_index()`.
- Bare integer vectors and arrays work.
- Doubles, logicals, characters, factors, and other classed inputs error.
- Nonempty `locations` with `axes = NULL` requires one array per source axis.
- Empty `locations` with `axes = NULL` selects no axes.
- Explicit `axes` must have one entry per location array.
- Axes must be valid, unique, and strictly increasing.
- `cross` must be one non-missing logical value.
- Zero, negative, and out-of-bounds locations error.
- Missing locations work for reads and error for assignment.
- Validation still occurs for empty results.

### Supplied-location broadcasting

- Equal dimensions pair pointwise.
- Singleton dimensions broadcast.
- Singleton dimensions on different axes produce Cartesian products.
- Missing trailing axes follow rray broadcasting.
- Incompatible dimensions error.
- Zero dimensions combine according to the existing rules.
- Location names coalesce with the existing broadcasting rules.
- `cross` never changes broadcasting among supplied arrays.

### Identity behavior

- Partial selection requires supplied arrays with the dimensionality of `x`.
- Exact unspecified dimensions work.
- Dimensions of one broadcast over identity axes.
- Supplied arrays cannot enlarge an unspecified source axis.
- Result dimensionality equals that of `x` for partial selection.
- Unspecified result dimensions equal their source dimensions.
- Selected result dimensions come from common supplied dimensions.
- Identity results equal full indexing with explicit identity arrays.
- One-dimensional `x` has no remaining identity axes.

### Crossed behavior

- Supplied arrays can have any dimensionality.
- One selected axis replaces that axis with every dimension of `G`.
- Multiple adjacent selected axes are replaced by one `G` block.
- Multiple nonadjacent selected axes use the same placement rule.
- Unselected axes before and after the first selected axis keep their order.
- Result dimensionality is `n - length(S) + dimensionality(G)`.
- Every independent source position crosses with every point in `G`.
- Maximum supported dimensionality errors before allocation.

### Cross-invariant cases

- Selecting every source axis gives identical results for both flag values.
- Selecting no source axes gives identical normalized copies for both values.
- The flag can be forwarded without special-casing either situation.

### Values

- All seven native storage types.
- One through several selected axes.
- One through several unspecified axes.
- Paired coordinates.
- Cartesian coordinates formed through broadcasting.
- Repeated coordinates.
- Missing coordinates.
- Empty indexing results.

Build a small R oracle that walks every output point, constructs its complete
source coordinate, and reads `x` through a point matrix. Compare values and
dimensions.

### Names

- Identity axes keep source names.
- Crossed independent axes keep source names.
- Selected source axis names are dropped.
- Location names populate common result dimensions.
- Broadcast location axes lose names when their dimension changes.
- Full unnamed indexing has no names.
- Assignment returns the original names of `x`.

### Assignment

- Lossless casts succeed and lossy casts fail.
- Scalar and array broadcasting to indexing result dimensions.
- Arbitrary recycling fails.
- Repeated targets use last-write-wins order.
- Missing targets fail before copying.
- Empty targets validate and return a copy with unchanged contents.
- `value` equal to `x` works.
- Type, dimensions, and names of `x` are unchanged.
- Reading selected targets agrees with assignment target order.

### Axis equivalence

For both values of `cross`:

```r
rray_index_axis(x, locations, axis, cross = cross)
```

must equal:

```r
rray_index(
  x,
  locations = list(locations),
  axes = axis,
  cross = cross
)
```

Compare values, dimensions, names, missing behavior, assignment results, and
errors.

### Slice properties

For positive integer orthogonal subscripts, compare `rray_slice()` with full
indexing over open-mesh location arrays. Compare names separately because the
slice wrapper owns source-name selection.

For positive one-axis subscripts, compare `rray_slice_axis()` with
`rray_index_axis(cross = TRUE)` after normalizing the subscript to locations.

### Extract properties

For a point matrix, compare `rray_extract()` with full indexing over the point
matrix columns.

For flat positions, unravel normalized positions to one location vector per
source axis and compare with full indexing. Cover missing and duplicate
positions and column-major order.

### Required checks

After each C change:

1. Run the explicit protection review required by `AGENTS.md`.
2. Run `clang-format -i src/*.c src/*.h`.
3. Run `air format .`.
4. Run focused tests, then all tests.
5. Run `devtools::check()` for the final pull request.

## Delivery order

### 1. General read path

Implement `rray_index()` reads with integer locations, supplied-location
broadcasting, both `cross` modes, result dimensions, names, missing locations,
and every storage type.

Use the loop oracle and worked examples as the first tests.

### 2. Axis read path

Add `rray_index_axis()` as an exact one-array wrapper. Start by calling the
general internal path, then add the specialized identity iterator without
changing behavior.

### 3. Assignment

Implement both assignment functions with casting, value broadcasting, missing
rejection, copying, and deterministic last-write-wins behavior.

### 4. Share lower-level machinery

Share location validation, source-offset iteration, and typed cores with point
extraction where doing so leaves each public contract clear. Keep slicing's
affine fast path and flat extraction's compact iterator.

### 5. Update the main implementation plan

Update the corresponding section of `plans/implementation.md`. Keep
`plans/slice.md` as the source of truth for ordinary subscripts and extraction,
and this file as the source of truth for location arrays.

## Research notes

### Sources of inspiration

The design should look to NumPy's `take()`, `take_along_axis()`, advanced
indexing, and NEP 21 as sources of inspiration. Each contributes a different
part of rray's indexing model:

| Source | Main idea | rray connection |
|---|---|---|
| `take()` | One location array replaces one source axis while every other axis remains independent | `rray_index_axis(cross = TRUE)` |
| `take_along_axis()` | One location array is matched with corresponding one-dimensional data slices | `rray_index_axis(cross = FALSE)` |
| Advanced indexing | Multiple integer arrays broadcast and are read pointwise | Supplied arrays in `rray_index()` always broadcast and pair pointwise |
| NEP 21 | Orthogonal and vectorized indexing should have explicit, separate contracts | `rray_slice()` owns Cartesian subscript selection and `rray_index()` owns paired location arrays |

These are conceptual sources rather than APIs to copy exactly. rray uses
one-based locations, left-aligned broadcasting, stable output placement,
strict assignment casting, and explicit name rules.

### `take()` and crossed axis indexing

NumPy's `take()` leaves source axes other than `axis` independent. The full
shape of its index array replaces the selected axis. This supplies the shape
model for:

```r
rray_index_axis(x, locations, axis, cross = TRUE)
```

The relationship concerns positive locations and result shape. It does not
make `rray_slice_axis()` an alias for `take()`. Slicing accepts ordinary R
subscripts, including negative complements, logical masks, and names.

### `take_along_axis()` and identity indexing

NumPy's `take_along_axis()` matches every one-dimensional index slice with the
corresponding data slice. This supplies the identity-coordinate model for:

```r
rray_index_axis(x, locations, axis, cross = FALSE)
```

Functions such as `rray_locate_min()` and `rray_locate_max()` should return
locations shaped for this operation. NumPy's `put_along_axis()` also informs
the corresponding assignment operation:

```r
rray_index_assign_axis(x, locations, axis, value, cross = FALSE)
```

### Advanced indexing and general indexing

NumPy advanced indexing broadcasts multiple integer arrays and reads them
pointwise. This supplies the core model for the location list accepted by:

```r
rray_index(x, locations, axes, cross)
```

Supplied location arrays always broadcast and pair pointwise. Singleton
dimensions on different axes form an explicit open mesh when a Cartesian
product is wanted.

NumPy changes output-axis placement depending on whether advanced indices are
adjacent. rray4 should not copy that rule. Crossed indexing always inserts the
common location block where the first selected source axis occurred.

### NEP 21 and explicit contracts

NEP 21 names NumPy's paired behavior vectorized indexing and proposes a
separate orthogonal indexing operation. The proposal is deferred, but its main
design lesson is valuable: paired and Cartesian indexing should not be selected
implicitly by the types or arrangement of arguments.

rray applies that lesson through separate public families:

- `rray_slice()` and `rray_slice_axis()` accept ordinary subscripts and use
  Cartesian selection.
- `rray_index()` and `rray_index_axis()` accept strict location arrays and pair
  supplied coordinates pointwise.
- `rray_extract()` gives flat positions and point matrices an explicit
  one-dimensional result contract.

The `cross` flag adds a choice that is separate from NEP 21's distinction
between paired and Cartesian indexing. It controls only source axes without
supplied location arrays. It never changes how supplied arrays relate to each
other.

Useful sources:

- [`numpy.take()`](https://numpy.org/doc/stable/reference/generated/numpy.take.html)
- [`numpy.take_along_axis()`](https://numpy.org/doc/stable/reference/generated/numpy.take_along_axis.html)
- [`numpy.put_along_axis()`](https://numpy.org/doc/stable/reference/generated/numpy.put_along_axis.html)
- [NumPy advanced indexing](https://numpy.org/doc/stable/user/basics.indexing.html#advanced-indexing)
- [NEP 21: Simplified and explicit advanced indexing](https://numpy.org/neps/nep-0021-advanced-indexing.html)

### Base R

Base R point-matrix indexing is the one-dimensional, all-axes-selected case of
`rray_index()`. Base R does not expose arbitrary broadcast location dimensions
through a separate function.

- [Extract or Replace Parts of an Object](https://stat.ethz.ch/R-manual/R-patched/library/base/html/Extract.html)
