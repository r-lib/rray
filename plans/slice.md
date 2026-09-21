# Array slicing plan

## Recommendation

Use `slice` for dimension-preserving selection. Use `extract` for selection
that returns a one-dimensional array. Flat positions and coordinate points are
two input forms for `rray_extract()`.

Keep ordinary subscripts separate from location arrays. A subscript describes
a set of positions and can use ordinary R forms such as negative integers,
logical masks, and names. A location array contains one positive, one-based
integer location or a missing value for each output position. Its shape
connects each location to the other axes of `x`.

The proposed family is:

| Operation | Read | Assign |
|---|---|---|
| Slice every axis | `rray_slice(x, ...)` | `rray_slice_assign(x, ..., value)` |
| Slice one axis | `rray_slice_axis(x, i, axis)` | `rray_slice_assign_axis(x, i, axis, value)` |
| Slice at locations | `rray_slice_locations(x, locations, axis)` | `rray_slice_assign_locations(x, locations, axis, value)` |
| Extract flat positions or points | `rray_extract(x, i)` | `rray_extract_assign(x, i, value)` |

The three slice rows should be implemented first. Extraction forms the second
layer.

Do not add replacement functions such as `rray_slice<-()`. The `_assign()`
forms return a modified copy and match the rest of rray4's function API.
There are no `[` or `[[` methods because rray4 provides functions rather than
an array class.

## The model

There are five addressing modes. Orthogonal and one-axis slicing accept
ordinary subscripts. Location slicing accepts an integer location array. Flat
positions and coordinate points share `rray_extract()` because both return a
one-dimensional array.

The function name determines the input contract. The dimensionality or class
of an input never changes a subscript into a location array.

### Subscripts and location arrays

| Property | Subscript | Location array |
|---|---|---|
| Used by | `rray_slice()`, `rray_slice_axis()` | `rray_slice_locations()` |
| Meaning | A set of positions on an axis | One position for each output point |
| Accepted forms | Integer, double, logical, character, missing, `NULL` | Bare integer array |
| Negative values | Select the complement | Error |
| Zero | Ignored | Error |
| Shape | Does not connect positions to other axes | Connects locations to lanes and can broadcast |

A subscript is normalized into a one-dimensional sequence of locations before
slicing. Its original type expresses how to form that sequence. A location
array is already normalized in meaning, but not in storage: its elements still
need bounds and missing-value validation. This distinction lets
`rray_slice_axis()` keep ordinary R subscript rules while
`rray_slice_locations()` has one strict contract on arrays of any
dimensionality.

### Orthogonal slices

Each subscript selects locations on one axis. Multiple subscripts form a
Cartesian product.

```r
x <- array(1:24, c(2, 3, 4))

rray_slice(x, c(2, 1), c(3, 1), 2)
# dimensions: c(2, 2, 1)

rray_slice_axis(x, c(3, 1), axis = 2)
# dimensions: c(2, 2, 4)
```

This is the ordinary array operation. It is close to base R's
`x[..., drop = FALSE]` and NumPy's open-mesh indexing. It always preserves
dimensionality.

### Coordinate points

Each row of `points` identifies one element. Rows are paired coordinates, not a
Cartesian product.

```r
points <- rbind(
  c(1, 1, 1),
  c(2, 3, 4),
  c(1, 2, 3)
)

rray_extract(x, points)
# dimensions: 3L
# values: c(1L, 24L, 15L)
```

This is base R's matrix indexing and xtensor's `index_view()` model. The result
is one dimensional because no source axis survives as an output axis.

### Flat positions

The array is treated as its column-major storage vector.

```r
rray_extract(x, c(1, 4, 24))
rray_extract(x, x %% 5 == 0)
```

This covers base R's single-subscript array indexing and whole-array logical
masks. The result is one dimensional.

### Location slices

The location array supplies a different axis location for each lane through
the array. A lane is the one-dimensional vector left after every coordinate
except `axis` has been fixed.

```r
x <- array(c(10, 60, 30, 40, 20, 50), c(2, 3))
locations <- array(c(2L, 1L), c(2L, 1L))

rray_slice_locations(x, locations, axis = 2)
# dimensions: c(2, 1)
# values: c(30, 60)
```

This operation matches NumPy's `take_along_axis()`. It is the direct consumer
of arrays returned by functions such as `rray_locate_min()` and
`rray_locate_max()`. It also accepts per-lane sorting or ranking locations.

## Why these divisions

Base R gives `[` several unrelated meanings. A single subscript on an array is
flat, one subscript per axis is orthogonal, and a matrix subscript is pointwise.
It also drops dimensions by default. The behavior is powerful, but the same
syntax can return unrelated shapes.

NumPy distinguishes basic and advanced indexing, but multiple integer arrays
switch to paired advanced indexing. `x[rows, columns]` selects paired points,
while `x[np.ix_(rows, columns)]` selects the Cartesian product. This is one of
the easiest indexing rules to misread.

The original rray improved shape stability, but its names did not line up with
the addressing modes:

- `rray_subset()` performed dimension-preserving orthogonal slicing.
- `rray_extract()` performed the same orthogonal selection and then flattened
  it.
- `rray_yank()` used flat positions.
- `rray_slice()` was only the one-axis convenience wrapper.

The proposed API makes the common operation `rray_slice()`. `rray_extract()`
uses the shape of `i` to distinguish flat positions from coordinate points.
`rray_slice_axis()` always accepts one ordinary subscript. Location arrays have
the explicit `rray_slice_locations()` entry point.

## `rray_slice()`

```r
rray_slice(x, ...)
rray_slice_assign(x, ..., value)
```

`value` follows `...`, so it must be supplied by name.

Each element of `...` applies to the matching axis. A missing argument selects
the whole axis. Unspecified trailing axes also select the whole axis.

```r
rray_slice(x, 1)
# dimensions: c(1, 3, 4)

rray_slice(x, , 1)
# dimensions: c(2, 1, 4)

rray_slice(x)
# dimensions: c(2, 3, 4)
```

Trailing missing arguments have no effect. More subscripts than the
dimensionality of `x` are an error.

The dots should support dynamic splicing. This gives a programmatic API without
adding `rray_slice_axes()`.

```r
indices <- list(1:2, c(3, 1))
rray_slice(x, !!!indices)
```

Subscripts in `...` must be unnamed. This keeps selection positional and leaves
named axes available as a future extension.

## `rray_slice_axis()`

```r
rray_slice_axis(x, i, axis)
rray_slice_assign_axis(x, i, axis, value)
```

`rray_slice_axis()` has exactly the same subscript semantics as slicing one
axis with `rray_slice()`. It uses one subscript for every lane and only changes
the selected axis.

```r
x <- matrix(
  c(
    10, 20, 30, 40,
    50, 60, 70, 80
  ),
  nrow = 2,
  byrow = TRUE
)

rray_slice_axis(x, c(4, 1), axis = 2)
#      [,1] [,2]
# [1,]   40   10
# [2,]   80   50
```

It replaces the need for a padding helper when the selected axis is late in a
high-dimensional array.

`i` is always an ordinary subscript. Its dimensions and class do not select a
different behavior. A shaped object that is not a valid ordinary subscript is
an error. Use `rray_slice_locations()` to supply one location per lane.

## `rray_slice_locations()`

```r
rray_slice_locations(x, locations, axis)
rray_slice_assign_locations(x, locations, axis, value)
```

`rray_slice_locations()` selects one location on `axis` for each output
position. `locations` must be a bare integer array with the same dimensionality
as `x`.

- On `axis`, the dimension of `x` is the range of valid positions and the
  dimension of `locations` becomes the output dimension.
- On every other axis, the dimension of `locations` must be one or equal the
  matching dimension of `x`.
- A dimension of one in `locations` broadcasts to the matching dimension of
  `x`.
- `x` never grows to match `locations`. Output dimensions outside `axis` are
  always identical to those of `x`.
- Every element of `locations` must be a positive, one-based integer no greater
  than the dimension of `x` on `axis`.
- Zero and negative locations are errors. Each element chooses one position,
  so complement selection has no meaning.
- Missing locations produce missing values when reading and are errors when
  assigning.

Directional broadcasting keeps the phrase "slice one axis" true and makes
assignment well-defined. Every selected output position maps back to an
existing element of `x`.

Using different locations for each row:

```r
locations <- rbind(
  c(4L, 1L),
  c(2L, 3L)
)

rray_slice_locations(x, locations, axis = 2)
#      [,1] [,2]
# [1,]   40   10
# [2,]   60   70
```

The calculation is:

```text
out[1, 1] = x[1, locations[1, 1]] = x[1, 4] = 40
out[1, 2] = x[1, locations[1, 2]] = x[1, 1] = 10
out[2, 1] = x[2, locations[2, 1]] = x[2, 2] = 60
out[2, 2] = x[2, locations[2, 2]] = x[2, 3] = 70
```

The same values can be used as an ordinary shared subscript, but that is an
explicitly different operation:

```r
rray_slice_axis(x, as.vector(locations), axis = 2)
#      [,1] [,2] [,3] [,4]
# [1,]   40   20   10   30
# [2,]   80   60   50   70
```

For assignment, `value` broadcasts to the location selection dimensions. The
returned array keeps the original dimensions of `x`. Repeated positions within
a lane are repeated assignment targets, with the final value in column-major
selection order winning.

### Broadcasting locations

A dimension of one in `locations` broadcasts across the corresponding lanes of
`x`.

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

dim(x)
# c(3, 4)

dim(locations)
# c(1, 2)

rray_slice_locations(x, locations, axis = 2)
#      [,1] [,2]
# [1,]   40   10
# [2,]   80   50
# [3,]  120   90
```

The one row of `locations` broadcasts across the three rows of `x`. A
`locations` array with dimensions `c(4, 2)` would be an error because its first
dimension cannot broadcast to the three rows of `x`.

### One-dimensional `x`

The two functions keep their distinct contracts even when `x` is
one-dimensional.

```r
x <- array(c(10, 20, 30, 40), 4)
locations <- array(c(4L, 1L), 2)

rray_slice_locations(x, locations, axis = 1)
# [1] 40 10
# dimensions: 2L

rray_slice_axis(x, -1, axis = 1)
# [1] 20 30 40
# dimensions: 3L
```

`rray_slice_locations()` requires positive integer locations.
`rray_slice_axis()` retains ordinary negative, logical, and named subscript
semantics.

### Assignment with `rray_locate_max()`

`rray_locate_max()` should return one-based locations local to `axis`. It must
preserve dimensionality, replacing the located axis with dimension one.

```r
x <- rbind(
  c(3, 9, 4),
  c(8, 2, 7),
  c(1, 5, 6)
)

locations <- rray_locate_max(x, axis = 2)

locations
#      [,1]
# [1,]    2
# [2,]    1
# [3,]    3

dim(locations)
# c(3, 1)
```

The result is directly accepted by `rray_slice_locations()`:

```r
rray_slice_locations(x, locations, axis = 2)
#      [,1]
# [1,]    9
# [2,]    8
# [3,]    6
```

A scalar assignment broadcasts to the three selected maxima:

```r
rray_slice_assign_locations(x, locations, axis = 2, value = 0)
#      [,1] [,2] [,3]
# [1,]    3    0    4
# [2,]    0    2    7
# [3,]    1    5    0
```

A value with the selection dimensions assigns a different value to each lane:

```r
value <- array(c(100, 200, 300), c(3, 1))

rray_slice_assign_locations(x, locations, axis = 2, value = value)
#      [,1] [,2] [,3]
# [1,]    3  100    4
# [2,]  200    2    7
# [3,]    1    5  300
```

If a lane has tied maxima, `rray_locate_max()` selects one according to its tie
rule, expected to be the first. Replacing every tied maximum uses a logical mask
instead:

```r
mask <- rray_equal(x, rray_max_along(x, axes = 2))
rray_extract_assign(x, mask, value = 0)
```

For a three-dimensional `x` with dimensions `c(2, 3, 4)`, locating along axis
2 returns dimensions `c(2, 1, 4)`. The assignment replaces one element in each
of the eight lanes and returns the original dimensions `c(2, 3, 4)`.

## Axis subscript rules

`rray_slice()` and `rray_slice_axis()` use the same subscript rules as
`vctrs::vec_as_location()`:

- A missing argument selects every location.
- `NULL`, `FALSE`, and `integer()` select no locations.
- Positive integers select locations in the supplied order. Duplicates are
  allowed.
- Zero is ignored.
- Negative integers select the complement. Positive and negative locations
  cannot be mixed, apart from zero. Missing and negative locations cannot be
  mixed.
- Doubles are accepted only when every non-missing value is a whole number that
  can be represented as an R integer.
- A logical subscript must have length one or equal the axis dimension. `TRUE`
  selects all, `FALSE` selects none, and a scalar `NA` expands to one unknown
  location per element of the axis.
- Character subscripts are matched exactly against that axis's names. The
  first match is used when source names are duplicated.
- Factors and other classed subscripts are errors. Factors are not silently
  interpreted through their integer codes.
- Out-of-bounds integers and unmatched names are errors. Slicing never extends
  an axis.

These rules are stricter than base R's logical recycling. A logical subscript
of length two does not silently recycle over an axis of dimension three.

Internally, every accepted subscript becomes zero-based locations. Preserve
three representations:

1. All locations.
2. An affine sequence with a start, size, and step.
3. A materialized integer location vector.

The affine form covers contiguous ranges, stepped ranges, and reverse order.
Users can write ordinary R sequences. There is no need for a public slice-range
object.

## Missing locations

Read functions allow missing locations. A missing location produces the
missing value for the storage type:

| Type | Missing output |
|---|---|
| logical | `NA` |
| integer | `NA_integer_` |
| double | `NA_real_` |
| complex | `NA_complex_` |
| character | `NA_character_` |
| raw | `as.raw(0)` |
| list | `NULL` |

This follows base R and vctrs, including the unavoidable raw and list behavior.

All assignment functions reject missing locations. Base R has special cases
where assignment through a missing subscript can be skipped or accepted for a
scalar value. Those cases are hard to explain and should not be copied.

## Names

Orthogonal slicing subsets names with the same normalized locations used for
the data.

- Selecting all of an axis preserves its names without copying them.
- Reordering or duplicating locations reorders or duplicates their names.
- A missing location gets an `NA` name.
- An empty selection gives `character()` if the source axis had names, and
  `NULL` otherwise.
- Names on the list of axis names stay attached to the same axes.

`rray_slice_axis()` follows the same rules. `rray_slice_locations()` drops
names on the selected axis because each lane can select different source
locations. Every other axis keeps the names from `x` because its dimension is
unchanged. Names on `locations` never supply output names.

Point and flat extraction drop all names. A source axis cannot be identified
with the one-dimensional output.

Every assignment function returns the original dimensions and names of the
normalized `x` unchanged.

## Assignment rules

All assignment functions are pure functions. They return a modified copy of
`x` and do not change `x` in place from R's point of view.

```r
out <- rray_slice_assign(x, 1, , value = 0L)
out <- rray_slice_assign_axis(x, 1, axis = 3, value = 0L)
```

Assignment follows these rules:

1. Normalize and fully validate the locations.
2. Cast `value` losslessly to the type of `x` with `rray_cast()`.
3. Broadcast the cast value to the dimensions of the selected result.
4. Copy `x`.
5. Write in column-major output order.

The operation cannot change the type, size, dimensions, or names of `x`.
Broadcasting can repeat a dimension of one, including a one-element value, but
it cannot recycle an arbitrary size. A zero-sized target accepts values that
can broadcast to its dimensions.

Repeated target locations are legal. The last value visited in column-major
selection order wins. The order must be documented and tested rather than left
as an implementation detail.

The cast and broadcast must be complete before writing. This handles cases
where `value` is `x`, and it ensures that an error cannot return a partially
modified result.

`NULL` is not an array and cannot be an assignment value. To assign `NULL` into
a list array, use a list array containing `NULL`.

## `rray_extract()`

```r
rray_extract(x, i)
rray_extract_assign(x, i, value)
```

`rray_extract()` always returns a one-dimensional array. The type and shape of
`i` determine whether it contains flat positions or coordinate points.

### Dispatch rule

- Any bare integer or double matrix is point indexing. It is never flattened
  into positions. It must have exactly one column per axis of `x`, otherwise it
  is an error. Double coordinates must be whole numbers.
- Any bare character matrix is named point indexing. It must also have exactly
  one column per axis of `x`.
- An integer or double vector, including a one-dimensional array, contains flat
  positions.
- A logical vector or one-dimensional logical array is a flat mask. A logical
  array with dimensions identical to `x` is also a flat mask.
- A logical matrix is therefore a mask only when its dimensions are identical
  to `x`. It is never point indexing.
- Character vectors are errors. Flat character positions have no consistent
  meaning for arrays with more than one axis.
- Numeric arrays with three or more dimensions are errors. Reshape them to a
  matrix to request points, or to one dimension to request flat positions.
- Factors, data frames, and other classed subscripts are errors.

The matrix rule is unconditional. If `i` is an integer matrix with the wrong
number of columns, it is an invalid point matrix rather than a flat subscript.
This makes the dispatch predictable from `i` alone.

### Flat positions

Flat extraction walks R's column-major storage order. Numeric `i` uses the
location rules against `rray_size(x)`, including negative complements, zero,
duplicates, and missing locations.

A logical array with dimensions identical to `x` is accepted as a mask. A
one-dimensional logical subscript must have length one or `rray_size(x)`.

### Coordinate points

Each row of a point matrix identifies one element. A point matrix has one
column per axis of `x` and one row per requested point.

Numeric points must be positive and in bounds. Zero and negative coordinates
are errors because complement selection has no useful meaning for a point.
Missing coordinates produce missing output for reads and are errors for
assignment.

Character points are matched exactly against the names of the corresponding
axis. Every axis must have names. Missing strings produce missing output for
reads. Empty strings and unmatched strings are errors.

A point result has dimensions `nrow(i)`. A zero-row matrix returns a
zero-length one-dimensional array. One row selects one element, but the result
is still a one-dimensional array of length one because rray4 always returns
arrays. This is the closest rray4 analogue to subset2 extraction.

### Shared result and assignment rules

For flat positions, the result dimension is the number of normalized
locations. For points, it is `nrow(i)`. Names are dropped in both cases.
`rray_extract_assign()` broadcasts `value` to this one-dimensional result.

There is no separate `rray_filter()` function. A logical mask is already clear
in `rray_extract()`, and a second name would not add a new addressing mode.

## Functions not proposed

Do not carry these parts of the original API forward:

- `rray_subset()`: `rray_slice()` is the clearer name for its operation.
- The original variadic `rray_extract(x, ...)`: flattening an orthogonal slice
  is composition, not a new addressing mode. Use `rray_slice()` followed by
  `rray_set_dimensions()` when it is genuinely needed. The proposed
  `rray_extract(x, i)` has exactly one subscript, whose shape determines whether
  it contains flat positions or coordinate points.
- `rray_yank()`: `rray_extract()` is the clearer name for flat extraction.
- `pad()`: `rray_slice_axis()` handles late axes, and dynamic splicing handles
  programmatic multi-axis slicing.
- `drop`: rray4 always returns arrays and never drops axes implicitly.
- `rray_take()`: it would be an alias for `rray_slice_axis()`.
- `rray_slice_along_axis()`, `rray_take_along_axis()`, and
  `rray_slice_by_lane()`: `rray_slice_locations()` names the strict location
  array contract directly.
- `rray_filter()`: it would be an alias for logical flat extraction.

Do not overload `rray_slice()` with point or flat modes based on the class or
shape of one subscript. The function's output shape should be clear from its
name and call structure.

## Implementation design

### Subscript normalization

Add `src/index.c`, `src/index.h`, and `src/decl/index-decl.h`. This layer owns:

- Converting user subscripts to zero-based locations.
- Exact logical size checks.
- Positive, negative, zero, missing, and bounds rules.
- Character matching against axis names.
- Detection of all and affine indices.
- Point matrix validation and conversion.
- Flat location validation.

Use `r_ssize` for subscript and output sizes. Axis dimensions and stored
locations remain integers because R's `dim` attribute is integer. Error before
creating a result whose selected axis would exceed `INT_MAX`.

The public behavior should match `vec_as_location()`, but the hot path should be
implemented in C rather than calling an R function once per axis.

### Orthogonal slice plan

Build one slice plan from normalized indices, source dimensions, and source
strides. The plan contains:

- Output dimensions.
- Source start offset.
- One index descriptor per axis.
- Source strides in R's column-major order.
- Whether every axis is affine.
- Whether any location is missing.

Use two execution paths:

1. When every axis is affine, use a strided plan with the selected step folded
   into each source stride. This handles whole axes, ranges, steps, and reverse
   order. Hold the source start offset in the slice plan and reuse the existing
   strided iterator coalescing for dimensions and strides.
2. When any axis is irregular, use an indexed iterator. Precompute source
   offsets for irregular axes. Walk the first axis in an inner run and update
   later-axis offsets only when their mixed-radix counters carry.

This keeps common contiguous slices fast without making arbitrary repeated or
reordered locations a special case in the public API.

### Location slice plan

Build a dedicated plan for `rray_slice_locations()`.

1. Validate that `locations` is a bare integer array with the same
   dimensionality as `x`.
2. Validate directional broadcasting on every non-selected axis.
3. Copy the dimensions of `x` and replace the selected dimension with the
   matching dimension of `locations`.
4. Give `x` its ordinary source stride on every non-selected axis and stride
   zero on the selected axis.
5. Give `locations` its ordinary stride on the selected axis. On every other
   axis, use stride zero when its dimension is one and its ordinary stride
   otherwise.
6. Walk the output point space once. At every point, read the one-based
   location and add `(location - 1) * x_axis_stride` to the base location in
   `x`.

The existing two-location strided iterator is a good fit. One location tracks
the base position in `x`, while the other tracks the possibly broadcast
position in `locations`. Read and assignment cores then differ only in whether
they copy from or write to the computed source location.

Missing `locations` values use a dedicated path that writes the storage type's
missing value during reads. Assignment validates that no missing location
exists before copying `x`. Duplicate computed locations are visited in output
column-major order, so the existing last-write-wins rule applies.

### Typed cores

Follow the existing shell and typed-core layout. The shell validates, builds the
plan, chooses dimensions and names, and dispatches on storage type. Atomic cores
write through data pointers. Character and list cores use the write barrier.

The assignment shell casts and broadcasts before it copies `x`. Read and
assignment use the same plan, so location order cannot drift between them.

The point path of `rray_extract()` computes a flat source offset for each row:

```text
offset = sum((point[axis] - 1) * stride[axis])
```

The flat path already has source offsets after normalization. Both paths share
the typed extraction and assignment cores. `rray_slice_locations()` uses a
separate iterator because `locations` can broadcast on the non-selected axes.

### Files

Keep each public family together with its assignment form:

- `R/slice.R`, `src/slice.c`, `src/slice.h`, `src/decl/slice-decl.h`
- `R/slice-axis.R`, `src/slice-axis.c`, `src/slice-axis.h`,
  `src/decl/slice-axis-decl.h`
- `R/slice-locations.R`, `src/slice-locations.c`, `src/slice-locations.h`,
  `src/decl/slice-locations-decl.h`
- `R/extract.R`, `src/extract.c`, `src/extract.h`,
  `src/decl/extract-decl.h`
- Shared `src/index.c`, `src/index.h`, `src/decl/index-decl.h`
- An indexed iterator header if the irregular slice path is large enough to
  stand alone

`rray_slice_axis()` can call the same internal slice implementation as
`rray_slice()`. `rray_slice_locations()` has its own FFI wrapper and location
plan.

## Test plan

### Ordinary slicing behavior

- All seven native storage types.
- Bare vectors normalize to one-dimensional arrays.
- Zero dimensions and empty selections.
- One through several axes, plus the maximum supported dimensionality.
- Positive, negative, zero, logical, character, missing, and `NULL`
  subscripts.
- Duplicates, reverse order, and stepped sequences.
- Out-of-bounds, mixed-sign, fractional, recycled logical, classed subscript,
  and too-many-axis errors.
- Named and unnamed axes, including reordered, duplicated, missing, and empty
  names.
- Long total sizes where practical, with axis dimensions still limited to
  integers.

### Slice axis

- Integer, double, logical, and character subscripts follow the ordinary
  subscript rules.
- Negative integers retain complement semantics.
- The dimensions of `i` never activate location slicing.
- Multidimensional objects that are not valid ordinary subscripts error.
- Selected-axis names follow the normalized subscript.

### Slice locations

- `locations` must be a bare integer array with the same dimensionality as
  `x`.
- One-dimensional `x` still uses strict location semantics.
- Zero and negative locations error.
- Exact non-axis dimensions and dimensions of one both work.
- A `locations` dimension greater than one cannot expand a dimension-one axis
  of `x`.
- Output dimensions replace only the selected dimension.
- The selected axis loses names and every other axis keeps its names.
- Hand-built location arrays select and assign one position per lane.
- Add direct integration tests for `rray_locate_min()` and
  `rray_locate_max()` when they land.

### Reference properties

For fully specified orthogonal indices:

```r
rray_slice(x, i, j, k)
identical to
x[i, j, k, drop = FALSE]
```

Compare values, dimensions, and names. Use only the shared semantic subset when
testing against base R because rray4 intentionally rejects logical recycling
and implicit dropping.

For points, compare against base R's matrix subscript. For flat positions,
compare against `x[i]` after wrapping the result as a one-dimensional array.
For `rray_slice_locations()`, build a small R loop oracle that walks every
output point. Cover exact non-axis dimensions and broadcasting dimensions of
one.

### Assignment

- Lossless casts succeed and lossy casts fail.
- Scalar and per-axis broadcasting.
- Arbitrary recycling fails.
- Empty targets.
- Missing targets fail before copying.
- Duplicate targets use last-write-wins order.
- `value` equal to `x` works.
- Type, size, dimensions, and names of `x` are unchanged.
- Every assignment result agrees with a read of the same selected locations.

### Required checks

After each C change:

1. Run the explicit protection review required by `AGENTS.md`.
2. Run `clang-format -i src/*.c src/*.h`.
3. Run `air format .`.
4. Run focused tests, then all tests.
5. Run `devtools::check()` for the final pull request in the family.

## Delivery order

### 1. Orthogonal slicing

Implement axis subscript normalization, `rray_slice()`,
`rray_slice_assign()`, `rray_slice_axis()`, and
`rray_slice_assign_axis()`. Then implement `rray_slice_locations()` and
`rray_slice_assign_locations()` with the strict location array contract.

This delivers the common API and proves the orthogonal and location iterators,
names, assignment rules, and the location array contract used by
`rray_locate_min()` and `rray_locate_max()`.

### 2. Extraction

Implement `rray_extract()` and `rray_extract_assign()`. The wrapper dispatches
to point or flat index normalization, then both paths reuse the typed copy
helpers from the first step.

### 3. Update the main implementation plan

Once this design is accepted, replace section 5.6 of
`plans/implementation.md` with the API and delivery order above. Keep this file
as the detailed semantic reference.

## Research notes

### Base R

Base R supports flat vector indexing, per-axis indexing, and point indexing by
a numeric or character matrix. Empty per-axis subscripts select all locations,
and `drop` controls dimensionality. Logical per-axis subscripts recycle. Point
matrix rows containing zero are ignored, while rows containing a missing value
produce missing output. Replacement promotes the left-hand type when needed,
and repeated locations are assigned sequentially.

Useful sources:

- [Extract or Replace Parts of an Object](https://stat.ethz.ch/R-manual/R-patched/library/base/html/Extract.html)
- [R source: `subset.c`](https://github.com/wch/r-source/blob/trunk/src/main/subset.c)
- [R source: `subscript.c`](https://github.com/wch/r-source/blob/trunk/src/main/subscript.c)
- [R source: `subassign.c`](https://github.com/wch/r-source/blob/trunk/src/main/subassign.c)

The local R source confirms that point matrices are converted to flat
column-major locations before ordinary vector extraction. Its array assignment
loops also establish sequential last-write-wins behavior.

### vctrs

vctrs separates subscript normalization from slicing. `vec_as_location()`
converts integer, logical, and character subscripts to positive locations. It
uses strict logical sizes, inverts negative locations, removes zero, and offers
an explicit missing policy. `vec_assign()` casts the right-hand side to the
left-hand type and requires a scalar or target-sized value.

Useful sources:

- [`vec_as_location()`](https://vctrs.r-lib.org/reference/vec_as_location.html)
- [`vec_slice()` and `vec_assign()`](https://vctrs.r-lib.org/reference/vec_slice.html)

rray4 should use the same location language, but replace vector recycling with
array broadcasting for assignment values.

### NumPy

NumPy distinguishes basic slicing, advanced integer or Boolean indexing, flat
selection, `take()` along one axis, and `take_along_axis()` with indices that
vary per lane. Basic integer indices drop an axis. Multiple advanced integer
indices are broadcast and paired, rather than forming a Cartesian product.
`ix_()` constructs an open mesh when a Cartesian product is wanted.

Useful sources:

- [Indexing on ndarrays](https://numpy.org/doc/stable/user/basics.indexing)
- [`numpy.take()`](https://numpy.org/doc/stable/reference/generated/numpy.take.html)
- [`numpy.take_along_axis()`](https://numpy.org/doc/stable/reference/generated/numpy.take_along_axis.html)
- [`numpy.put_along_axis()`](https://numpy.org/doc/stable/reference/generated/numpy.put_along_axis.html)
- [`numpy.ix_()`](https://numpy.org/doc/stable/reference/generated/numpy.ix_.html)

The useful idea to copy is the distinction between one shared axis index and an
index that varies by lane. The advanced-indexing shape switch should not be
copied.

### xtensor

xtensor separates strided views from dynamic views. Strided views support all,
ranges, steps, and scalar positions efficiently. Arbitrary keep and drop lists
require a more general dynamic view. `index_view()` and `filter()` return flat
one-dimensional views from coordinate points and masks.

Useful sources:

- [xtensor views](https://xtensor.readthedocs.io/en/latest/view.html)
- [`xindex_view`](https://xtensor.readthedocs.io/en/latest/api/xindex_view.html)

The original rray used exactly this split, choosing a strided view for
contiguous indices and a dynamic view for arbitrary locations. rray4 should
keep the optimization in its iterator design without exposing two public
slicing APIs.

### Original rray

The relevant local files are:

- `/Users/davis/files/r/packages/rray/R/subset.R`
- `/Users/davis/files/r/packages/rray/R/extract.R`
- `/Users/davis/files/r/packages/rray/R/yank.R`
- `/Users/davis/files/r/packages/rray/R/slice.R`
- `/Users/davis/files/r/packages/rray/src/subset-tools.cpp`
- `/Users/davis/files/r/packages/rray/src/subset.cpp`
- `/Users/davis/files/r/packages/rray/src/yank.cpp`

The strongest parts to retain are dimension preservation, strict logical
sizes, exact name matching, assignment casting toward `x`, assignment
broadcasting, and the strided fast path. The main change is to align names with
addressing modes and remove the flattened version of an orthogonal slice from
the core family.
