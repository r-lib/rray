# Array slicing and extraction plan

## Recommendation

Use ordinary R subscripts for slicing and flat extraction:

```r
rray_slice(x, ...)
rray_slice_assign(x, ..., value)

rray_slice_axis(x, i, axis)
rray_slice_assign_axis(x, i, axis, value)

rray_extract(x, i)
rray_extract_assign(x, i, value)
```

`rray_slice()` performs dimension-preserving orthogonal selection across one
or more axes. `rray_slice_axis()` performs the same operation on one axis held
in a variable. `rray_extract()` treats `x` as its flat column-major storage and
always returns a one-dimensional array.

Keep subscripts separate from integer coordinate arrays:

| Property | Subscript | Coordinate array |
|---|---|---|
| Used by | `rray_slice()`, `rray_slice_axis()`, `rray_extract()` | `rray_index()`, `rray_index_axis()` |
| Meaning | A set of positions | One source coordinate per output point |
| Accepted forms | Integer, double, logical, character where defined, missing, `NULL` | Bare integer vector or array |
| Negative values | Select the complement | Error |
| Zero | Ignored | Error |
| Shape | Does not connect positions across axes | Connects coordinates pointwise through broadcasting |

The function name determines the contract. The dimensions of `i` never turn a
subscript into a coordinate array.

Do not add replacement functions. Every `_assign()` function returns a
modified copy of `x`. There are no `[` or `[[` methods because rray4 provides
functions rather than an array class.

## The indexing family

The complete family has five read operations:

| Operation | Function | Result shape |
|---|---|---|
| Orthogonal subscripts | `rray_slice(x, ...)` | One output axis per source axis |
| One-axis subscript | `rray_slice_axis(x, i, axis)` | Source dimensions with one axis replaced |
| One-axis coordinate array | `rray_index_axis(x, i, axis)` | Source dimensions with one axis replaced |
| Full coordinate arrays | `rray_index(x, ...)` | Common coordinate dimensions |
| Flat subscript | `rray_extract(x, i)` | One dimensional |

`rray_index()` is the general coordinate operation. Every other operation can
be described by constructing one explicit coordinate array for every source
axis and then calling `rray_index()`. The public wrappers remain important
because they accept different inputs, guarantee more specific shapes, retain
more names, and can use faster implementations.

See `plans/index.md` for strict coordinate arrays and the complete lowering of
each operation to `rray_index()`.

## Orthogonal slicing

Each subscript controls one source axis. Subscripts are normalized separately,
then their locations form a Cartesian product.

```r
x <- array(1:24, c(2, 3, 4))

rray_slice(x, c(2, 1), c(3, 1), 2)
# dimensions: c(2, 2, 1)

rray_slice_axis(x, c(3, 1), axis = 2)
# dimensions: c(2, 2, 4)
```

This is close to base R's `x[..., drop = FALSE]`. Slicing never drops axes.

The same Cartesian product can be expressed through full coordinate indexing.
After normalizing the three subscripts to positive locations, shape them as an
open mesh:

```text
axis 1 coordinates: (I, 1, 1)
axis 2 coordinates: (1, J, 1)
axis 3 coordinates: (1, 1, K)
common dimensions:  (I, J, K)
```

The shaped coordinate arrays broadcast to the slicing result dimensions.

## `rray_slice()`

```r
rray_slice(x, ...)
rray_slice_assign(x, ..., value)
```

Each element of `...` applies to the matching source axis. A missing argument
selects the whole axis. Omitted trailing arguments also select whole axes.

```r
rray_slice(x, 1)
# dimensions: c(1, 3, 4)

rray_slice(x, , 1)
# dimensions: c(2, 1, 4)

rray_slice(x)
# dimensions: c(2, 3, 4)
```

More subscripts than the dimensionality of `x` are an error. Subscripts in
`...` must be unnamed because selection is positional.

The dots support dynamic splicing when a caller already has one subscript per
source axis:

```r
indices <- list(1:2, c(3, 1))
rray_slice(x, !!!indices)
```

`value` follows `...` in `rray_slice_assign()`, so it must be supplied by
name.

## `rray_slice_axis()`

```r
rray_slice_axis(x, i, axis)
rray_slice_assign_axis(x, i, axis, value)
```

`rray_slice_axis()` has exactly the same subscript semantics as supplying `i`
at the matching position in `rray_slice()`. The same normalized subscript is
used for every lane through the selected axis. Only the selected axis
dimension can change.

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

The function replaces the need to pad a call with missing arguments when
`axis` is held in a variable.

`i` is always an ordinary subscript. Its dimensions do not affect the result
dimensions. A multidimensional object that is not a valid ordinary subscript
is an error. Use `rray_index_axis()` when an integer array supplies a different
location for different lanes.

### Connection to full coordinate indexing

Let `x` have dimensions `(A, B, C)`, let `axis = 2`, and let normalized `i`
have length `J`. The slice can be expressed with these full coordinates:

```r
axis1 <- array(seq_len(A), c(A, 1L, 1L))
axis2 <- array(normalize(i), c(1L, J, 1L))
axis3 <- array(seq_len(C), c(1L, 1L, C))

rray_index(x, axis1, axis2, axis3)
```

The common dimensions are `(A, J, C)`, exactly the dimensions of
`rray_slice_axis(x, i, axis = 2)`. The public slice function additionally owns
subscript normalization and selected-axis name selection.

## `rray_extract()`

```r
rray_extract(x, i)
rray_extract_assign(x, i, value)
```

`rray_extract()` performs flat indexing only. It treats `x` as its
column-major storage vector and always returns a one-dimensional array.

```r
x <- array(1:24, c(2, 3, 4))

rray_extract(x, c(1, 4, 24))
rray_extract(x, x %% 5 == 0)
```

The shape of `i` does not select another addressing mode:

- Integer and double vectors or arrays contain flat positions.
- Logical vectors or arrays are flat masks.
- Character subscripts are errors because flat character positions have no
  stable meaning for a multidimensional array.
- Factors, data frames, and other classed subscripts are errors.

Numeric `i` follows the ordinary location rules against `rray_size(x)`,
including negative complements, zero, duplicates, and missing locations.
Double locations must be whole numbers representable as R integers.

A logical `i` must have size one or `rray_size(x)`. A logical array with the
same dimensions as `x` is therefore accepted naturally. Arbitrary logical
recycling is an error.

For numeric input, the result dimension is the number of normalized
locations. For logical input, it is the number of selected or missing
locations. The result is one dimensional even when `i` is an array.

### Coordinate point example

Point matrices no longer select a separate `rray_extract()` mode. Their
columns are already fully specified coordinate arrays:

```r
points <- rbind(
  c(1, 1, 1),
  c(2, 3, 4),
  c(1, 2, 3)
)

rray_index(x, points[, 1], points[, 2], points[, 3])
# dimensions: 3L
# values: c(1L, 24L, 15L)
```

Rows remain paired because every column has the same one-dimensional shape.
This is base R's numeric matrix indexing expressed directly through the
general coordinate operation.

### Connection to full coordinate indexing

A normalized flat position can be unravelled into one coordinate for every
source axis in R's column-major order:

```text
flat locations
  -> unravel to one location vector per source axis
  -> rray_index() over every source axis
```

Every unravelled coordinate vector has the one-dimensional result size, so
the common coordinate dimensions are also one dimensional. This gives the
same values, missing behavior, and duplicate order as `rray_extract()`.

The implementation should retain a direct flat path. Materializing one
coordinate vector per source axis would add work without changing the public
result.

## Subscript rules

`rray_slice()`, `rray_slice_axis()`, and numeric or logical flat extraction use
the same normalization rules as `vctrs::vec_as_location()` where those rules
apply:

- A missing slice argument selects every location.
- `NULL`, `FALSE`, and `integer()` select no locations.
- Positive integers select locations in the supplied order. Duplicates are
  allowed.
- Zero is ignored.
- Negative integers select the complement. Positive and negative locations
  cannot be mixed, apart from zero. Missing and negative locations cannot be
  mixed.
- Doubles are accepted only when every non-missing value is a whole number
  representable as an R integer.
- A logical subscript must have length one or equal the indexed size. `TRUE`
  selects all, `FALSE` selects none, and scalar `NA` expands to one missing
  location per indexed element.
- Character slice subscripts match exactly against names on the selected
  source axis. The first match is used when source names are duplicated.
- Factors and other classed subscripts are errors.
- Out-of-bounds integers and unmatched names are errors. Slicing and
  extraction never extend the source.

Logical recycling is deliberately stricter than base R. A logical subscript
of length two does not silently recycle over an axis of dimension three.

Internally, accepted subscripts normalize to zero-based locations. Preserve
three representations:

1. Every location.
2. An affine sequence with a start, size, and step.
3. A materialized integer location vector.

The affine form covers contiguous ranges, stepped ranges, and reverse order.
Users can write ordinary R sequences, so no public slice-range object is
needed.

## Missing locations

Read functions allow missing normalized locations. A missing location produces
the missing value for the storage type:

| Type | Missing output |
|---|---|
| logical | `NA` |
| integer | `NA_integer_` |
| double | `NA_real_` |
| complex | `NA_complex_` |
| character | `NA_character_` |
| raw | `as.raw(0)` |
| list | `NULL` |

This follows base R and vctrs, including the unavoidable raw and list
behavior.

All assignment functions reject missing locations. Base R has special cases
for assignment through missing subscripts, but they do not provide a clear
general contract.

## Names

Orthogonal slicing subsets names with the same normalized locations used for
the data:

- Selecting all of an axis preserves its names without copying them.
- Reordering or duplicating locations reorders or duplicates their names.
- A missing location gets an `NA` name.
- An empty selection gives `character()` if the source axis had names, and
  `NULL` otherwise.
- Names on the list of axis names stay attached to the same axes.

`rray_slice_axis()` follows the same rules. Because one location sequence is
used for every lane, each output position on the selected axis has one clear
source name.

`rray_extract()` drops all names. A flat one-dimensional result cannot retain
a source-axis identity.

The coordinate family has different guarantees:

- `rray_index_axis()` preserves source names on unselected axes and drops
  names on the selected axis.
- `rray_index()` drops all names because no output axis necessarily
  corresponds to a source axis.

Every assignment function returns the original dimensions and names of `x`
unchanged.

## Assignment rules

All assignment functions are pure functions. They return a modified copy of
`x` and do not modify `x` from R's point of view.

```r
out <- rray_slice_assign(x, 1, , value = 0L)
out <- rray_slice_assign_axis(x, 1, axis = 3, value = 0L)
```

Assignment follows these steps:

1. Normalize and fully validate the target locations.
2. Cast `value` losslessly to the type of `x` with `rray_cast()`.
3. Broadcast the cast value to the selected result dimensions.
4. Copy `x`.
5. Write in column-major selection order.

The operation cannot change the type, size, dimensions, or names of `x`.
Broadcasting can repeat a dimension of one, including a one-element value, but
cannot recycle an arbitrary size. A zero-sized target accepts values that can
broadcast to its dimensions.

Repeated target locations are legal. The final value visited in column-major
selection order wins. Casting and broadcasting finish before copying so an
error cannot return a partially modified result and `value` may safely be
`x`.

`NULL` is not an array and cannot be an assignment value. To assign `NULL` into
a list array, use a list array containing `NULL`.

## Functions not proposed

Do not carry these parts of the original API forward:

- `rray_subset()`: `rray_slice()` names the orthogonal operation directly.
- The original variadic `rray_extract(x, ...)`: orthogonal selection belongs
  to `rray_slice()`. The new `rray_extract(x, i)` is flat-only.
- Point-matrix dispatch in `rray_extract()`: use one matrix column per argument
  to `rray_index()`.
- `rray_yank()`: `rray_extract()` is the clearer name for flat extraction.
- `pad()`: missing arguments handle fixed calls and `rray_slice_axis()` handles
  an axis held in a variable.
- `drop`: rray4 always returns arrays and never drops axes implicitly.
- `rray_take()`, `rray_take_along_axis()`, `rray_shuffle_axis()`, and
  `rray_slice_by_lane()`: `rray_slice_axis()` and `rray_index_axis()` provide
  the two distinct one-axis contracts.
- `rray_filter()`: it would duplicate logical flat extraction.

Do not overload `rray_slice()` or `rray_extract()` based on the dimensions of a
subscript. The function name must determine the result model.

## Implementation design

### Subscript normalization

Add `src/subscript.c`, `src/subscript.h`, and
`src/decl/subscript-decl.h`. This layer owns:

- Converting user subscripts to zero-based locations.
- Exact logical size checks.
- Positive, negative, zero, missing, and bounds rules.
- Character matching against axis names for slicing.
- Detection of all and affine indices.
- Flat location validation.

Use `r_ssize` for subscript and output sizes. Axis dimensions and stored
locations remain integers because R's `dim` attribute is integer. Error before
creating a result whose selected axis would exceed `INT_MAX`.

The public behavior should match `vec_as_location()`, but the hot path should
be implemented in C rather than calling an R function once per axis.

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
   into each source stride. Hold the source start offset in the plan and reuse
   the existing strided iterator coalescing.
2. When an axis is irregular, use an indexed iterator. Precompute source
   offsets for irregular axes. Walk the first axis in an inner run and update
   later-axis offsets when their mixed-radix counters carry.

This keeps contiguous slices fast without changing the public contract for
repeated or reordered locations.

### Flat extraction plan

Normalize `i` against `rray_size(x)` and convert each positive location to a
zero-based flat source offset. The dimensions of `i` do not survive. Logical
masks first become the corresponding flat locations.

The flat path shares typed copy and assignment cores with slicing but does not
build per-axis coordinate arrays.

### Typed cores

The shell validates, builds the plan, chooses dimensions and names, and
dispatches on storage type. Atomic cores write through data pointers.
Character and list cores use the write barrier.

The assignment shell casts and broadcasts before copying `x`. Read and
assignment use the same plan so target order cannot drift.

### Files

Keep each public family with its assignment form:

- `R/slice.R`, `src/slice.c`, `src/slice.h`, `src/decl/slice-decl.h`
- `R/slice-axis.R`, `src/slice-axis.c`, `src/slice-axis.h`,
  `src/decl/slice-axis-decl.h`
- `R/extract.R`, `src/extract.c`, `src/extract.h`,
  `src/decl/extract-decl.h`
- Shared `src/subscript.c`, `src/subscript.h`,
  `src/decl/subscript-decl.h`

`rray_slice_axis()` can call the same internal implementation as
`rray_slice()`.

## Test plan

### Ordinary slicing

- All seven native storage types.
- Bare vectors normalize to one-dimensional arrays.
- Zero dimensions and empty selections.
- One through several axes, including the maximum supported dimensionality.
- Positive, negative, zero, logical, character, missing, and `NULL`
  subscripts.
- Duplicates, reverse order, and stepped sequences.
- Out-of-bounds, mixed-sign, fractional, recycled logical, classed subscript,
  and too-many-axis errors.
- Named and unnamed axes, including reordered, duplicated, missing, and empty
  names.

### Slice axis

- Integer, double, logical, and character inputs follow ordinary subscript
  rules.
- Negative integers retain complement semantics.
- The dimensions of `i` never activate coordinate indexing.
- Multidimensional objects that are not valid ordinary subscripts error.
- Only the selected axis dimension changes.
- Selected-axis names follow the normalized subscript.

### Flat extraction

- Integer and double vectors and arrays are always flat positions.
- Logical vectors and arrays are always flat masks.
- A logical input must have size one or `rray_size(x)`.
- Numeric matrix input does not become point indexing.
- Character and classed inputs error.
- Results are always one dimensional and unnamed.
- Column-major order, duplicates, complements, zero, missing, and empty inputs.

### Reference properties

For fully specified orthogonal subscripts:

```r
rray_slice(x, i, j, k)
```

is identical to:

```r
x[i, j, k, drop = FALSE]
```

on the shared semantic subset. Compare values, dimensions, and names. rray4
intentionally rejects logical recycling and implicit dropping.

After subscript normalization, compare `rray_slice()` and
`rray_slice_axis()` with `rray_index()` over explicit open-mesh coordinates.
Compare names according to the stronger slice guarantees.

For flat positions, unravel normalized positions and compare with
`rray_index()` over one coordinate vector per source axis. Cover missing and
duplicate positions and column-major order.

### Assignment

- Lossless casts succeed and lossy casts fail.
- Scalar and per-axis broadcasting.
- Arbitrary recycling fails.
- Empty targets.
- Missing targets fail before copying.
- Duplicate targets use final-write-wins order.
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

Implement subscript normalization, `rray_slice()`, `rray_slice_assign()`,
`rray_slice_axis()`, and `rray_slice_assign_axis()`.

### 2. Flat extraction

Implement `rray_extract()` and `rray_extract_assign()` on the shared subscript
normalization and typed copy cores.

### 3. Coordinate indexing

Implement the strict coordinate family from `plans/index.md`, then add the
reference properties that lower slicing and extraction to fully specified
coordinates.

### 4. Main implementation plan

Update the corresponding section of `plans/implementation.md`. Keep this file
as the source of truth for ordinary subscripts and flat extraction, and
`plans/index.md` as the source of truth for integer coordinate arrays.

## Research notes

### Base R

Base R supports flat vector indexing, per-axis indexing, and point indexing by
a numeric or character matrix. It drops dimensions by default and recycles
logical per-axis subscripts. rray4 keeps the useful subscript forms but assigns
each output model to an explicit function and never drops axes implicitly.

Useful sources:

- [Extract or Replace Parts of an Object](https://stat.ethz.ch/R-manual/R-patched/library/base/html/Extract.html)
- [R source: `subset.c`](https://github.com/wch/r-source/blob/trunk/src/main/subset.c)
- [R source: `subscript.c`](https://github.com/wch/r-source/blob/trunk/src/main/subscript.c)
- [R source: `subassign.c`](https://github.com/wch/r-source/blob/trunk/src/main/subassign.c)

### vctrs

vctrs separates subscript normalization from slicing. `vec_as_location()`
converts integer, logical, and character subscripts to positive locations. It
uses strict logical sizes, inverts negative locations, removes zero, and has an
explicit missing policy.

- [`vec_as_location()`](https://vctrs.r-lib.org/reference/vec_as_location.html)

### Python Array API standard

The Array API standard separates one-axis `take()` from fully specified
integer-array indexing. `take()` uses a one-dimensional integer input and
changes only the selected axis. Its shape model matches
`rray_slice_axis()` after an R subscript is normalized.

- [`take()`](https://data-apis.org/array-api/2024.12/API_specification/generated/array_api.take.html)
- [Integer array indexing](https://data-apis.org/array-api/2024.12/API_specification/indexing.html#integer-array-indexing)
