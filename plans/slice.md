# Array slicing and extraction plan

## Recommendation

Use ordinary R subscripts for slicing and extraction:

```r
rray_slice(x, ...)
rray_slice_assign(x, ..., value)

rray_slice_axis(x, i, ..., axis)
rray_slice_assign_axis(x, i, ..., axis, value)

rray_slice_rows(x, i)
rray_slice_assign_rows(x, i, value)

rray_slice_columns(x, i)
rray_slice_assign_columns(x, i, value)

rray_extract(x, i)
rray_extract_assign(x, i, value)
```

`rray_slice()` performs dimension-preserving orthogonal selection across one
or more axes. It requires exactly one subscript per source axis, so `TRUE`
selects a whole axis. `rray_slice_axis()` performs the same operation on one
axis held in a variable, and `rray_slice_rows()` and `rray_slice_columns()`
are its first and second axis shortcuts. `rray_extract()` accepts flat
positions or coordinate points and always returns a one-dimensional array.

Keep subscripts separate from integer coordinate arrays:

| Property | Subscript | Coordinate array |
|---|---|---|
| Used by | `rray_slice()`, `rray_slice_axis()`, `rray_extract()` | `rray_index()`, `rray_index_axis()` |
| Meaning | A set of positions | One source coordinate per output point |
| Accepted forms | Integer, double, logical, character where defined, `NULL` | Bare integer vector or array |
| Negative values | Select the complement | Error |
| Zero | Ignored | Error |
| Shape | Does not connect positions across axes | Connects coordinates pointwise through broadcasting |

The function name determines the contract. The dimensions of `i` never turn a
slice subscript into a coordinate array. `rray_extract()` has a separate,
explicit matrix rule for coordinate points.

Do not add replacement functions. Every `_assign()` function returns a
modified copy of `x`. There are no `[` or `[[` methods because rray4 provides
functions rather than an array class.

## The indexing family

The complete family has five read operations:

| Operation | Function | Result shape |
|---|---|---|
| Orthogonal subscripts | `rray_slice(x, ...)` | One output axis per source axis |
| One-axis subscript | `rray_slice_axis(x, i, ..., axis)` | Source dimensions with one axis replaced |
| First or second axis subscript | `rray_slice_rows(x, i)`, `rray_slice_columns(x, i)` | Source dimensions with one axis replaced |
| One-axis coordinate array | `rray_index_axis(x, i, ..., axis)` | Source dimensions with one axis replaced |
| Full coordinate arrays | `rray_index(x, ...)` | Common coordinate dimensions |
| Flat subscript or point matrix | `rray_extract(x, i)` | One dimensional |

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

Each element of `...` applies to the matching source axis. `...` must contain
exactly one subscript per source axis. `TRUE` selects a whole axis.

```r
rray_slice(x, 1, TRUE, TRUE)
# dimensions: c(1, 3, 4)

rray_slice(x, TRUE, 1, TRUE)
# dimensions: c(2, 1, 4)

rray_slice(x, TRUE, TRUE, TRUE)
# dimensions: c(2, 3, 4)
```

Empty arguments are an error. Base R writes a whole axis as a gap between two
commas, which is easy to miscount and invisible on screen, so `rray_slice()`
asks for a value instead. The error should point at `TRUE` and at
`rray_slice_axis()`.

```r
rray_slice(x, , , 1:2)
# Error: `...` must not contain empty arguments.
# Use `TRUE` to select a whole axis, or `rray_slice_axis()` for a single axis.
```

Too few and too many subscripts are the same error. Subscripts in `...` must
be unnamed because selection is positional.

Because every argument is an ordinary value, the dots are plain `list2()` dots
and splicing needs no special handling:

```r
indices <- rep(list(TRUE), rray_dimensionality(x))
indices[[3]] <- c(3, 1)
rray_slice(x, !!!indices)
```

`value` follows `...` in `rray_slice_assign()`, so it must be supplied by
name.

## `rray_slice_axis()`

```r
rray_slice_axis(x, i, ..., axis)
rray_slice_assign_axis(x, i, ..., axis, value)
```

The dots are empty and must stay empty. They force `axis` and `value` to be
supplied by name, so a call always says which axis it means and never reads as
a second subscript. This matches `rray_rep()` and `rray_split()`.

```r
rray_slice_axis(x, c(3, 1), axis = 2)
rray_slice_assign_axis(x, c(3, 1), axis = 2, value = 0L)
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

The function replaces the need to pad a call with `TRUE` when `axis` is held
in a variable.

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

## `rray_slice_rows()` and `rray_slice_columns()`

```r
rray_slice_rows(x, i)
rray_slice_assign_rows(x, i, value)

rray_slice_columns(x, i)
rray_slice_assign_columns(x, i, value)
```

These are the first and second axis shortcuts for `rray_slice_axis()`. They
exist because those two axes cover most interactive slicing:

```r
rray_slice_rows(x, i)    # rray_slice_axis(x, i, axis = 1)
rray_slice_columns(x, i) # rray_slice_axis(x, i, axis = 2)
```

They are pure sugar and add no subscript rules of their own.
`rray_slice_columns()` requires a dimensionality of at least two. The names
match the existing `rray_row_names()` and `rray_column_names()`.

They take no dots. The axis is in the name, so there is nothing to
disambiguate and `value` can stay positional.

## `rray_extract()`

```r
rray_extract(x, i)
rray_extract_assign(x, i, value)
```

`rray_extract()` accepts flat positions or coordinate points and always returns
a one-dimensional array. The type and shape of `i` determine which form it
contains.

```r
x <- array(1:24, c(2, 3, 4))

rray_extract(x, c(1, 4, 24))
rray_extract(x, x %% 5 == 0)
```

### Dispatch rule

- A bare integer or double matrix contains coordinate points. It must have one
  column per source axis. Double coordinates must be whole numbers.
- A bare character matrix contains named coordinate points. It must also have
  one column per source axis.
- An integer or double vector, including a one-dimensional array, contains flat
  positions.
- A logical vector or one-dimensional logical array is a flat mask. A logical
  array with dimensions identical to `x` is also a flat mask.
- A logical matrix is a mask only when its dimensions are identical to `x`. It
  is never a point matrix.
- Character vectors are errors. Flat character positions have no stable
  meaning for multidimensional arrays.
- Numeric arrays with three or more dimensions are errors. Reshape them to a
  matrix for coordinate points or to one dimension for flat positions.
- Factors, data frames, and other classed subscripts are errors.

The matrix rule is unconditional. An integer matrix with the wrong number of
columns is an invalid point matrix, not a flat subscript. This makes dispatch
predictable from `i` alone.

### Flat positions

Flat extraction treats `x` as its column-major storage vector. Numeric `i`
follows ordinary location rules against `rray_size(x)`, including negative
complements, zero, duplicates, and missing locations. Double locations must be
whole numbers representable as R integers.

A logical `i` must have length one or `rray_size(x)`. Arbitrary logical
recycling is an error.

### Coordinate points

Each matrix row identifies one element. A point matrix has one column per
source axis and one row per requested point.

Numeric coordinates must be positive and in bounds. Zero and negative
coordinates are errors because complement selection has no useful meaning for
one point. Missing coordinates produce missing output for reads and are errors
for assignment.

Character coordinates match exactly against the names of the corresponding
source axis. Every source axis must have names. Missing strings produce missing
output for reads. Empty and unmatched strings are errors.

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

A point result has dimensions `nrow(i)`. A zero-row matrix returns a
zero-length one-dimensional array. One row still returns a one-dimensional
array of length one.

### Connection to full coordinate indexing

Point matrices lower directly to full coordinate indexing. Their columns are
the coordinate arrays:

```r
points <- rbind(
  c(1, 1, 1),
  c(2, 3, 4),
  c(1, 2, 3)
)

rray_index(x, points[, 1], points[, 2], points[, 3])
```

Every column has dimensions `(P)`, so rows remain paired and the result has
dimensions `(P)`. Character coordinates are matched to integer coordinates
before this lowering.

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

The implementation should retain direct point and flat paths. Materializing or
unravelling coordinate vectors adds work without changing the public result.

## Subscript rules

`rray_slice()`, `rray_slice_axis()`, and numeric or logical flat extraction use
the same normalization rules as `vctrs::vec_as_location()` where those rules
apply:

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
  location per indexed element. `TRUE` against a zero dimension selects
  nothing, which is how a whole axis is spelled for an empty axis.
- Character slice subscripts match exactly against names on the selected
  source axis. The first match is used when source names are duplicated.
- Factors and other classed subscripts are errors.
- Out-of-bounds integers and unmatched names are errors. Slicing and
  extraction never extend the source.

Logical recycling is deliberately stricter than base R. A logical subscript
of length two does not silently recycle over an axis of dimension three.

A slice subscript must be a vector or a one-dimensional array. A matrix or
higher dimensional array is an error, as in vctrs.

Character `NA` selects a missing location, as in vctrs. The empty string, a
name that is not on the axis, and a character subscript against an axis
without names are errors.

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

`rray_extract_assign()` skips a missing location while using its replacement
value. The other assignment functions reject missing locations.

## Names

Orthogonal slicing subsets names with the same normalized locations used for
the data:

- Selecting all of an axis preserves its names without copying them.
- Reordering or duplicating locations reorders or duplicates their names.
- A missing location gets an `NA` name.
- An empty selection gives `NULL`, because R stores zero length axis names as
  `NULL`.
- Names on the list of axis names stay attached to the same axes.

`rray_slice_axis()` follows the same rules. Because one location sequence is
used for every lane, each output position on the selected axis has one clear
source name.

`rray_extract()` drops all names. Neither a flat result nor a point result has
an output axis that corresponds to one source axis.

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
out <- rray_slice_assign(x, 1, TRUE, TRUE, value = 0L)
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
  to `rray_slice()`. The new `rray_extract(x, i)` has one input containing
  flat positions or coordinate points.
- `rray_yank()`: `rray_extract()` is the clearer name for flat extraction.
- `pad()`: `rray_slice_axis()` handles an axis held in a variable, and
  `rray_slice_rows()` and `rray_slice_columns()` handle the common ones.
- `drop`: rray4 always returns arrays and never drops axes implicitly.
- `rray_take()`, `rray_take_along_axis()`, `rray_shuffle_axis()`, and
  `rray_slice_by_lane()`: `rray_slice_axis()` and `rray_index_axis()` provide
  the two distinct one-axis contracts.
- `rray_filter()`: it would duplicate logical flat extraction.

Do not overload `rray_slice()` based on the dimensions of a subscript.
`rray_extract()` has exactly two forms under the unconditional matrix dispatch
rule, and both have the same one-dimensional result contract.

## Implementation design

### Subscript normalization

A subscript normalizes to a `struct rray_subscript`:

```c
struct rray_subscript {
  r_obj* index;
  enum rray_subscript_kind kind;
  r_ssize size;
};
```

`kind` is one of `locations_int`, `locations_dbl`, `mask`, `points_int`, or
`points_dbl`. `size` is the number of selected positions, including missing
ones.

`src/subscript.c` owns the rules shared by extraction and slicing:

- Numeric locations checked against a size. Negative locations become a
  complement mask and zeros are dropped. Both allocate. Every other numeric
  subscript is returned as is.
- A logical mask of size one or the checked size, returned as is.
- The numeric summary, fractional error, and kind names used by both.

`src/extract-subscript.c` adds point matrices and logical arrays with the
dimensions of `x`, checked against `rray_size(x)`.

`src/slice-subscript.c` adds the rules for one axis, checked against the axis
dimension:

| Input | Result | Allocates |
|---|---|---|
| `NULL` | Empty `locations_int` | No |
| Logical | `mask` | No |
| Integer or double | `locations_int` or `locations_dbl` | Only for negatives and zeros |
| Character | `locations_int` matched with `Rf_match()` | Yes |

### Orthogonal slice core

1. Normalize every subscript. The output dimensions are the subscript sizes.

2. Allocate one `r_ssize` table with `sum(output dimensions)` entries, one block
   per axis. Fill each block with zero-based source locations, using
   `R_SSIZE_MIN` for a missing location. This is the only place that switches
   on `kind`.

3. Subset the names of each axis with its block. A block of
   `0, 1, ..., dimension - 1` keeps the source names without copying.

4. Multiply each location by the source stride of its axis, in place. The
   largest real offset sum is below `R_XLEN_T_MAX`, so any sum that includes
   `R_SSIZE_MIN` is negative.

5. Walk the output in column-major order. The inner run reads
   `v_x[start + v_offsets[i]]` along the first axis. Between runs, `start` is
   updated from the later axes. A second loop checks for negative locations
   and only runs when a location is missing.

The table costs one allocation proportional to the sum of the output
dimensions, and each element costs constant work. A fast path for a contiguous
first axis is added only if benchmarks against base R show a real gap.

### Extraction plan

Normalize `i` against `rray_size(x)` and convert each positive location to a
zero-based flat source offset. The dimensions of `i` do not survive. Logical
masks first become the corresponding flat locations.

The flat path shares typed copy and assignment cores with slicing but does not
build per-axis coordinate arrays.

The point path validates one matrix column against each source axis and
computes a flat source offset for each row:

```text
offset = sum((point[axis] - 1) * source_stride[axis])
```

Flat and point extraction share typed read and assignment cores after their
source offsets have been built.

### Typed cores

The shell validates, builds the plan, chooses dimensions and names, and
dispatches on storage type. Atomic cores write through data pointers.
Character and list cores use the write barrier.

The assignment shell casts and broadcasts before copying `x`. Read and
assignment use the same plan so target order cannot drift.

### Files

Keep each public family with its assignment form:

- `R/slice.R`, `src/slice.c`, `src/slice.h`, `src/decl/slice-decl.h`
- `R/slice-subscript.R`, `src/slice-subscript.c`, `src/slice-subscript.h`,
  `src/decl/slice-subscript-decl.h`
- `R/slice-axis.R`, `src/slice-axis.c`, `src/slice-axis.h`,
  `src/decl/slice-axis-decl.h`
- `R/extract.R`, `src/extract.c`, `src/extract.h`,
  `src/decl/extract-decl.h`
- `R/extract-subscript.R`, `src/extract-subscript.c`,
  `src/extract-subscript.h`, `src/decl/extract-subscript-decl.h`
- Shared `src/subscript.c`, `src/subscript.h`,
  `src/decl/subscript-decl.h`

`rray_slice_rows()` and `rray_slice_columns()` live in `R/slice-axis.R` and
need no C code of their own.

`rray_slice_axis()` can call the same internal implementation as
`rray_slice()`.

## Test plan

### Ordinary slicing

- All seven native storage types.
- Bare vectors normalize to one-dimensional arrays.
- Zero dimensions and empty selections.
- One through several axes, including the maximum supported dimensionality.
- Positive, negative, zero, logical, character, and `NULL` subscripts.
- `TRUE` selects a whole axis, including a zero dimension.
- Duplicates, reverse order, and stepped sequences.
- Out-of-bounds, mixed-sign, fractional, recycled logical, classed subscript,
  empty argument, and wrong-number-of-axes errors.
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
- A positional third argument lands in the empty dots and errors.
- `rray_slice_rows()` and `rray_slice_columns()` agree with `rray_slice_axis()`
  at axes 1 and 2.
- `rray_slice_columns()` errors on a one-dimensional array.

### Extraction

- Integer and double vectors and one-dimensional arrays are flat positions.
- Logical vectors and arrays are always flat masks.
- A logical input must have size one or `rray_size(x)`.
- Integer and double matrices with one column per source axis are points.
- Character matrices match names on every source axis.
- Point matrices with the wrong column count error.
- Character vectors and classed inputs error.
- Results are always one dimensional and unnamed.
- Flat inputs cover column-major order, duplicates, complements, zero, missing,
  and empty selections.
- Point inputs cover paired coordinates, missing coordinates, repeated points,
  and zero-row matrices.

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

Spell a whole axis as an empty argument on the base side, not as `TRUE`. Base
R rejects `TRUE` against a zero dimension, where `vec_as_location(TRUE, 0)`
returns `integer(0)`:

```r
x <- array(integer(), c(2, 0, 3))
x[TRUE, , 1:2, drop = FALSE]   # 2 0 2
x[, TRUE, 1:2, drop = FALSE]   # Error: (subscript) logical subscript too long
```

After subscript normalization, compare `rray_slice()` and
`rray_slice_axis()` with `rray_index()` over explicit open-mesh coordinates.
Compare names according to the stronger slice guarantees.

For flat positions, unravel normalized positions and compare with
`rray_index()` over one coordinate vector per source axis. Cover missing and
duplicate positions and column-major order.

For point matrices, compare `rray_extract(x, points)` with `rray_index()` over
the normalized matrix columns. Cover numeric and character coordinates.

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

### 2. Row and column shortcuts

Implement `rray_slice_rows()`, `rray_slice_assign_rows()`,
`rray_slice_columns()`, and `rray_slice_assign_columns()` on top of
`rray_slice_axis()`.

### 3. Extraction

Implement `rray_extract()` and `rray_extract_assign()` on the shared subscript
normalization and typed copy cores. Support both flat positions and coordinate
point matrices.

### 4. Coordinate indexing

Implement the strict coordinate family from `plans/index.md`, then add the
reference properties that lower slicing and extraction to fully specified
coordinates.

### 5. Main implementation plan

Update the corresponding section of `plans/implementation.md`. Keep this file
as the source of truth for ordinary subscripts and extraction, and
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
