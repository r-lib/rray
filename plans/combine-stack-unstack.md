# Combine, split, stack, and unstack

## Status

This document is an implementation plan. `rray_combine()`, `rray_split()`, and
`rray_stack()` are done. The remaining work adds unstack.

Split shipped with its second argument named `dimensions` rather than `sizes`,
behind empty dots. This document uses the shipped name throughout.

## Goal

Provide a parallel family of array operations:

```r
rray_combine(..., .axis)
rray_split(x, ..., axis, dimensions)
rray_stack(..., .axis)
rray_unstack(x, axis)
```

The pairs have direct relationships:

- `rray_split()` divides one existing axis into contiguous chunks. Combining
  those chunks on the same axis reconstructs the input.
- `rray_stack()` inserts a new singleton axis into each input, then combines
  the inputs on that axis.
- `rray_unstack()` splits one existing axis into chunks of size 1, then removes
  that singleton axis from every chunk.

Axes are one based. Negative axes are not accepted.

`.axis` follows `...` in combine and stack, so callers must name it. Split and
unstack take `axis` after empty dots, so callers name it there too.

## Current state

`rray_combine()` has been merged. Treat its behavior and implementation as the
foundation for this work, not as unfinished work to repeat.

The merged implementation already provides:

- broadcasting on every axis except `.axis`;
- support for differing input dimensionality through implicit trailing axes of
  dimension 1;
- common type calculation and casting;
- an internal `ptype` argument used by C callers;
- concatenation of names on the combined axis;
- broadcast name selection on every other axis;
- checked dimension sums and output sizes;
- strided writes into the final output without full broadcast copies.

The public R function does not expose `.ptype`. Stack follows the same rule.
Prototype selection stays internal on both.

The merged combine implementation deliberately has no lower-level prepared
entry point. Keep that boundary. Stack composes the existing internal
`rray_combine()` entry point rather than exposing a second combine engine.

`rray_split()` has been merged with the single-axis, variable-dimension
interface described below. The old multiple-axis split is gone.

`rray_stack()` has been merged. It prepares each input with
`rray_expand_dimensionality()` and `rray_insert_axes()`, calls `rray_combine()`,
and then names the new axis from `...`.

## Decision summary

- Keep the existing public `rray_combine(..., .axis)` unchanged.
- Replace `rray_split(x, axes)` with `rray_split(x, ..., axis, dimensions)`.
- Split along one axis only and retain that axis in every output.
- A length-1 `dimensions` value is a uniform chunk dimension and must divide the
  selected axis dimension exactly.
- A `dimensions` vector with length other than 1 gives each chunk dimension
  directly. Its entries may be zero and must sum to the selected axis dimension.
- A scalar chunk dimension must be positive. Zero is only meaningful in the
  explicit vector form.
- Optimize the uniform `dimensions = 1L` case because unstack uses it and it is
  the most common direct split.
- Implement stack as trailing singleton expansion where needed, insertion of a
  singleton axis, and a call to the existing combine engine.
- Add an internal `rray_expand_dimensionality()` helper that pads dimensions
  with trailing ones without copying data.
- Expand an input for stack only when it has fewer than `axis - 1` dimensions.
  There is no need to expand all inputs to the greatest dimensionality.
- Implement unstack as split with `dimensions = 1L`, followed by removal of the
  selected singleton axis from every piece.
- Names of `...` become names on the new axis made by stack.
- Names on the removed axis become names of the list returned by unstack.
- Keep prototype control internal for combine and stack.
- Add no lower-level R or FFI entry points beyond the wrappers for the new
  public functions.

## Public behavior

### `rray_combine()`

`rray_combine()` joins one or more arrays along an existing axis. The selected
dimensions are added. Every other axis broadcasts to a common dimension.

```text
[2, 3] + [4, 3] along axis 1 -> [6, 3]
[2, 3] + [2, 5] along axis 2 -> [2, 8]
```

The selected axis does not broadcast. Missing trailing axes act as dimensions
of 1, including when the missing axis is the selected axis.

```text
[2] + [2, 3] along axis 2 -> [2, 4]
```

No public behavior changes are planned for combine. Its current tests remain
part of the verification for stack and split composition.

### `rray_split()`

`rray_split()` divides `x` into contiguous chunks along one existing `axis`.
Each output keeps the dimensionality of `x`. Only the selected axis dimension
changes.

For `x` with dimensions `[6, 3]`:

```text
split on axis 1 with dimensions 2       -> three arrays of [2, 3]
split on axis 1 with dimensions c(1, 5) -> arrays of [1, 3] and [5, 3]
split on axis 1 with dimensions c(0, 6) -> arrays of [0, 3] and [6, 3]
```

The two forms of `dimensions` have different meanings.

#### Uniform chunk dimension

A length-1 `dimensions` value is the dimension of every chunk. It must be
positive and the selected axis dimension must be evenly divisible by it.

```text
axis dimension 6, dimensions 1 -> 6 chunks of dimension 1
axis dimension 6, dimensions 2 -> 3 chunks of dimension 2
axis dimension 6, dimensions 6 -> 1 chunk of dimension 6
axis dimension 6, dimensions 4 -> error
```

An axis dimension of 0 produces an empty list for any positive uniform chunk
dimension. In particular, `dimensions = 1L` works without a special error.

#### Explicit chunk dimensions

A `dimensions` vector with length other than 1 gives the dimension of every
output chunk in order. Every entry must be nonnegative and their checked sum
must equal the selected axis dimension. Zero-dimension chunks are retained in
the output list.

```text
axis dimension 6, dimensions c(2, 0, 4) -> chunk dimensions 2, 0, and 4
axis dimension 6, dimensions c(2, 3)    -> error
```

`integer()` is valid only when the selected axis has dimension 0. It returns an
empty list. This is the explicit counterpart of splitting an empty axis into no
chunks.

The output is always an unnamed list. Split has no source for names describing
the chunk boundaries. Axis names stay on the selected axis inside each chunk.

The central identity is:

```r
pieces <- rray_split(x, axis = axis, dimensions = dimensions)
out <- rray_combine(!!!pieces, .axis = axis)
```

`out` should be identical to `x`, including type, dimensions, values, and
dimension names, for every valid split that produces at least one piece.
Combine requires at least one input, so an empty split of a zero-length axis
does not have a combine round trip.

### `rray_stack()`

`rray_stack()` joins one or more inputs along a newly inserted axis. Old axes
broadcast according to the existing combine rules.

For two arrays with dimensions `[2, 3]`:

```text
stack on axis 1 -> [2, 2, 3]
stack on axis 2 -> [2, 2, 3]
stack on axis 3 -> [2, 3, 2]
```

The first two shapes happen to match because there are two inputs. Their value
layouts differ. Tests must check slices, not only dimensions.

For greatest input dimensionality `D`, valid axes are 1 through `D + 1`.
Stacking adds exactly one axis, so the output dimensionality is `D + 1`.

Stack broadcasts old axes because it delegates to combine:

```text
[2, 1, 3] and [1, 4, 3]
stack on axis 2 -> [2, 2, 4, 3]
```

Inputs of different dimensionality also work:

```text
[2] and [2, 3]
stack on axis 3 -> [2, 3, 2]
```

One input is valid and adds an axis of dimension 1. No inputs is an error.

Stack uses the common rray4 type chosen by combine. Its public R interface has
no `.ptype` argument. The internal C entry point accepts a prototype so an
internal caller can select one without adding another public path.

### `rray_unstack()`

`rray_unstack()` returns one slice for every position on `axis`. The selected
axis is removed from each slice.

```text
unstack [2, 3, 4] on axis 1 -> 2 arrays of [3, 4]
unstack [2, 3, 4] on axis 2 -> 3 arrays of [2, 4]
unstack [2, 3, 4] on axis 3 -> 4 arrays of [2, 3]
```

Its definition is deliberately mechanical:

1. Call `rray_split(x, axis = axis, dimensions = 1L)`.
2. Call `rray_remove_axes(piece, axis)` on every piece.
3. Use the removed axis names as the names of the output list.

The input must have dimensionality 2 or greater. rray4 has no zero-dimensional
arrays, and `rray_remove_axes()` cannot remove the only axis.

An axis of dimension 0 returns an empty list. An axis of dimension 1 returns a
one-element list. Every element keeps the storage type of `x`.

## Why stack only expands to `axis - 1`

`rray_insert_axes()` refers to axes in the result. An input must therefore have
at least `axis - 1` dimensions before a new axis can be inserted at `axis`.

It does not need the greatest input dimensionality first. The combine engine
already treats missing trailing dimensions as 1.

For inputs `[2]` and `[2, 3]` stacked on axis 3:

```text
[2]    -> expand to [2, 1] -> insert axis 3 -> [2, 1, 1]
[2, 3]                         insert axis 3 -> [2, 3, 1]
combine on axis 3                              -> [2, 3, 2]
```

For the same inputs stacked on axis 1, neither input needs expansion:

```text
[2]    -> insert axis 1 -> [1, 2]
[2, 3] -> insert axis 1 -> [1, 2, 3]
combine on axis 1       -> [2, 2, 3]
```

The implicit trailing axis on the first prepared input broadcasts to 3. This
keeps stack small and lets combine remain the single source of broadcasting
rules.

## Internal dimensionality expansion

This helper is in `src/dimensionality.c` and `src/dimensionality.h`:

```c
r_obj* rray_expand_dimensionality(
  r_obj* x,
  int dimensionality,
  struct rray_arg* arg,
  struct r_lazy error_call
);
```

Its contract is:

- normalize and validate `x` as an unclassed array;
- require `dimensionality` to be greater than the current dimensionality;
- require the target to stay within `RRAY_MAX_DIMENSIONALITY`;
- return a metadata-only wrapper;
- copy the existing dimensions and append dimensions of 1;
- preserve existing axis names in place;
- leave every appended axis unnamed;
- always append at least one singleton axis.

It has no R wrapper and no FFI registration. The helper is implementation
support for stack. If it later proves useful as a public array operation, it can
be documented and exported separately.

A dimensionality that is not greater than the current one is an internal error,
because the only caller checks first. Stack calls it only when an input
dimensionality is less than `axis - 1`. Inputs that already reach the insertion
point go directly to `rray_insert_axes()`.

## Split implementation

Done. The engine works one chunk at a time, which also incorporates the useful
result from `plans/split-optimize.md`: write one output buffer to completion
instead of keeping many output write streams active.

### Validation and size planning

1. Normalize and validate `x` as an unclassed array.
2. Convert `axis` to one integer and validate it against the dimensionality of
   `x`.
3. Convert `dimensions` to an integer vector without attributes.
4. Read the dimension on `axis`.
5. Select uniform mode when `dimensions` has length 1.
6. In uniform mode, require a positive dimension and exact divisibility. Set the
   output count to `axis_dimension / dimension`.
7. In explicit mode, require nonnegative entries and a checked sum equal to the
   axis dimension. Set the output count to `length(dimensions)`.
8. Allocate the output list once.

Errors name `axis` and `dimensions`, not `.axis`.

Do not divide by the selected dimension. A zero axis dimension is valid.

### Dimensions and allocation

Every output copies the dimensions of `x` and replaces the selected dimension
with its chunk size.

Uniform mode can allocate one dimensions vector and share it across every
output because all chunks have the same shape. This is especially important
for `dimensions = 1L`.

Explicit mode must use dimensions matching each chunk. It shares one dimensions
object across a run of equal chunk dimensions.

Use checked size calculation before allocating each output. A zero chunk size
produces a valid zero-size array with a zero dimension on `axis`.

### Data movement

Compute ordinary input strides once. For each chunk:

1. Build a point space from that chunk's output dimensions.
2. Use the input strides as the source location strides.
3. Start the source at the cumulative axis offset multiplied by the input
   stride for `axis`.
4. Write the current output sequentially from location 0.
5. Advance the cumulative axis offset by the chunk size.

This traverses one complete chunk before moving to the next. It avoids the
cache conflict problem recorded in `plans/split-optimize.md` for the old flat
split kernel.

Uniform mode should build the iterator plan once and reuse it for every chunk.
The `dimensions = 1L` path then has one dimensions object, one plan, and one
simple outer loop over axis positions.

Use one typed core for each native type. Atomic types write through direct
pointers. Character and list types use write barriers. A contiguous source run
may use a bulk copy for atomic types when the iterator reports stride 1.

Zero-size chunks allocate and attach metadata but perform no copy. Repeated
zero chunks do not advance the source offset.

### Split names

Each chunk keeps names on every axis:

- names on non-split axes are shared unchanged;
- names on the split axis are sliced over the same contiguous range as the
  data;
- a zero-size named chunk receives `character()` on the split axis;
- if `x` has no dimension names, no output gets a `dimnames` attribute.

The current `rray_split_names()` machinery is designed for multiple axes of
size 1. Replace it with single-axis chunk name handling owned by split. Remove
its separate FFI entry point. There should be no user-callable lower-level name
operation.

The result list itself stays unnamed.

## Stack implementation

Done. Stack is a small composition around the combine engine, in `src/stack.c`
with `rray_stack_prepare()` as its only helper.

### Validation and preparation

1. Reject an empty input list.
2. Convert `.axis` with `arg_as_int()` and `rray_args.dot_axis`.
3. Find the greatest input dimensionality `D` with the existing helper and
   original dots argument context.
4. Check that `D + 1` stays within `RRAY_MAX_DIMENSIONALITY`.
5. Validate `.axis` from 1 through `D + 1`.
6. For each input whose dimensionality is less than `.axis - 1`, call
   `rray_expand_dimensionality()` with `.axis - 1`.
7. Call `rray_insert_axes()` on each prepared input with `.axis`.
8. Keep the original dots names on the prepared list.

Build a subscript argument for each input before calling expansion or insertion
so errors still identify a named dots element or its `..n` position.

The inserted views do not copy data. They preserve old axis names, leave the
new axis unnamed, and leave combine responsible for casting, broadcasting, and
copying into the final result.

### Delegate to combine

Give stack an internal C signature parallel to combine:

```c
r_obj* rray_stack(
  r_obj* xs,
  int axis,
  r_obj* ptype,
  struct rray_arg* arg,
  struct rray_arg* ptype_arg,
  struct r_lazy error_call
);
```

The FFI wrapper passes `r_null` for `ptype` and empty internal argument tags,
just as the current combine FFI wrapper does. There is no public `.ptype`
argument.

Call the existing `rray_combine()` with the prepared views, `.axis`, and the
internal prototype. Do not add a prepared combine helper to a header and do not
duplicate combine's type, dimensions, names, iterator, or fill logic.

Errors raised by combine must retain the `rray_stack()` call and the original
dots argument names.

### Stack names

After combine returns, replace names on the inserted axis with the names of
`...`:

```r
rray_stack(first = x, second = y, .axis = 2L)
```

The new second axis has names `c("first", "second")`.

If no inputs are named, the new axis is unnamed. If only some are named, keep
the empty strings supplied by `list2()`. Old axis names are already handled by
insert and combine.

Stack does not mutate the result returned from combine in place, in case combine
ever gains a fast path that returns an input. It calls `rray_set_axis_names()`,
which wraps the result and attaches a fresh names list.

## Unstack implementation

Unstack should reuse split and remove-axes rather than owning another copy
engine.

1. Normalize and validate `x`.
2. Require dimensionality 2 or greater.
3. Convert and validate `axis` as one axis of `x`.
4. Call the internal split entry point with `dimensions = 1L`.
5. Call `rray_remove_axes()` on `axis` for every split piece.
6. Set the output list names to the original names on `axis`.

The split pieces already have the right values, type, and surviving axis
names. Removing the singleton axis is a metadata-only operation and drops its
one-element name from each piece.

Keep unstack as a native feature pair so validation, argument context, and the
loop over pieces stay in C. Do not expose the internal split result or a
`keepdims` switch through another R entry point.

For an unnamed removed axis, leave the output list unnamed. For a named axis
of dimension 0, `character()` is a valid zero-length names attribute and should
be retained.

## Names and round trips

Names make the two pairs useful inverses.

### Split and combine

Split subsets the selected axis names into each chunk. Combine concatenates
those names. Names on other axes are shared by split and selected by combine's
existing broadcast name rules.

For every nonempty valid split:

```r
pieces <- rray_split(x, axis = axis, dimensions = dimensions)
rray_combine(!!!pieces, .axis = axis)
```

must reproduce all names on `x`.

### Stack and unstack

Stack turns dots names into names on the new axis. Unstack turns those axis
names back into list names.

```r
pieces <- rray_unstack(x, axis)
out <- rray_stack(!!!pieces, .axis = axis)
```

This should reproduce the values, dimensions, type, and dimension names of
`x` for every valid axis.

The other direction is exact when stack does not broadcast or cast:

```r
rray_unstack(
  rray_stack(a = x, b = y, .axis = axis),
  axis = axis
)
```

It returns `list(a = x, b = y)` when `x` and `y` already have equal dimensions,
types, and compatible names.

When stack broadcasts or casts, unstack returns the broadcasted and cast forms.
It cannot recover singleton dimensions that expanded or narrower input types.

## R interface

Keep one R file per public operation:

- `R/combine.R`
- `R/split.R`
- `R/stack.R`
- `R/unstack.R`

The new wrappers are thin:

```r
rray_split <- function(x, ..., axis, dimensions) {
  check_dots_empty0(...)
  .Call(ffi_rray_split, x, axis, dimensions, environment())
}

rray_stack <- function(..., .axis) {
  .Call(ffi_rray_stack, list2(...), .axis, environment())
}

rray_unstack <- function(x, axis) {
  .Call(ffi_rray_unstack, x, axis, environment())
}
```

Do not change the `rray_combine()` wrapper and do not add `.ptype` to any
public wrapper.

Export and document all four functions. Add them to the Manipulation section of
`_pkgdown.yml`. Combine, split, and stack are there already.

## Native files and registration

Keep or add these feature pairs and decl headers:

- `src/combine.c`, `src/combine.h`, `src/decl/combine-decl.h`
- `src/split.c`, `src/split.h`, `src/decl/split-decl.h`
- `src/stack.c`, `src/stack.h`, `src/decl/stack-decl.h`
- `src/unstack.c`, `src/unstack.h`, `src/decl/unstack-decl.h`

The internal expansion helper lives in `src/dimensionality.c` and
`src/dimensionality.h`. Add declarations to a decl header only for static
helpers.

The old split-name feature files are gone.

The registrations are:

```text
ffi_rray_split(ffi_x, ffi_axis, ffi_dimensions, ffi_frame)
ffi_rray_stack(ffi_xs, ffi_axis, ffi_frame)
ffi_rray_unstack(ffi_x, ffi_axis, ffi_frame)
```

Do not register dimensionality expansion or any lower-level combine, split,
stack, or unstack helper.

The C files must follow the package's top-down order. FFI wrappers come first,
internal entry points next, then helpers and typed cores in the order used. The
decl include stays last. Do not add source comments.

## Existing split references

Done. Tests, benchmarks, and broader plans use the new split signature, and
`plans/split-optimize.md` is gone.

## Errors and validation

Use snapshots for error tests.

Split must cover:

- missing `axis` or `dimensions`;
- `axis` with length other than 1, missing values, attributes, or a lossy cast;
- `axis` below 1 or above the input dimensionality;
- `dimensions` with attributes, missing values, or lossy casts;
- scalar `dimensions` equal to or below 0;
- a scalar dimension that does not divide the axis dimension;
- negative explicit dimensions;
- an explicit checked sum below or above the axis dimension;
- an empty explicit vector for a nonempty axis;
- classed and invalid array inputs.

Stack must cover:

- no inputs;
- missing or invalid `.axis`;
- `.axis` below 1 or above `D + 1`;
- an output above maximum dimensionality;
- incompatible dimensions outside the new axis;
- incompatible types;
- classed and invalid array inputs;
- errors that identify a named input or its `..n` position.

Unstack must cover:

- missing or invalid `axis`;
- 1D input;
- an axis outside the input dimensionality;
- classed and invalid array inputs.

Errors from composed operations must point to the public call the user made.
Stack errors say `.axis`. Split and unstack errors say `axis`.

## Test plan

### Combine regression tests

Keep the current combine suite passing. Add tests only where stack or split
composition reveals a missing guarantee. Do not recreate already merged
combine coverage.

### Split tests

Done, in `tests/testthat/test-split.R`.

- Split vectors, matrices, and 3D arrays along every axis.
- Check exact values and dimensions for uniform dimensions 1, 2, and the full
  axis.
- Check explicit equal, unequal, and zero chunk dimensions.
- Check `integer()` against a zero and nonzero axis dimension.
- Check a zero-dimensional axis in uniform mode.
- Check zero-size chunks at the beginning, middle, and end.
- Check all seven native storage types.
- Keep names on non-split axes.
- Slice names on the split axis, including zero-size chunks.
- Keep the result list unnamed.
- Check that input objects are not modified.
- Check combine round trips for uniform and explicit forms.
- Compare against a small reference implementation built from `[` over a
  range of shapes, axes, and chunk plans.

### Stack tests

Done, in `tests/testthat/test-stack.R`. `stack_slice()` in
`tests/testthat/helper-stack.R` takes one position on the new axis and removes
it, so a slice can be compared to an input directly.

- Stack vectors on axes 1 and 2.
- Stack 2D arrays on axes 1, 2, and 3.
- Check exact values by slicing the new axis.
- Check one input and more than two inputs.
- Check all seven native storage types.
- Check common type promotion.
- Broadcast singleton dimensions.
- Broadcast differing dimensionality.
- Exercise an axis that requires trailing expansion for only some inputs.
- Reject incompatible dimensions and types.
- Check zero-size inputs.
- Move dots names to the new axis.
- Keep partial dots names with empty strings.
- Leave the new axis unnamed when dots are unnamed.
- Keep old axis names under broadcasting.
- Reject greatest input dimensionality 64 because stack would make 65.
- Check that input objects are not modified.

### Dimensionality expansion tests

Done, alongside the stack tests. Exercise the internal helper through stack:

- stack bypasses the helper when the insertion axis is already reachable;
- one and several appended singleton axes;
- preservation of existing dimensions and names;
- unnamed appended axes;
- differing input dimensionality at the last valid stack axis.

There is no R-facing test for `rray_expand_dimensionality()` because it is not
public.

### Unstack tests

- Unstack a 2D array along both axes.
- Unstack a 3D array along every axis.
- Check dimensions and exact slice values.
- Check all seven native storage types.
- Check an axis with dimension 0.
- Check an axis with dimension 1.
- Move removed axis names to list names.
- Shift surviving axis names to their new positions.
- Handle partial dimension names.
- Reject a 1D input.
- Check that `x` is not modified.

### Round-trip tests

For each axis of named 2D and 3D arrays, test both families:

```r
pieces <- rray_split(x, axis = axis, dimensions = dimensions)
out <- rray_combine(!!!pieces, .axis = axis)
expect_identical(out, x)

pieces <- rray_unstack(x, axis)
out <- rray_stack(!!!pieces, .axis = axis)
expect_identical(out, x)
```

Also unstack a stack of named equal-shape inputs and expect the original named
list. Add one broadcasted and one cast stack case and expect the normalized
outputs rather than the original inputs.

## Work order

Steps 1 through 6 are done.

1. ~~Rewrite split validation around one `axis` and `dimensions`.~~
2. ~~Replace its flat multiple-output traversal with one-chunk-at-a-time
   copying.~~
3. ~~Add chunk dimension names and remove the old split-name entry point.~~
4. ~~Add and test the internal dimensionality expansion helper.~~
5. ~~Implement stack with expansion, axis insertion, and the current combine
   entry point.~~
6. ~~Add dots names to the new stack axis.~~
7. Implement unstack through split and remove-axes.
8. Add pairwise round-trip tests.
9. Update registration, documentation, pkgdown, benchmarks, and broader plan
   references.
10. Run the protection audit, formatting, documentation checks, focused tests,
    benchmarks, and the full test suite.

## C protection audit

Before running any C change, perform the required separate protection pass over
the full diff.

For every new or touched `r_obj*`:

1. Name the next function that reads it.
2. Check whether that function can allocate before it protects or consumes the
   value.
3. Add `KEEP()`, `KEEP_HERE()`, or `KEEP_AT()` where needed.
4. Check pointers returned by vector accessors. Their owners must stay
   protected across every later allocation.
5. Check shared dimensions and names in uniform split while outputs are
   allocated and attributes are attached.
6. Check each explicit split dimensions and names object through allocation and
   attachment.
7. Check stack's expanded wrappers, inserted wrappers, prepared list, prototype,
   and combined result.
8. Check unstack's split list and every remove-axes wrapper.
9. Balance every success-path `FREE()` count. Error paths unwind through R.

Do not use `gctorture()` or `gctorture2()`.

## Verification

After C or R code is generated, run both required formatters:

```sh
clang-format -i src/*.c src/*.h
air format .
```

Run focused tests:

```sh
Rscript -e "devtools::test(filter = '^(combine|split|stack|unstack|dimensionality|insert-axes|remove-axes)$')"
```

Redocument and check the reference index:

```sh
Rscript -e "devtools::document()"
Rscript -e "pkgdown::check_pkgdown()"
```

Run the full test suite last:

```sh
Rscript -e "devtools::test()"
```

Split benchmarks live in `bench/iterator.R` and cover leading, middle, and
trailing axes with `dimensions = 1L`, a larger uniform chunk, and an explicit
unequal plan. Stack adds no new traversal, so it needs no benchmark of its own.

## Done means

- The four public signatures match this plan.
- Combine remains unchanged and all of its current tests pass.
- Split accepts one axis and both forms of `dimensions`.
- Split retains its selected axis and handles zero-dimension chunks.
- Uniform split, especially `dimensions = 1L`, reuses dimensions and traversal
  work.
- Stack is implemented through expansion, insertion, and combine.
- Stack expands inputs only as far as the insertion point requires.
- Unstack is implemented through dimension-1 split and remove-axes.
- Split/combine and stack/unstack pass the stated round trips.
- Public combine and stack do not expose `.ptype`.
- Internal combine and stack can receive a prototype.
- No lower-level R or FFI entry points exist beyond the wrappers for the public
  functions.
- Names, zero dimensions, value order, types, and errors match this plan.
- Old multiple-axis split behavior and split-name registration are gone.
- Checked-in benchmarks and broader plans use the new split signature.
- No input is modified.
- Native routines are registered with the correct arities.
- `_pkgdown.yml` includes combine, split, stack, and unstack.
- The explicit protection pass is complete.
- All C and R files are formatted.
- Focused tests, the full suite, documentation, and pkgdown checks pass.
