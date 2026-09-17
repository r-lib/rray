# Combine, stack, and unstack

## Status

This document is an implementation plan. It does not describe code that is
already present.

The target branch is `feature/combine-stack-unstack`, based on `main`.

## Goal

Add three array manipulation functions:

```r
rray_combine(..., .axis)
rray_stack(..., .axis)
rray_unstack(x, .axis)
```

This family replaces `rray_split()`. Remove split in the same implementation
change. There is no deprecation period and no private split engine left behind.

They follow the main ideas of the Python Array API functions `concat()`,
`stack()`, and `unstack()`:

- `rray_combine()` joins arrays along an existing axis.
- `rray_stack()` joins arrays along a new axis.
- `rray_unstack()` removes one axis and returns its slices in a list.

The Python references are:

- <https://data-apis.org/array-api/2024.12/API_specification/generated/array_api.concat.html>
- <https://data-apis.org/array-api/2024.12/API_specification/generated/array_api.stack.html>
- <https://data-apis.org/array-api/2024.12/API_specification/generated/array_api.unstack.html>

rray4 should differ in two deliberate ways:

- Axes are one based and negative axes are not accepted.
- `rray_combine()` and `rray_stack()` broadcast their inputs on every axis
  except the axis being combined.

`.axis` is required. Because it follows `...`, callers must name it. This
matches the proposed R signatures and the package's existing axis-taking
functions. Python uses the first axis as the default, so this is an intentional
R API difference.

## Decision summary

- Implement broadcasting in the first version. It is well defined and fits
  the rest of rray4.
- Require callers to supply `.axis` by name. It has no default.
- `rray_combine()` accepts `.axis` from 1 through the greatest input
  dimensionality.
- `rray_stack()` accepts `.axis` from 1 through the common input
  dimensionality plus 1. For a 2D input, axes 1, 2, and 3 are all valid.
- `rray_unstack()` accepts arrays with dimensionality 2 or greater. rray4 has
  no 0D arrays, so unstacking a 1D array cannot return valid rray4 arrays.
- Inputs use the common rray4 type. No `.ptype` argument is added.
- Names on the combined axis are concatenated. Missing pieces are represented
  by empty strings if another input supplies names.
- Names of `...` become names on the new axis made by `rray_stack()`.
- Names on the removed axis become names of the list returned by
  `rray_unstack()`.
- `rray_stack()` should be built as cheap dimension views followed by the
  combine engine.
- `rray_unstack()` replaces `rray_split()` and owns the slice engine directly.
- Remove the old split API and all of its private name machinery.

## Public behavior

### `rray_combine()`

`rray_combine()` joins one or more arrays along `.axis`. The selected axis
keeps its dimension from each input, and those dimensions are added together.
Every other axis is broadcast to a common dimension.

For inputs with the same dimensionality:

```text
[2, 3] + [4, 3] along axis 1 -> [6, 3]
[2, 3] + [2, 5] along axis 2 -> [2, 8]
```

The selected axis is not broadcast. A dimension of 1 on that axis contributes
one position to the result. On all other axes, the usual rray4 rule applies:
equal dimensions are compatible, and a dimension of 1 can expand.

```text
[2, 1, 3] + [3, 2] along axis 1

pad the second input:       [3, 2, 1]
broadcast the first chunk:  [2, 2, 3]
broadcast the second chunk: [3, 2, 3]
combine the chunks:         [5, 2, 3]
```

This confirms the example in the request. rray4 aligns dimensions from the
first axis and adds missing singleton axes at the end. It does not use NumPy's
right-aligned broadcasting rule.

An axis that is missing from a lower-dimensional input is treated as an
implicit dimension of 1. For example:

```text
[2] + [2, 3] along axis 2

pad the first input: [2, 1]
result:              [2, 4]
```

This follows directly from existing rray4 broadcasting. It should be shown in
the documentation because it is less obvious than singleton broadcasting on
an axis that is already present.

One input is valid. It is normalized to an array and cast to its own common
type. Its dimensions and names are otherwise unchanged.

No inputs is an error. Named dots are used in error messages but do not add or
prefix names on the combined axis.

### `rray_stack()`

`rray_stack()` first finds the common broadcast dimensions of its inputs. It
then inserts a singleton axis into each input and combines along that axis.

For two 2 by 3 arrays:

```text
axis 1: [2, 2, 3]
axis 2: [2, 2, 3]
axis 3: [2, 3, 2]
```

The first two results happen to have the same dimensions because there are two
inputs. Their values are laid out differently. With three inputs the
difference is clearer:

```text
three [2, 3] arrays on axis 1 -> [3, 2, 3]
three [2, 3] arrays on axis 2 -> [2, 3, 3]
three [2, 3] arrays on axis 3 -> [2, 3, 3]
```

Again, the last two shapes happen to match because both an old dimension and
the input count are 3. Indexing tests must check values, not just dimensions.

The useful mechanical description is:

```text
stack [2, 3] on axis 1: [1, 2, 3], then combine on axis 1
stack [2, 3] on axis 2: [2, 1, 3], then combine on axis 2
stack [2, 3] on axis 3: [2, 3, 1], then combine on axis 3
```

For inputs with common dimensionality `D`, valid axes are `1` through `D + 1`.
The output has dimensionality `D + 1`. This means the user's check was right:
a 2D array may be stacked on axis 3 to put the new axis last.

Broadcasting happens before the new axis is inserted:

```text
[2, 1, 3] and [1, 4, 3] -> common dimensions [2, 4, 3]
stack on axis 2              -> [2, 2, 4, 3]
```

A useful feature of broadcasted stack is building feature planes over a grid:

```r
row <- array(c(10, 20), c(2, 1))
column <- array(c(1, 2, 3), c(1, 3))

out <- rray_stack(
  row = row,
  column = column,
  .axis = 3
)
```

The inputs broadcast to `[2, 3]`, then stack to `[2, 3, 2]`:

```text
out[, , "row"]       out[, , "column"]

10 10 10              1 2 3
20 20 20              1 2 3
```

Each `out[i, j, ]` contains the row and column features for one grid position.
The same pattern is useful for coordinate grids, model feature arrays, image
channels, and parameter grids. Without broadcasted stack, callers must
explicitly broadcast every input before packing them along the new axis.

Use this as a main `rray_stack()` documentation example. It shows that
broadcasting is a useful part of the function rather than only a relaxed shape
check. It also shows dots names becoming names on the new axis.

One input is valid and adds a new axis with dimension 1. No inputs is an
error.

### `rray_unstack()`

`rray_unstack()` returns one slice for every position on `.axis`, in storage
order for that axis. The selected axis is removed from each slice rather than
kept with dimension 1.

```text
unstack [2, 3, 4] on axis 1 -> 2 arrays of [3, 4]
unstack [2, 3, 4] on axis 2 -> 3 arrays of [2, 4]
unstack [2, 3, 4] on axis 3 -> 4 arrays of [2, 3]
```

The input must have dimensionality 2 or greater. The Python Array API can
unstack a 1D array into 0D arrays. rray4 normalizes vectors to 1D arrays and
does not support 0D arrays, so accepting that case would break the package's
basic array rule.

An axis with dimension 0 returns an empty list. An axis with dimension 1
returns a one-element list.

The result type is always list. Each element keeps the storage type of `x`.

## Broadcasting is a sound extension

NumPy and the Python Array API require equal input shapes for `stack()`, and
equal shapes outside the selected axis for `concat()`. rray4 does not need to
copy that restriction.

For combine, define the result in two steps:

1. Find one common broadcast dimension for every non-combine axis.
2. Broadcast each input to its own chunk, keeping its combine-axis dimension,
   then place the chunks one after another.

There is no conflict between broadcasting and combining because they control
different axes. The combined axis is added, never recycled. Every other axis
uses the existing common-dimension rule.

The rule is also stable across more than two inputs. Common non-combine
dimensions are computed across the full input list. The final combine-axis
dimension is the sum across the same full list. Grouping inputs differently
does not change the dimensions or values, apart from the package's existing
first-input rule for choosing names.

For stack, the same rule applies after inserting a singleton axis. This gives
a clean relation:

```text
stack(xs, axis) = combine(expand_each(xs, axis), axis)
```

### Benefits

- Broadcasting is the main purpose of rray4, so strict shape matching would
  be an odd exception.
- The rule handles singleton dimensions and differing dimensionality with one
  model already used elsewhere in the package.
- The implementation can read inputs through broadcast strides. It does not
  need full broadcast copies.
- Adding broadcasting later would expand the accepted input set, but deciding
  it now gives names and errors one clear design from the start.

### Costs and risks

- The behavior differs from Python even though the functions use Python as
  their model. The rray4 behavior must still have one clear definition.
- A missing trailing axis acts like dimension 1. Combining on that implicit
  axis is logical but may surprise a reader.
- The output write is strided when the selected axis is not last. A flat
  `memcpy()` implementation is not enough.
- Names need separate rules for the combined axis and broadcast axes.

These costs are manageable. The iterator already represents the required
source and destination strides. The plan therefore recommends broadcasting in
the first version, without a `.broadcast` switch.

If implementation work finds a real blocker, strict shape matching can ship
first and broadcasting can follow without breaking successful calls. That is
a fallback, not the target behavior of this plan.

## Type rules

`rray_combine()` and `rray_stack()` find one common type across all inputs by
using the existing `rray_ptype_common()` and `rray_cast_common()` rules.

The supported results are:

| Inputs | Result |
|---|---|
| logical only | logical |
| logical and integer | integer |
| logical, integer, double | widest of those types |
| logical, integer, double, complex | widest of those types |
| character only | character |
| raw only | raw |
| list only | list |

Incompatible families remain an error. For example, integer and character do
not combine. Classed inputs, `NULL`, functions, environments, and other scalar
objects remain errors under the existing array checks.

There is no `.ptype` argument in this API. It can be added later if users need
an explicit type override. The first implementation should keep the surface
small.

The combine engine may materialize an input only when it must cast to the
common type. Broadcasting must happen while copying into the final output, not
by calling `rray_broadcast()` on every input.

`rray_unstack()` does not combine types. Every output element has exactly the
storage type of `x`.

## Dimension rules

### Common setup

Bare vectors are normalized to 1D arrays before dimension work. Differing
dimensionality follows the current package rule: shorter dimension vectors are
padded with trailing ones.

All outputs must stay within `RRAY_MAX_DIMENSIONALITY`, which is currently 64.

### Combine dimensions

Let `D` be the greatest dimensionality across the inputs. `.axis` must be from
1 through `D`.

Build `out_dimensions` as follows:

- On `.axis`, use the sum of each input's dimension. Use 1 when the input does
  not reach that axis.
- On every other axis, merge dimensions with the current broadcasting rule.

For each input, also build `chunk_dimensions`:

- Start with `out_dimensions`.
- Replace the combine-axis dimension with that input's actual or implicit
  dimension.

The input must broadcast to `chunk_dimensions`.

### Stack dimensions

Find ordinary common broadcast dimensions across the inputs first. Let their
length be `D`. `.axis` must be from 1 through `D + 1`.

Insert the number of inputs at `.axis` in the common dimensions. These are the
output dimensions.

Each input gets a cheap view whose dimensions are made in two steps:

1. Pad its dimensions with trailing ones to length `D`.
2. Insert a dimension of 1 at `.axis`.

Combining those views on `.axis` produces the required result and lets the
combine engine handle all data movement.

Stack must reject common dimensionality 64 because its output would have
dimensionality 65.

### Unstack dimensions

Copy the dimensions of `x` except for `.axis`. Since `x` must be at least 2D,
at least one dimension remains.

### Zero dimensions and overflow

Zero dimensions remain valid.

- A non-combine dimension of 1 can broadcast to 0, following current rray4
  rules.
- A combine-axis dimension of 0 contributes nothing to the sum.
- Stacking zero-size arrays still adds a new axis whose dimension is the
  number of inputs.
- Unstacking an axis of dimension 0 returns an empty list.

Dimension sums must be computed in `r_ssize`, checked against `INT_MAX`, and
only then stored in the integer dimensions vector. The number of stack inputs
must also fit in an R dimension.

The full product of the output dimensions must be checked before allocation.
Do not rely on signed overflow in `rray_size_from_dimensions()`. A checked
helper must return 0 immediately if any dimension is 0, even if large earlier
dimensions would overflow when multiplied. A local helper is enough unless
another pending feature already adds a shared checked-size function.

## Name rules

Names are part of the operation, not an afterthought. The three functions must
form a useful round trip when the inputs already share dimensions.

### Names on non-combine axes

Use the existing broadcast-name rule on every axis that is not being combined:

- An input can contribute names only when its dimension on that axis is kept.
- Names from a dimension that expands are dropped.
- The first eligible input with names wins.
- If no input contributes names on any axis, do not attach a `dimnames`
  attribute.

This is the behavior of `rray_broadcast_names_common()` and should be reused
rather than copied.

### Names on the axis combined by `rray_combine()`

Concatenate axis names in input order.

If every input lacks names on that axis, the output axis is unnamed. If any
input supplies names, allocate the full axis-name vector and use `""` for each
position contributed by an unnamed input. Keep `NA` names as `NA`.

For example:

```text
c("a", "b") + unnamed length 2 + c("e")
-> c("a", "b", "", "", "e")
```

This preserves all available information and matches ordinary R vector
concatenation. A stricter alternative is to drop the complete axis names when
any input is unnamed. Confirm this choice before implementation.

Names of dots do not affect `rray_combine()`. A named input may contribute
many positions, so one dots name has no direct position-preserving meaning.

### Names made by `rray_stack()`

Names of `...` become names on the new axis. This gives:

```r
rray_stack(first = x, second = y, .axis = 2L)
```

a new second axis named `c("first", "second")`.

If no dots are named, the new axis is unnamed. If only some dots are named,
`list2()` supplies empty strings for the others, and that partial name vector
is kept.

Names on all old axes use the common broadcast-name rule. The new axis names
from dots replace any temporary names produced while using the combine engine.

### Names returned by `rray_unstack()`

Names from the removed axis become `names()` on the output list. If the axis
has no names, the list is unnamed. This also applies to a zero-length named
axis, where `character()` is a valid zero-length names attribute.

Names on every surviving axis keep their order and shift left to close the
removed position. The removed names must not remain as singleton dimension
names on the output arrays.

For a named input:

```text
dimensions: [2, 3]
axis names: list(c("r1", "r2"), c("a", "b", "c"))

unstack on axis 2:
- list names: c("a", "b", "c")
- each element dimensions: [2]
- each element axis names: list(c("r1", "r2"))
```

Names on the `dimnames` list itself are not a new concern for these functions.
Follow the behavior of the existing broadcast and remove-axes helpers.

## Round trips

The main identity is:

```r
rray_stack(!!!rray_unstack(x, .axis = axis), .axis = axis)
```

It should reproduce the values, dimensions, type, and axis names of `x` for
every valid axis. It should also restore the removed axis names through the
names of the spliced list.

The other direction is exact when stack does not need to broadcast:

```r
rray_unstack(
  rray_stack(a = x, b = y, .axis = axis),
  .axis = axis
)
```

It returns `list(a = x, b = y)` when `x` and `y` already have the same
dimensions and compatible names.

When stack broadcasts inputs, unstack returns their broadcasted forms. It
cannot recover dimensions of 1 that were expanded, so this is expected:

```text
[2, 1] and [1, 3] stack to common old dimensions [2, 3]
unstack returns two [2, 3] arrays
```

## R interface

Create one R file per public function:

- `R/combine.R`
- `R/stack.R`
- `R/unstack.R`

The wrappers should be thin:

```r
rray_combine <- function(..., .axis) {
  .Call(ffi_rray_combine, list2(...), .axis, environment())
}

rray_stack <- function(..., .axis) {
  .Call(ffi_rray_stack, list2(...), .axis, environment())
}

rray_unstack <- function(x, .axis) {
  .Call(ffi_rray_unstack, x, .axis, environment())
}
```

All three functions need exported roxygen documentation. The docs should
explain the valid `.axis` range for each function.

Add all three topics to the Manipulation section of `_pkgdown.yml`.
`devtools::document()` will update `NAMESPACE` and the generated help files.

Remove `rray_split` from the Manipulation section. Redocumenting must remove
its export from `NAMESPACE`.

## Native interface and files

Add these feature pairs and decl headers:

- `src/combine.c`, `src/combine.h`, `src/decl/combine-decl.h`
- `src/stack.c`, `src/stack.h`, `src/decl/stack-decl.h`
- `src/unstack.c`, `src/unstack.h`, `src/decl/unstack-decl.h`

Remove the old split files:

- `R/split.R`
- `R/split-names.R`
- `src/split.c`
- `src/split.h`
- `src/decl/split-decl.h`
- `src/split-names.c`
- `src/split-names.h`
- `src/decl/split-names-decl.h`
- `tests/testthat/test-split.R`
- `tests/testthat/test-split-names.R`
- `tests/testthat/_snaps/split.md`
- `plans/split-optimize.md`

Update benchmark files that call split:

- Replace the single-axis split cases in `bench/iterator.R` with unstack cases.
- Remove the multi-axis split case from `bench/iterator.R`.
- Remove the split-specific sections from `bench/stride-zero.R`. Add combine
  iterator cases there only if they still answer the stride question that file
  measures.

Remove both split FFI declarations and registrations from `src/init.c`:

```text
ffi_rray_split
ffi_rray_split_names
```

Add FFI declarations and registrations to `src/init.c`:

```text
ffi_rray_combine(ffi_xs, ffi_axis, ffi_frame)
ffi_rray_stack(ffi_xs, ffi_axis, ffi_frame)
ffi_rray_unstack(ffi_x, ffi_axis, ffi_frame)
```

The registration arities are 3 for all three functions.

Add `dot_axis` to `struct rray_args` in `src/arg.h` and initialize it as
`".axis"` in `src/arg.c`. Existing functions should keep using `axis`.

The `.c` files must follow the package's top-down order. FFI wrappers come
first, public internal functions next, then helpers and typed cores in the
order used. The decl include stays last. Do not add source comments.

Headers should declare internal C functions only. Keep FFI declarations in
`src/init.c`.

## Combine implementation

Combine and unstack each need a data-movement engine. Stack prepares views and
calls the combine engine.

### Phase 1: validate and cast

1. Reject an empty input list.
2. Convert `.axis` with `arg_as_int()` and the new `rray_args.dot_axis` tag.
3. Build a dots subscript argument with `new_subscript_arg()`. It should report
   `x` for a named input and `..2` for an unnamed second input.
4. Find the common type across the raw input list.
5. Pass that resolved prototype to `rray_cast_common()`. This avoids finding
   the common type twice. Identity casts keep their data; widening casts make
   ordinary array copies.
6. Read dimensions from the cast arrays and find `D`.
7. Check `.axis` against `D`.

Keep list names through every prepared list so type, dimension, and class
errors continue to name the right dots input.

The public internal `rray_combine()` should do this preparation and then call a
second internal helper that accepts an already normalized, common-type list.
`rray_stack()` can use the same preparation and call that helper after making
its dimension views. This avoids a second type pass when stack delegates to
combine.

### Phase 2: plan dimensions and names

Build the output dimensions and one chunk-dimensions vector per input using the
rules above. Check the combine-axis sum and final size.

Build output names in two parts:

- Call `rray_broadcast_names_common()` with the output dimensions, then clear
  the combine-axis slot. Inputs normally cannot contribute on that axis because
  their dimensions do not match the summed output dimension, but clearing it
  also handles the one-input case.
- Build the concatenated combine-axis names and place them in that slot.

Only allocate an output names list if at least one axis has names.

### Phase 3: allocate and copy

Allocate one output vector in the common type and attach attributes only after
the data copy succeeds.

Compute ordinary column-major strides for the final output once. For each
input:

1. Compute broadcast source strides from its dimensions into its chunk
   dimensions. A source dimension of 1 gets stride 0. Missing trailing axes
   also get stride 0.
2. Use the chunk dimensions as the point space.
3. Use final output strides as the first location space.
4. Use broadcast source strides as the second location space.
5. Start the output location at the cumulative combine-axis offset multiplied
   by the final output stride for that axis.
6. Copy every point, then advance the cumulative offset by this input's
   combine-axis dimension.

This maps a local point in a chunk to both its source value and its final output
position. It handles a broadcast read and a strided output write in one pass.

Build the traversal with `rray_strided_iterator2_plan()`. Do not use
`rray_broadcast_iterator2_plan()`: the final output has a larger dimension on
the combined axis than the current chunk, so it is not a broadcast location
space.

The shape is:

```text
point dimensions: chunk dimensions
location 1:       final output strides
location 2:       broadcast input strides
location 1 start: axis offset * output axis stride
location 2 start: 0
```

Use one typed core per native type. Atomic types write through direct pointers.
Character and list outputs use write barriers. The inputs are already cast to
one common type, so the copy kernels do not need a matrix of source and output
type combinations.

The implementation should allocate no full broadcast input. Apart from small
dimension, name, plan, and wrapper objects, it allocates the final output plus
any input casts required by common type promotion.

## Stack implementation

`rray_stack()` should reuse the combine implementation through metadata-only
views.

1. Reject an empty input list.
2. Convert `.axis` with `arg_as_int()`.
3. Find the common type, then normalize and cast every input to it with the
   same preparation helper used by `rray_combine()`.
4. Find ordinary common broadcast dimensions across the prepared inputs with
   `rray_dimensions_common()`.
5. Check that adding one axis stays within maximum dimensionality.
6. Check `.axis` from 1 through `D + 1`.
7. Wrap each prepared input with `r_wrap()`.
8. Give the wrapper padded dimensions with a singleton inserted at `.axis`.
9. Move its existing axis names to the matching new positions. Leave the new
   axis and padded trailing axes unnamed.
10. Put the wrappers in a list that keeps the original dots names.
11. Call the prepared common-type combine helper on that list and `.axis`.
12. Replace the new axis names with the original dots names, when present.

Adding or moving dimensions of size 1 does not change the underlying flat data
order. The wrappers therefore avoid both copies and full broadcasts.

Do not implement stack as repeated calls to `rray_broadcast()` followed by
combine. That would allocate one full common-size input per dots element.

The combine entry point used by stack should accept an existing error call and
dots argument context. Errors raised inside combine must still point at the
`rray_stack()` call and use the original input names.

## Unstack implementation

`rray_unstack()` replaces `rray_split()`, so move the useful single-axis part
of the split engine into `src/unstack.c`. Do not keep a private split layer or a
`keepdims` option. Unstack only accepts one axis and always removes it.

### Phase 1: validate and plan

1. Normalize and validate `x`.
2. Read its dimensions and require dimensionality 2 or greater.
3. Convert and validate `.axis` as one axis of `x`.
4. Build `out_dimensions` by removing `.axis` from the input dimensions.
5. Build the retained axes by removing `.axis` from the full axis sequence.
6. Compute ordinary column-major strides for `x`.
7. Build an outer iterator plan over `.axis`.
8. Build an inner iterator plan over the retained axes.
9. Compute the size of one output array from `out_dimensions`.
10. Allocate one output array for every position on `.axis`.

The relation to the removed function is:

```text
split   [2, 3, 4] on axis 2 -> 3 arrays of [2, 1, 4]
unstack [2, 3, 4] on axis 2 -> 3 arrays of [2, 4]
```

Only the second form remains public or private after this work.

### Phase 2: copy data

Use a two-stage traversal:

```text
outer plan: selected axis, chooses one output array
inner plan: retained axes, fills that output array from start to end
```

The outer plan has one dimension and uses the input stride for `.axis`. The
inner plan gathers the dimensions and input strides for every retained axis in
their original order. Build both with `rray_strided_iterator_plan()`.

Together the plans visit each input value exactly once. The outer loop selects
one result and one starting input location. The inner loop reads that complete
slice and writes it sequentially into the result.

Do not move the current flat split traversal unchanged. It writes to many
separate output vectors while walking `x`, which causes severe cache conflicts
for common leading-axis shapes. Measurements recorded in
`plans/split-optimize.md` found the one-output-at-a-time traversal 2.5 to 5
times faster in the important slow cases. Fold this design into unstack before
deleting that plan.

The current split typed kernels are still useful as a source for type access
and write-barrier details. Rewrite them around the nested traversal and rename
every macro, typed core, and helper to unstack. Remove all code for multiple
axes and for keeping selected axes with dimension 1.

Use one typed core for each native type. Each outer iteration obtains the data
pointer for one atomic output and fills it sequentially. Character and list
outputs write through the barrier. No shelter containing pointers to every
output is needed. Attach `out_dimensions` when each result array is allocated,
so no later reshape pass is needed.

For atomic types, add a bulk copy path when the inner plan is one contiguous
run with input stride 1. Keep the ordinary strided path for every other shape.
The old measurements found this useful for trailing-axis slices, while the
nested traversal fixes the much larger leading-axis cache problem.

An input axis with dimension 0 allocates an empty output list and performs no
copy. Do not divide the total input size by the selected dimension because that
would divide by zero. Compute each element size from `out_dimensions`.

### Phase 3: attach names

Build retained dimension names once by copying every input axis name except
the selected one. Share that list across all result arrays. If every retained
axis is unnamed, do not attach a `dimnames` attribute.

Set `names(out)` to the selected axis names. No per-output subsetting is
needed because the selected axis no longer exists in an output array.

The old `rray_split_names()` machinery should not move into unstack. It made a
different names list for each output because split kept singleton axes.
Unstack needs one retained names list plus the output list names.

## Removing split

This family replaces `rray_split()`. The implementation pull request must
remove it completely rather than deprecating it.

Remove its R wrappers, native entry points, headers, decl headers, tests,
snapshots, registration, export, and pkgdown entry. Remove
`plans/split-optimize.md` because it plans work on code that no longer exists.
Update the split benchmark sections so every checked-in benchmark still runs.

Some broader plans mention split as an example of a two-location iterator or
of `NULL` handling. Update these references so the repository does not point
at the removed API:

- `plans/implementation.md`
- `plans/n-ary.md`
- `plans/null.md`

Keep the general `rray_strided_iterator2_plan()` support. Combine still needs
two locations, so removing split is not a reason to remove that iterator.

Move useful single-axis test coverage from the split tests into the unstack
tests before deleting the old files. Multiple-axis split behavior, empty
`axes`, and kept singleton axes have no replacement and should disappear.

## Errors and validation

Use snapshots for every error test.

Required cases are:

- no inputs to combine or stack;
- a missing `.axis`;
- `.axis` with length other than 1;
- missing `.axis` value;
- `.axis` with attributes;
- a lossy or impossible cast of `.axis` to integer;
- `.axis < 1`;
- `.axis` above the function's valid range;
- stack output above maximum dimensionality;
- unstack input below dimensionality 2;
- incompatible non-combine dimensions;
- incompatible common types;
- classed input;
- non-array input, including `NULL`;
- combine-axis dimension sum above `INT_MAX` where a practical synthetic test
  can reach the check without allocating the data;
- output size overflow where a practical synthetic test can reach the check.

Errors involving an input should identify the named dots element or its `..n`
position. Errors involving the axis should say `.axis`.

## Test plan

Create:

- `tests/testthat/test-combine.R`
- `tests/testthat/test-stack.R`
- `tests/testthat/test-unstack.R`

Delete the old split test files and snapshot listed above after their useful
single-axis cases have been moved to unstack.

Keep every test inside a `test_that()` block.

### Combine tests

- Combine 1D arrays.
- Combine 2D and 3D arrays along every valid axis.
- Check values through array indexing, not only dimensions.
- Check one input.
- Check more than two inputs.
- Check dots splicing with `!!!`.
- Check common type promotion across logical, integer, double, and complex.
- Check character, raw, and list arrays.
- Check incompatible type families.
- Broadcast dimension 1 on each non-combine axis.
- Broadcast differing dimensionality.
- Include `[2, 1, 3]` with `[3, 2]` on axis 1 and expect `[5, 2, 3]`.
- Combine on an axis missing from a lower-dimensional input.
- Reject an incompatible non-combine axis.
- Check a zero combine-axis dimension.
- Check a zero non-combine dimension, including broadcasting 1 to 0.
- Concatenate complete axis names.
- Fill unnamed chunks with empty names when another chunk is named.
- Keep non-combine names from the first eligible input.
- Drop names from a broadcast dimension.
- Show that dots names do not prefix combine-axis names.
- Check that inputs are not modified.

### Stack tests

- Stack vectors on axes 1 and 2.
- Stack 2D arrays on axes 1, 2, and 3.
- Check exact values by slicing the new axis.
- Check one input and more than two inputs.
- Check all seven native types.
- Check common type promotion.
- Broadcast singleton dimensions.
- Broadcast differing dimensionality.
- Test the documented row and column feature-plane example, including values,
  dimensions, and new-axis names.
- Reject incompatible dimensions.
- Check zero-size inputs.
- Move dots names to the new axis.
- Keep partial dots names with empty strings.
- Leave the new axis unnamed when dots are unnamed.
- Keep old-axis names under broadcasting.
- Check dimensionality 64 is rejected because stack would make 65.
- Check that inputs are not modified.

### Unstack tests

- Unstack a 2D array along both axes.
- Unstack a 3D array along every axis.
- Check dimensions and exact slice values.
- Check all seven native types.
- Check an axis with dimension 0.
- Check an axis with dimension 1.
- Move removed axis names to list names.
- Shift surviving axis names to their new positions.
- Handle partial `dimnames`.
- Reject 1D input.
- Reject invalid axes, classed input, and non-array input.
- Check that `x` is not modified.

### Round-trip tests

For each axis of a named 2D and 3D array, test:

```r
pieces <- rray_unstack(x, .axis = axis)
out <- rlang::inject(rray_stack(!!!pieces, .axis = axis))
expect_identical(out, x)
```

Also unstack a stack of named equal-shape inputs and expect the original named
list. Add one broadcasted stack case and expect the common-size versions rather
than the original singleton shapes.

## Work order

1. Confirm the remaining open public choice about partial names on the combine
   axis.
2. Add `.axis` argument support in `src/arg.c` and `src/arg.h`.
3. Implement and test `rray_combine()` without names.
4. Add combine names and their tests.
5. Implement `rray_stack()` through dimension views and combine.
6. Add stack names and round-trip tests.
7. Implement the direct `rray_unstack()` engine and add its names.
8. Move useful single-axis split tests to unstack, then remove the split API,
   native code, tests, snapshot, and obsolete optimization plan.
9. Update split benchmarks and broader plans that refer to split.
10. Register the new native routines and add all R documentation.
11. Add the three topics to `_pkgdown.yml` and remove `rray_split`.
12. Run the protection audit, formatting, documentation, tests, benchmarks,
    and pkgdown checks below.

Keeping names as a separate step makes data-order failures easier to isolate.
Stack should come after combine because its implementation depends on it.
Unstack can be built independently after the public decisions are fixed.

## C protection audit

Before running any C change, perform the package's required separate protection
pass over the full diff.

For every new or touched `r_obj*`:

1. Name the next function that reads it.
2. Check whether that function can allocate before it protects or consumes the
   value.
3. Add `KEEP()`, `KEEP_HERE()`, or `KEEP_AT()` where needed.
4. Check pointers from `r_int_cbegin()`, `r_list_cbegin()`, and the typed vector
   accessors. Their owner must stay protected across every later allocation.
5. Check metadata views in stack. The wrapped input, inserted dimensions,
   inserted names, and prepared list must all remain protected until combine
   has finished with them.
6. Check shared retained dimensions and names in unstack while attributes are
   attached to each list element.
7. Balance every success-path `FREE()` count. Error paths unwind through R.

Do not use `gctorture()` or `gctorture2()`.

## Verification

After C or R code is generated, run all required formatters:

```sh
clang-format -i src/*.c src/*.h
air format .
```

Then run focused tests:

```sh
Rscript -e "devtools::test(filter = '^(combine|stack|unstack)$')"
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

Run the updated unstack section of `bench/iterator.R` against a build from
before the change. Check leading, middle, and trailing axes. Correctness does
not depend on a fixed timing threshold, but the new traversal should retain the
large improvement already measured for leading-axis slices and should not make
trailing-axis slices materially slower.

If the iterator or a shared dimension helper changes, also run the broadcast,
remove-axes, cast-common, and ptype-common tests directly before the full suite.

## Done means

- All three functions are exported and documented.
- Their axes, dimensions, type, value order, names, zero-size behavior, and
  errors match this plan.
- Combine and stack broadcast without full broadcast intermediates.
- Stack accepts the last insertion point, `D + 1`.
- Unstack rejects 1D input and returns arrays with one fewer axis.
- Stack and unstack pass the stated round trips.
- `rray_split()` and `rray_split_names()` have no remaining R or C entry point.
- Old split files, tests, snapshots, documentation, pkgdown entries, and the
  obsolete split optimization plan are removed.
- Checked-in benchmarks no longer call split, and the new unstack traversal is
  measured on leading, middle, and trailing axes.
- Broader plans no longer refer to split as a live API.
- No input is modified.
- Native routines are registered.
- `_pkgdown.yml` includes all three topics.
- The explicit protection pass is complete.
- All C and R files are formatted.
- Focused tests, full tests, documentation, and pkgdown checks pass.
