# Titles plan

## Goal

Every function that takes one array and returns one array should keep the
titles of `x` wherever its axes keep their meaning. Titles are the names on
the dimnames list:

```r
x <- array(1:24, c(2, 3, 4), dimnames = list(A = c("a", "b"), B = NULL, C = NULL))

rray_titles(x)
# [1] "A" "B" "C"
```

Binary and n-ary functions are out of scope until we settle how names combine
across inputs. See [Out of scope](#out-of-scope).

## Rules

These were decided up front and apply to every step.

- A title labels an axis, not its positions. An axis that keeps its identity
  keeps its title, even when it loses its names. `rray_sum(x, axes = 2)` keeps
  title `"B"` on axis 2.

- The result has a titles vector exactly when `x` has one. A new axis gets
  the title `""`. An all `""` titles vector is never turned into `NULL`. This
  matches `rray_set_axis_title(x, axis, NULL)`, which already leaves `""` in
  place.

- Titles can exist with no axis names at all. The dimnames list is then all
  `NULL` elements, like `list(A = NULL, B = NULL)`. Every helper must handle
  this case, which is the main trap: most names helpers only allocate their
  output list when some axis has names.

- A function that drops all names drops all titles. Today that is
  `rray_extract()`, `rray_index()`, and `rray_set_dimensions()`.

- `rray_set_names(x, names)` replaces the whole dimnames list. The names of
  `names` become the titles of `x`, so an unnamed list drops them and `NULL`
  clears everything.

- `rray_broadcast()` is left alone for now, see [Out of scope](#out-of-scope).

## Current state

From running each function on `x` above:

| Function | Titles today | After this plan |
|---|---|---|
| `rray_slice()`, `rray_slice_axis()`, `rray_slice_rows()`, `rray_slice_columns()` | Dropped | Kept |
| `rray_reverse()`, `rray_roll()`, `rray_rep()`, `rray_rep_each()` | Dropped | Kept |
| `rray_sum()`, `rray_prod()`, `rray_mean()`, `rray_min()`, `rray_max()`, `rray_all()`, `rray_any()` | Dropped | Kept, including on reduced axes |
| `rray_locate_max()`, `rray_locate_min()` | Dropped | Kept, including on the located axis |
| `rray_roll_each()` | Dropped | Kept, including on `axis` |
| `rray_permute_axes()`, `rray_move_axes()` | Dropped | Reordered with the axes |
| `rray_insert_axes()` | Dropped | Kept, `""` on new axes |
| `rray_remove_axes()` | Dropped | Kept, removed axes lose theirs |
| `rray_split()` | Kept only when `axis` has no names | Always kept |
| `rray_unstack()` | Kept only when `axis` has no names | Always kept, minus `axis` |
| `rray_set_axis_names()`, `rray_set_row_names()`, `rray_set_column_names()` | Dropped | Kept |
| `rray_set_names()` | Taken from `names` | Unchanged, now documented and tested |
| `rray_cumulative_*()` | Kept | Unchanged, already tested |
| `rray_cast()` | Kept | Unchanged, add a test |
| `rray_slice_assign()` and friends, `rray_extract_assign()` | Kept | Unchanged, add a test |
| `rray_extract()`, `rray_index()`, `rray_set_dimensions()` | Dropped | Unchanged, add a test |
| `rray_broadcast()` | Kept only on a no-op | Unchanged |

## Implementation pattern

Each step adds titles to an existing names helper rather than adding a new
code path. The pattern is the same everywhere:

1. Read `x_titles = r_names(x_names)` next to the existing `x_names`. It is
   protected through `x`.

2. If `x_titles != r_null`, allocate the output names list up front instead of
   lazily, so a titles-only input still gets a list.

3. Either reuse `x_titles` unchanged (when the result has the same axes as
   `x`) or build an `out_titles` character vector in the same loop that builds
   the output names (when axes move, appear, or disappear).

4. Poke the titles onto the output list with `r_attrib_poke_names()`.

`r_alloc_character()` fills with `""`, so new axes need no explicit poke.

Per the `AGENTS.md` protection review, `out_titles` is allocated while the
output list is live, so `KEEP()` it until it is poked onto the list.

## Steps

Each step is one pull request, merged before the next one starts. Every step
adds a `"titles are kept"` test next to the existing names tests for each
function it touches, following the existing cumulative tests. Each test covers
two inputs:

```r
x <- array(1:24, c(2, 3, 4), dimnames = list(A = c("a", "b"), B = NULL, C = NULL))
y <- array(1:24, c(2, 3, 4), dimnames = list(A = NULL, B = NULL, C = NULL))
```

`y` is the titles-only case. It catches helpers that skip allocating their
output list when no axis has names.

Where a function's roxygen already describes what happens to names, add one
short sentence about titles next to it.

### 1. Slice names

`rray_slice_names()` in `src/slice.c` covers every function built on
`rray_slice()`:

- `rray_slice()`

- `rray_slice_axis()`, `rray_slice_rows()`, `rray_slice_columns()`

- `rray_reverse()`

- `rray_roll()`

- `rray_rep()`, `rray_rep_each()`

Slicing never adds, removes, or moves axes, so the titles of `x` are reused
unchanged. `rray_slice_names()` takes `x_names` instead of `v_x_names` and
pokes `r_names(x_names)` onto its output. It already allocates its output
whenever `x_names` exists, so the titles-only case needs no extra work.

```r
rray_titles(rray_slice(x, 1, TRUE, TRUE))
# [1] "A" "B" "C"

rray_titles(rray_rep(x, times = 2, axes = 1))
# [1] "A" "B" "C"
```

Tests go in `test-slice.R`, `test-reverse.R`, `test-roll.R`, and
`test-rep.R`.

### 2. Reduce names

`rray_reduce_names()` in `src/reduce-names.c` covers:

- `rray_sum()`, `rray_prod()`, `rray_mean()`, `rray_min()`, `rray_max()`,
  `rray_all()`, `rray_any()`

- `rray_locate_max()`, `rray_locate_min()`

- `rray_roll_each()`

Reduced axes stay at dimension 1, so the result has the same axes as `x` and
reuses its titles unchanged. The output list is allocated lazily today, so it
moves up front when `x_titles` exists.

```r
rray_titles(rray_sum(x, axes = 2))
# [1] "A" "B" "C"

rray_names(rray_sum(y, axes = 2))
# $A
# NULL
#
# $B
# NULL
#
# $C
# NULL
```

`rray_reduce_names()` has its own internal tests in `test-reduce-names.R`. Add
titles cases there, and one test per family in `test-reduce-sum.R`,
`test-reduce-mean.R`, `test-reduce-prod.R`, `test-reduce-extremum.R`,
`test-reduce-logical.R`, `test-locate.R`, and `test-roll.R`.

### 3. Axis reshaping

These move axes around, so each builds `out_titles` in the loop it already
uses for names.

#### `rray_permute_axes()`

`rray_permute_axes_names()` in `src/permute-axes.c` already allocates its
output whenever `x_names` exists. Add the titles in the same loop:

```text
out_titles[i] = x_titles[axes[i] - 1]
```

```r
rray_titles(rray_permute_axes(x, c(3, 1, 2)))
# [1] "C" "A" "B"
```

#### `rray_move_axes()`

Built on `rray_permute_axes()`, so it needs no change. Add a test only.

```r
rray_titles(rray_move_axes(x, from = 1, to = 3))
# [1] "B" "C" "A"
```

#### `rray_insert_axes()`

In `src/insert-axes.c`, inserted axes keep the `""` from
`r_alloc_character()` and every other axis copies `x_titles[x_i]`. Allocate
`out_names` up front when `x_titles` exists.

```r
rray_titles(rray_insert_axes(x, 2))
# [1] "A" ""  "B" "C"
```

#### `rray_remove_axes()`

In `src/remove-axes.c`, surviving axes copy `x_titles[i]` to `out_i`. A
removed axis loses its title silently, the same as its names. Allocate
`out_names` up front when `x_titles` exists.

```r
z <- rray_set_axis_title(rray_insert_axes(x, 2), 2, "Z")

rray_titles(rray_remove_axes(z, 2))
# [1] "A" "B" "C"
```

Tests go in `test-permute-axes.R`, `test-move-axes.R`, `test-insert-axes.R`,
and `test-remove-axes.R`.

### 4. Split and unstack

#### `rray_split()`

In `src/split.c`, the branch without names on `axis` reuses `x_names` whole,
so titles survive. The branch with names on `axis` builds a new list in
`rray_split_elt_names()` and drops them. Poke `x_titles` onto that list. Every
element has the same axes as `x`, so the titles are reused unchanged.

```r
lapply(rray_split(x, axis = 1, dimensions = 1), rray_titles)
# [[1]]
# [1] "A" "B" "C"
#
# [[2]]
# [1] "A" "B" "C"
```

#### `rray_unstack()`

Built on `rray_split()` and `rray_remove_axes()`. After step 3 and the split
fix, every element keeps the titles of `x` minus `axis`. No code change.

```r
lapply(rray_unstack(x, axis = 1), rray_titles)
# $a
# [1] "B" "C"
#
# $b
# [1] "B" "C"
```

Tests go in `test-split.R` and `test-unstack.R`.

### 5. Names setters

#### `rray_set_axis_names()`, `rray_set_row_names()`, `rray_set_column_names()`

In `src/names.c`, `rray_set_axis_names()` copies the old names into a fresh
list and drops the titles. Poke `r_names(old_names)` onto `new_names` when
`old_names` exists. The row and column shortcuts share this function.

```r
rray_titles(rray_set_axis_names(x, 2, c("c", "d", "e")))
# [1] "A" "B" "C"
```

#### `rray_set_names()`

No code change. It already uses the names of `names` as the titles. Document
that on `@param names` and add tests:

```r
rray_titles(rray_set_names(x, list(NULL, NULL, NULL)))
# NULL

rray_titles(rray_set_names(x, list(D = NULL, E = NULL, F = NULL)))
# [1] "D" "E" "F"
```

Tests go in `test-names.R`.

### 6. Pin the remaining single-array functions

No code changes. Add tests so these do not regress.

Already keep titles:

- `rray_cast()` in `test-cast.R`.

- `rray_slice_assign()`, `rray_slice_assign_axis()`, `rray_slice_assign_rows()`,
  `rray_slice_assign_columns()` in `test-slice.R`.

- `rray_extract_assign()` in `test-extract.R`.

The cumulative functions already have `"titles are kept"` tests.

Drop all titles along with all names:

- `rray_extract()` in `test-extract.R`.

- `rray_index()` in `test-index.R`.

- `rray_set_dimensions()` in `test-dimensions.R`, including the case where the
  new dimensions equal the old ones.

## Out of scope

- `rray_broadcast()`. Its names helper, `rray_broadcast_names_fill()`, is
  shared with the binary and n-ary helpers. It changes together with them once
  the n-ary names design is settled. The existing `"dimension titles are
  lost"` test in `test-broadcast.R` stays as is.

- Binary and n-ary functions: arithmetic, comparison, `rray_equal()`, logical
  operators, `rray_pmax()`, `rray_pmin()`, `rray_if_else()`,
  `rray_combine()`, and `rray_stack()`.

- `rray_expand_dimensionality()`. It is internal and only used by
  `rray_stack()`.

## Checks for each step

1. Run the explicit protection review from `AGENTS.md` over the diff.

2. Run `clang-format -i src/*.c src/*.h`.

3. Run `air format .`.

4. Run `devtools::document()` if any roxygen changed.

5. Run the focused tests, then `devtools::test()`.

## Progress

- [x] 1. Slice names

- [ ] 2. Reduce names

- [ ] 3. Axis reshaping

- [ ] 4. Split and unstack

- [ ] 5. Names setters

- [ ] 6. Pin the remaining single-array functions
