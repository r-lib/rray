# Names and titles across several inputs

## Goal

When a function builds one result from several inputs, the inputs must agree
on names and titles. If they don't, the function errors instead of silently
picking one input's names.

```r
jan <- array(1:2, 2, dimnames = list(c("north", "south")))
feb <- array(3:4, 2, dimnames = list(c("south", "north")))

rray_stack(jan = jan, feb = feb, .axis = 2)
#> Error in `rray_stack()`:
#> ! Can't find common names at axis 1. `jan` and `feb` have different names at position 1: "north" and "south".
```

Today this succeeds, labels every row with the order of `jan`, and the `feb`
column is silently wrong.

There is no argument to choose another behavior. To resolve a conflict, remove
the names from one input with `rray_set_names()`, which returns a view and
does not copy the data.

At the same time, titles stop being dropped by these functions.

## Which functions keep names

A function that takes values and returns the same kind of value keeps names.
A function that returns a verdict about its inputs drops them.

- `rray_add()` takes numbers and returns numbers. The result is a new value at
  the same labeled position, so the names still describe it.

- `rray_and()` takes logicals and returns logicals. Two masks over rows `a`
  and `b` combine into a mask over rows `a` and `b`.

- `rray_equal()` takes anything and returns logicals. The result answers a
  question about how the inputs relate, so it is not labeled like either one.

The rule also decides what a name conflict means. Two masks with different
row names are misaligned, so `rray_and()` errors. Comparing two arrays whose
names differ is an ordinary question, so `rray_equal()` answers it.

`rray_if_else()` is left out of this rule and keeps dropping names.

## Functions in scope

| Function | Names | Titles |
|---|---|---|
| `rray_add()`, `rray_subtract()`, `rray_multiply()`, `rray_divide()`, `rray_exponentiate()` | Must agree | Must agree, kept |
| `rray_and()`, `rray_or()`, `rray_xor()` | Must agree | Must agree, kept |
| `rray_pmax()`, `rray_pmin()` | Must agree | Must agree, kept |
| `rray_combine()` | Must agree, except on `.axis` | Must agree, kept |
| `rray_stack()` | Must agree, new axis from the names of `...` | Must agree, kept, `""` on the new axis |
| `rray_broadcast()`, `rray_broadcast_common()` | One input at a time, never errors | Kept, including on broadcast axes |
| `rray_equal()`, `rray_not_equal()`, `rray_greater_than()`, `rray_greater_than_or_equal()`, `rray_less_than()`, `rray_less_than_or_equal()` | Dropped | Dropped |
| `rray_if_else()` | Already dropped, unchanged | Already dropped, unchanged |

## Names rules

For each axis of the result:

- An input has a say only if it has names on that axis and its dimension
  there is not broadcast. An axis of dimension 1 that stays 1 is not
  broadcast.

- An input that is missing the axis, because it has a lower dimensionality,
  has no say.

- Every input with a say must have exactly the same names. There are no
  holes: `c("r1", "")` and `c("r1", "r2")` disagree. `NA` agrees only with
  `NA`.

- If they agree, the result has those names. If no input has a say, the axis
  has no names.

- The combine axis of `rray_combine()` is not checked. Its names are pasted
  together from every input, with `""` for unnamed inputs, as today.

- The new axis of `rray_stack()` is not checked. Its names come from the names
  of `...`, as today.

## Titles rules

Titles are the names on the dimnames list.

- An empty title, or no titles at all, has no say.

- Every other title on an axis must agree, including titles from inputs that
  broadcast on that axis or have no names there. A title labels the axis, not
  its positions, so it survives broadcasting.

- Because all non-empty titles must agree, the title of an axis is that one
  agreed title, or `""` when nobody has one.

- The result has a titles vector exactly when some input has one.

- The combine axis of `rray_combine()` is checked like any other axis.

- The new axis of `rray_stack()` gets the title `""`.

- `rray_broadcast()` keeps every title of `x`, so it no longer depends on
  whether any broadcasting happened.

## Errors

Inputs are visited left to right, and the axes of each input left to right.
Names are checked before titles on each axis. The first disagreement errors.

The error names the first input that had a say on the axis and the first input
that disagrees with it. For names, it reports the first position that differs.
The wording mirrors the existing `Can't find common dimensions` error in
`src/dimensions.c`.

```text
Can't find common names at axis {axis}. {x_arg} and {y_arg} have different names at position {position}: "{x_name}" and "{y_name}".

Can't find common titles at axis {axis}. {x_arg} has title "{x_title}" and {y_arg} has title "{y_title}".
```

An `NA` name is written without quotes, as `NA`.

Names are compared by `CHARSXP` pointer. The same text in two different
encodings compares as different, which is accepted.

## Examples

The outputs below are the intended behavior.

### 1. Identical names pass, different names error

```r
x <- array(1:2, 2, dimnames = list(c("r1", "r2")))
y <- array(1:2, 2, dimnames = list(c("a", "b")))

rray_add(x, x)
#> r1 r2
#>  2  4

rray_add(x, y)
#> Error in `rray_add()`:
#> ! Can't find common names at axis 1. `x` and `y` have different names at position 1: "r1" and "a".

rray_add(x, rray_set_names(y, NULL))
#> r1 r2
#>  2  4
```

### 2. An input without names has no say

```r
x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2"), NULL))
y <- array(5:8, c(2, 2), dimnames = list(NULL, c("c1", "c2")))

rray_add(x, y)
#>    c1 c2
#> r1  6 10
#> r2  8 12
```

Each input names a different axis, so nothing conflicts and both are kept.

### 3. A broadcast axis has no say on names

```r
x <- array(1:3, c(1, 3), dimnames = list("total", c("a", "b", "c")))
y <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("a", "b", "c")))

rray_multiply(x, y)
#>    a b  c
#> r1 1 6 15
#> r2 2 8 18
```

`"total"` names a single row that gets repeated, so it is dropped rather than
compared. Axis 2 is compared, and agrees.

### 4. Empty names count, there are no holes

```r
x <- array(1:2, 2, dimnames = list(c("r1", "")))
y <- array(1:2, 2, dimnames = list(c("r1", "r2")))

rray_add(x, y)
#> Error in `rray_add()`:
#> ! Can't find common names at axis 1. `x` and `y` have different names at position 2: "" and "r2".
```

### 5. A named vector is a 1D array with names

```r
x <- array(1:4, c(2, 2), dimnames = list(c("r1", "r2"), c("c1", "c2")))

rray_multiply(x, c(a = 10, b = 20))
#> Error in `rray_multiply()`:
#> ! Can't find common names at axis 1. `x` and `y` have different names at position 1: "r1" and "a".

rray_multiply(x, c(10, 20))
#>    c1 c2
#> r1 10 30
#> r2 40 80
```

### 6. Titles must agree, even when the names do

```r
x <- array(1:2, 2, dimnames = list(Row = c("r1", "r2")))
y <- array(1:2, 2, dimnames = list(Sample = c("r1", "r2")))
z <- array(1:2, 2, dimnames = list(c("r1", "r2")))

rray_add(x, y)
#> Error in `rray_add()`:
#> ! Can't find common titles at axis 1. `x` has title "Row" and `y` has title "Sample".

rray_titles(rray_add(x, z))
#> [1] "Row"
```

`z` has no title, so it has no say.

### 7. A broadcast input still has a say on titles

```r
x <- array(1:3, c(1, 3), dimnames = list(Row = "total", Col = c("a", "b", "c")))
y <- array(1:6, c(2, 3), dimnames = list(Sample = c("r1", "r2"), Col = c("a", "b", "c")))

rray_multiply(x, y)
#> Error in `rray_multiply()`:
#> ! Can't find common titles at axis 1. `x` has title "Row" and `y` has title "Sample".
```

The names on axis 1 are fine because `x` broadcasts there, but the titles
conflict.

### 8. Combining three inputs

```r
a <- array(1:2, c(1, 2), dimnames = list("r1", c("x", "y")))
b <- array(3:4, c(1, 2))
c <- array(5:6, c(1, 2), dimnames = list("r3", c("x", "z")))

rray_combine(a, b, c, .axis = 1)
#> Error in `rray_combine()`:
#> ! Can't find common names at axis 2. `..1` and `..3` have different names at position 2: "y" and "z".
```

`b` has no names, so it is skipped. With `c` named `c("x", "y")`:

```r
#>    x y
#> r1 1 2
#>    3 4
#> r3 5 6
```

The combine axis is never checked. Its names are pasted together, with `""`
for `b`.

### 9. Stacking data that was reordered

```r
jan <- array(1:2, 2, dimnames = list(c("north", "south")))
feb <- array(3:4, 2, dimnames = list(c("south", "north")))

rray_stack(jan = jan, feb = feb, .axis = 2)
#> Error in `rray_stack()`:
#> ! Can't find common names at axis 1. `jan` and `feb` have different names at position 1: "north" and "south".
```

The new axis gets its names from the names of `...`, which are never checked.

### 10. Logical functions check names, comparisons drop them

```r
x <- array(c(TRUE, FALSE), 2, dimnames = list(c("r1", "r2")))
y <- array(c(TRUE, TRUE), 2, dimnames = list(c("a", "b")))

rray_and(x, x)
#>    r1    r2
#>  TRUE FALSE

rray_and(x, y)
#> Error in `rray_and()`:
#> ! Can't find common names at axis 1. `x` and `y` have different names at position 1: "r1" and "a".

rray_equal(x, y)
#> [1]  TRUE FALSE
```

`rray_and()` returns the same kind of value it takes, so it follows the same
rules as `rray_add()`. `rray_equal()` returns a verdict about `x` and `y`, so
it returns no names and no titles, like `vctrs::vec_equal()`.

## Implementation

### Where names are built today

| Caller | Helper |
|---|---|
| `rray_broadcast()` in `src/broadcast.c` | `rray_broadcast_names()` |
| `src/arithmetic.c`, `src/extremum.c`, `src/logical.c` | `rray_broadcast_names2()` |
| `rray_combine_names()` in `src/combine.c` | `rray_broadcast_names_common_opts()` |

All of these live in `src/broadcast-names.c`. Today they implement first wins
per axis through `rray_broadcast_names_fill()`, and drop titles.

`rray_stack()` goes through `rray_combine()`, after passing each input through
`rray_expand_dimensionality()` in `src/dimensionality.c` and
`rray_insert_axes()`. Both keep titles.

`rray_broadcast_common()` calls `rray_broadcast()` once per input.

### Design

Mirror `rray_dimensions_common_opts()` and `rray_dimensions_merge()` in
`src/dimensions.c`, which already solve the same problem for dimensions: visit
inputs in order, remember per axis which input set the current value
(`v_out_args`), and build the error from two `rray_arg`s.

- Replace `rray_broadcast_names_fill()` with a merge step that takes the
  input, its index, its `rray_arg`, the `rray_arg` and index pointer for the
  input that set each axis, and the `error_call`. It keeps two per axis
  arrays of size `RRAY_MAX_DIMENSIONALITY`: the index of the input that set
  the names, and the index of the input that set the title.

- `rray_broadcast_names_common_opts()` gains `arg` and `error_call`, and uses
  `new_subscript_arg()` twice, exactly like `rray_dimensions_common_opts()`.
  This is what makes the errors say `..1`, `..3`, `jan`, and `feb`.

- `rray_broadcast_names2()` gains `x_arg`, `y_arg`, and `error_call`. It must
  not allocate a list of inputs just to reuse the common path: call the merge
  step twice.

- `rray_broadcast_names()` stays single input. It can never error, but goes
  through the same merge step so its title handling is identical.

- Ignored axes, the combine axis, skip the names check but not the titles
  check.

- The output list is allocated as soon as any input has names or titles. A
  titles only input like `list(A = NULL, B = NULL)` has no names to trigger a
  lazy allocation, which is the trap from the single array titles work.

- Titles go in a separate character vector, allocated with
  `r_alloc_character()` so untitled axes are already `""`, and are poked onto
  the output list with `r_attrib_poke_names()`.

- In `src/arithmetic.c`, `src/extremum.c`, `src/logical.c`, and
  `src/combine.c`, the names are still computed after the result is filled.
  Failing fast is not worth breaking the shared ordering of these functions.

- `rray_stack()` checks names on its original inputs, before the new axis is
  inserted, so the axis in the error matches the axis in the dimension errors.
  The check inside `rray_combine()` then can't fail.

- In `src/compare.c` and `src/equal.c`, remove the `rray_broadcast_names2()`
  call and its include.

- `rray_combine_names()` keeps pasting names on the combine axis. When it
  pokes them into the list, the titles already on that list must survive.

- `rray_expand_dimensionality()` copies the titles of `x`, with `""` on the
  new axes.

The internal R wrappers in `R/broadcast-names.R` exist for tests. They pass
`environment()` so errors have a call, and their FFI wrappers build the
`error_call` from the frame like every other FFI wrapper.

### Existing tests that change

- `test-arithmetic-*.R`: `"\`x\` wins over \`y\` on an axis they both name"`
  becomes an error snapshot.

- `test-logical.R`: `"coalesces names across inputs"` becomes an error
  snapshot.

- `test-compare.R`, `test-equal.R`: `"coalesces names across inputs"` becomes
  a test that names and titles are dropped.

- `test-combine.R`: the `z` case in `"names on the combine axis never come
  from broadcasting"` has columns `c("c", "d")` against `c("a", "b")`, and
  becomes an error.

- `test-broadcast.R`: `"dimension titles are lost"` is rewritten. Titles are
  now kept on broadcast axes.

- `test-broadcast-names.R`: the `"prefers \`x\`"` and `"first input to keep an
  axis win"` tests become error snapshots.

Every new behavior gets a test next to the existing names tests of the
function, using the examples above. Each title test also covers a titles only
input with no names at all.

### Documentation that changes

- `R/arithmetic.R`, `R/extremum.R`, `R/logical.R`, `R/combine.R`,
  `R/stack.R`: describe the rule in `@details` in a few plain sentences, and
  show the `rray_set_names()` fix in an example.

- `R/compare.R`, `R/equal.R`: `@returns` says the result has no names.

- `README.Rmd`: the limitation bullet about dimension name handling being
  experimental is replaced with the rule, then `README.md` is re-rendered.

- `plans/titles.md` and this plan are deleted in the last pull request.

### Checks for each pull request

1. Run the explicit protection review from `AGENTS.md` over the diff.

2. Run `clang-format -i src/*.c src/*.h`.

3. Run `air format .`.

4. Run `devtools::document()` if any roxygen changed.

5. Run the focused tests, then `devtools::test()`.

## Checklist

Each item is one pull request, merged before the next starts.

- [x] 1. `rray_expand_dimensionality()` keeps titles, with `""` on new axes.

- [x] 2. Comparison functions drop names and titles, with docs and tests.

- [x] 3. Names must agree. Rework `src/broadcast-names.c` around the merge
  step, thread `rray_arg`s and `error_call` through every caller, and update
  tests, docs, and the README.

- [ ] 4. Titles must agree and are kept. Add titles to the merge step for
  `rray_broadcast()`, `rray_broadcast_common()`, arithmetic, logical,
  `rray_pmax()`, `rray_pmin()`, `rray_combine()`, and `rray_stack()`. Rewrite the broadcast
  titles test, then delete `plans/titles.md` and this plan.
