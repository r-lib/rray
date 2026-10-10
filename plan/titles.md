# Array titles

Titles are the names on an array's `dimnames` list. They label axes in base
R's array print output. Element names remain the values inside that list.

```r
x <- matrix(1:6, 2, 3, dimnames = list(Row = NULL, Column = NULL))
rray_titles(x)
rray_axis_title(x, 2)
```

These calls return `c("Row", "Column")` and `"Column"`.

## API

- `rray_titles(x)` returns `names(rray_names(x))`. It returns `NULL` when the
  dimension names list has no names. A plain vector uses the same one-axis
  array view as `rray_names()`.
- `rray_set_titles(x, titles)` accepts `NULL` or a character vector with one
  entry per axis. `NULL` removes all titles. An empty string leaves that axis
  without a title. If every entry is empty, remove the names attribute. It
  keeps every axis's element names.
- `rray_axis_title(x, axis)` returns `NULL` when there is no titles vector.
  Otherwise, it returns `rray_titles(x)[[axis]]` as is, including `""` or
  `NA_character_`.
- `rray_set_axis_title(x, axis, title)` accepts one character string or
  `NULL`. `NULL` and `""` remove that axis's title. It keeps titles and element
  names on the other axes.

Follow the existing names API for bare array input, one-based axis validation,
and conversion of plain vectors to one-dimensional arrays. Reject values of
the wrong type or length without implicit character conversion. The getters
may return an existing `NA_character_` title, as base R permits it in names.

```r
x <- matrix(1:6, 2, 3)
y <- rray_set_axis_title(x, 2, "Column")
rray_titles(y)
rray_axis_title(y, 1)
dimnames(y)
```

`rray_titles(y)` returns `c("", "Column")`, and `rray_axis_title(y, 1)` returns
`""`. `dimnames(y)` is a list of two `NULL` entries with those titles.

## Implementation

1. Add `R/titles.R` with the four exported functions and roxygen topics.
   Add the topics to the Names section of `_pkgdown.yml`.
2. Add `src/titles.c` and `src/titles.h`. Put thin FFI wrappers before the
   internal functions. Use `src/decl/titles-decl.h` for any helpers needed
   after the entry points, and include it last. Register the wrappers in
   `src/init.c`. Add `titles` and `title` to `rray_args` for errors.
3. Read titles from the names attribute of the dimension names list. Validate
   `x` and `axis` through the same paths as `src/names.c`.
4. For a non-`NULL` setter value, create a dimension names list of length
   `rray_dimensionality(x)` when none exists. Its entries can all be `NULL`:
   the list's titles alone are enough to make it useful. Copy an existing
   list before changing its names attribute, so the input is untouched.
5. For one-axis updates, copy the existing title vector or make a vector of
   empty strings, then replace one entry. Both setters remove the names
   attribute when no titles remain. Removing a title from an array with no
   dimension names must not create a list. Wrap `x` to change attributes
   without copying its data, as the names setters do.
6. Limit behavior changes to the four title functions. Leave existing names
   functions and other array operations as they are. Title handling in those
   operations belongs in future plans and PRs.
7. Make a separate protection pass over every new or touched `r_obj*`, then
   run `clang-format -i src/*.c src/*.h`, `air format .`, and
   `devtools::document()`.

## Tests

Add `tests/testthat/test-titles.R` with focused tests for:

- Untitled arrays, title-only arrays, partly titled arrays, named vectors,
  and one-dimensional arrays. Check that an empty axis title is returned as
  `""` when the titles vector exists.
- Both setters when dimension names are absent, and preservation of element
  names and the input object when dimension names are present.
- Removing one or all titles, including the last title, while keeping axis
  element names.
- Invalid array inputs, axes, types, and lengths. Snapshot only package
  errors.
- A title-only matrix in base R's printed output, to confirm the underlying
  representation reaches the standard print method.

Run the titles and names tests, then the full test suite and
`pkgdown::check_pkgdown()`.
