# FAQ - How do I refer to an axis?

Functions that work along an axis typically allow you to provide it by
position or by title.

## Positions

A position counts axes from `1`. Axis `1` runs down the rows, axis `2`
runs across the columns, and so on.

    x <- array(
      1:24,
      dim = c(2, 3, 4),
      dimnames = list(row = NULL, col = NULL, page = NULL)
    )

    rray_dimensions(rray_sum(x, axes = 1))
    #> [1] 1 3 4

## Titles

Titles are the names of the dimension names list, like `row` in
`list(row = NULL)`.
[`rray_titles()`](https://rray.r-lib.org/dev/reference/rray-titles.md)
returns them. A title works most anywhere a position does:

    rray_titles(x)
    #> [1] "row"  "col"  "page"

    identical(rray_sum(x, axes = "row"), rray_sum(x, axes = 1))
    #> [1] TRUE

    identical(rray_sum(x, axes = c("page", "row")), rray_sum(x, axes = c(1, 3)))
    #> [1] TRUE

## Positions in the output

Some arguments specify a position in the output rather than an axis of
`x`, like `axes` in
[`rray_insert_axes()`](https://rray.r-lib.org/dev/reference/rray_insert_axes.md)
and `.axis` in
[`rray_stack()`](https://rray.r-lib.org/dev/reference/rray_stack.md).
That axis doesn’t exist yet, so it has no title, and these arguments
only take positions:

    rray_dimensions(rray_insert_axes(x, axes = 1))
    #> [1] 1 2 3 4

    rray_insert_axes(x, axes = "row")
    #> Error in `rray_insert_axes()`:
    #> ! `axes` must be an integer or double vector, not the string "row".
