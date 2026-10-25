# Move array axes

`rray_move_axes()` takes axes `from` `x` and moves them `to` axes in the
output. The axes that don't move keep their relative order, and fill in
the locations that are left over.

## Usage

``` r
rray_move_axes(x, ..., from, to)
```

## Arguments

- x:

  An array.

- ...:

  These dots are for future extensions and must be empty.

- from:

  The axes to move, given [by
  position](https://rray.r-lib.org/dev/reference/faq-axes.md).

- to:

  The axes to move to, given [by position in the
  output](https://rray.r-lib.org/dev/reference/faq-axes.md).

## Value

An array.

## Details

Names and titles travel with their axis to its new position.

## See also

[`rray_permute_axes()`](https://rray.r-lib.org/dev/reference/rray_permute_axes.md)

## Examples

``` r
x <- array(1:24, c(2, 3, 4))

# Move the first axis to the end
# (2, 3, 4) -> (3, 4, 2)
rray_dimensions(rray_move_axes(x, from = 1, to = 3))
#> [1] 3 4 2

# Move the last axis to the front
# (2, 3, 4) -> (4, 2, 3)
rray_dimensions(rray_move_axes(x, from = 3, to = 1))
#> [1] 4 2 3

# Move two axes at once, the axis that stays fills in what is left
# (2, 3, 4) -> (4, 2, 3)
rray_dimensions(rray_move_axes(x, from = c(1, 3), to = c(2, 1)))
#> [1] 4 2 3

# Swap the first two axes
# (2, 3, 4) -> (3, 2, 4)
rray_dimensions(rray_move_axes(x, from = c(1, 2), to = c(2, 1)))
#> [1] 3 2 4
```
