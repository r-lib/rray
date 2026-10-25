# Reverse elements along axes

`rray_reverse()` reverses the order of the elements of `x` along one or
more `axes`.

## Usage

``` r
rray_reverse(x, ..., axes)
```

## Arguments

- x:

  An array.

- ...:

  These dots are for future extensions and must be empty.

- axes:

  The axes to reverse along, given [by position or by
  title](https://rray.r-lib.org/dev/reference/faq-axes.md).

## Value

An array with the same type and dimensions as `x`.

## Details

Names on a reversed axis move with the data. Every other axis keeps its
names untouched. The titles of `x` are kept.

## Examples

``` r
rray_reverse(1:5, axes = 1)
#> [1] 5 4 3 2 1

x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))

# Reverse the rows, names move with the data
rray_reverse(x, axes = 1)
#>    a b c
#> r2 2 4 6
#> r1 1 3 5

# Reverse the columns
rray_reverse(x, axes = 2)
#>    c b a
#> r1 5 3 1
#> r2 6 4 2

# Reverse both axes
rray_reverse(x, axes = c(1, 2))
#>    c b a
#> r2 6 4 2
#> r1 5 3 1

# Reverse the columns, by title
y <- rray_set_titles(x, c("row", "col"))
rray_reverse(y, axes = "col")
#>     col
#> row  c b a
#>   r1 5 3 1
#>   r2 6 4 2
```
