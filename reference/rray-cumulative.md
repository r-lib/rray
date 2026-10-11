# Accumulate along an axis

- `rray_cumulative_all()` checks if all values are `TRUE` so far along
  `axis`.

- `rray_cumulative_any()` checks if any value is `TRUE` so far along
  `axis`.

## Usage

``` r
rray_cumulative_all(x, axis)

rray_cumulative_any(x, axis)
```

## Arguments

- x:

  An array.

- axis:

  A single integer. The axis to accumulate along. `1` accumulates down
  rows, `2` accumulates across columns, and so on.

## Value

An array with the same dimensions and names as `x`.

## Details

Each position along `axis` summarizes every position up to and including
it.

## All / Any

`x` must be logical. Missing values follow
[`all()`](https://rdrr.io/r/base/all.html) and
[`any()`](https://rdrr.io/r/base/any.html), so a later `FALSE` or `TRUE`
can settle an earlier `NA`:

    rray_cumulative_all(c(TRUE, NA, FALSE), 1L) # TRUE, NA, FALSE
    rray_cumulative_any(c(FALSE, NA, TRUE), 1L) # FALSE, NA, TRUE

## Examples

``` r
x <- array(c(TRUE, TRUE, FALSE, TRUE, NA, TRUE), c(2L, 3L))
x
#>      [,1]  [,2] [,3]
#> [1,] TRUE FALSE   NA
#> [2,] TRUE  TRUE TRUE

# Down rows
rray_cumulative_all(x, 1L)
#>      [,1]  [,2] [,3]
#> [1,] TRUE FALSE   NA
#> [2,] TRUE FALSE   NA
rray_cumulative_any(x, 1L)
#>      [,1]  [,2] [,3]
#> [1,] TRUE FALSE   NA
#> [2,] TRUE  TRUE TRUE

# Across columns
rray_cumulative_all(x, 2L)
#>      [,1]  [,2]  [,3]
#> [1,] TRUE FALSE FALSE
#> [2,] TRUE  TRUE  TRUE
rray_cumulative_any(x, 2L)
#>      [,1] [,2] [,3]
#> [1,] TRUE TRUE TRUE
#> [2,] TRUE TRUE TRUE
```
