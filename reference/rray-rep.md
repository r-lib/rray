# Repeat an array

- `rray_rep()` repeats whole axes of `x` in bulk, i.e. repeat all of the
  columns 5 `times`. It can repeat several `axes` at once, with one
  `times` per axis.

- `rray_rep_each()` repeats each individual element of the `axis` of `x`
  separately, i.e. repeat the first column 2 `times`, the second column
  3 `times`, and so on. For convenience, you can also provide a single
  number to repeat each element along the `axis` the same number of
  times.

These are the array versions of
[`vctrs::vec_rep()`](https://vctrs.r-lib.org/reference/vec-rep.html) and
[`vctrs::vec_rep_each()`](https://vctrs.r-lib.org/reference/vec-rep.html),
which repeat along the size of a vector rather than along an axis.

## Usage

``` r
rray_rep(x, ..., times, axes)

rray_rep_each(x, ..., times, axis)
```

## Arguments

- x:

  An array.

- ...:

  These dots are for future extensions and must be empty.

- times:

  For `rray_rep()`, a vector of integers greater than or equal to 0,
  either size 1 or the size of `axes`.

  For `rray_rep_each()`, a vector of integers greater than or equal
  to 0. It is recycled to the dimension of `x` along `axis`.

- axes:

  An integer vector of axes to repeat along.

- axis:

  A single integer representing the axis to repeat along.

## Value

An array with the same dimensions as `x`, except along the repeated
axes.

## Details

Names repeat alongside the data, so a repeated axis can come back with
duplicate names. Every other axis keeps its names untouched.

## Examples

``` r
x <- array(1:6, c(3, 2))

# All rows in bulk, twice
rray_rep(x, times = 2, axes = 1)
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6
#> [4,]    1    4
#> [5,]    2    5
#> [6,]    3    6

# Each row individually, twice
rray_rep_each(x, times = 2, axis = 1)
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    1    4
#> [3,]    2    5
#> [4,]    2    5
#> [5,]    3    6
#> [6,]    3    6

# Columns rather than rows
rray_rep(x, times = 2, axes = 2)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    4    1    4
#> [2,]    2    5    2    5
#> [3,]    3    6    3    6

# Rows and columns, twice each
rray_rep(x, times = 2, axes = c(1, 2))
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    4    1    4
#> [2,]    2    5    2    5
#> [3,]    3    6    3    6
#> [4,]    1    4    1    4
#> [5,]    2    5    2    5
#> [6,]    3    6    3    6

# Rows twice and columns three times
rray_rep(x, times = c(2, 3), axes = c(1, 2))
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    1    4    1    4    1    4
#> [2,]    2    5    2    5    2    5
#> [3,]    3    6    3    6    3    6
#> [4,]    1    4    1    4    1    4
#> [5,]    2    5    2    5    2    5
#> [6,]    3    6    3    6    3    6

# A different number of repeats per row
rray_rep_each(x, times = c(1, 2, 3), axis = 1)
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    2    5
#> [4,]    3    6
#> [5,]    3    6
#> [6,]    3    6

# Zeroes drop slices entirely
rray_rep_each(x, times = c(1, 0, 2), axis = 1)
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    3    6
#> [3,]    3    6

# Names repeat with the data, duplicates and all
y <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))
rray_names(rray_rep(y, times = 2, axes = 1))
#> [[1]]
#> [1] "a" "b" "c" "a" "b" "c"
#> 
#> [[2]]
#> [1] "x" "y"
#> 
rray_names(rray_rep_each(y, times = 2, axis = 1))
#> [[1]]
#> [1] "a" "a" "b" "b" "c" "c"
#> 
#> [[2]]
#> [1] "x" "y"
#> 
```
