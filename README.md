
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mtrx

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The goal of `mtrx` is to provide a simpler matrix class in R, similar in
spirit to `tibble`.

Eventually, it would be great if mtrx objects had broadcasting
semantics, similar to what is done in Numpy.

It would also be neat to have a `dplyr`-ish interface on top of them.

**This is just an experiment, and this package should not be used for
anything meaningful.**

## Installation

You can install from Github with:

``` r
devtools::install_github("DavisVaughan/mtrx")
```

## Creation

``` r
library(mtrx)
```

Create `mtrx` object by either coercing existing objects to `mtrx` or by
using the helpful constructor, `mtrx()`.

``` r
# Coercion
mat <- matrix(1:10, nrow = 5, dimnames = list(NULL, c("a", "b")))

as_mtrx(mat)
#> <vctrs_mtrx[10]>
#>      a  b 
#> [1,]  1  6
#> [2,]  2  7
#> [3,]  3  8
#> [4,]  4  9
#> [5,]  5 10
```

The constructor takes multiple vectors in `...`, where each element is a
column of the matrix, possibly named.

``` r
mtrx_ex <- mtrx(1:5, a = 6:10)

mtrx_ex
#> <vctrs_mtrx[10]>
#>      ..1 a 
#> [1,]  1   6
#> [2,]  2   7
#> [3,]  3   8
#> [4,]  4   9
#> [5,]  5  10
```

mtrx objects have the same slicing semantics as tibbles. Dimensions are
never dropped.

``` r
# first column
mtrx_ex[1]
#> <vctrs_mtrx[5]>
#>      ..1
#> [1,] 1  
#> [2,] 2  
#> [3,] 3  
#> [4,] 4  
#> [5,] 5

# first row
mtrx_ex[1,]
#> <vctrs_mtrx[2]>
#>      ..1 a
#> [1,] 1   6

# second column by name
mtrx_ex["a"]
#> <vctrs_mtrx[5]>
#>      a 
#> [1,]  6
#> [2,]  7
#> [3,]  8
#> [4,]  9
#> [5,] 10

# individual elements
mtrx_ex[1, "a"]
#> <vctrs_mtrx[1]>
#>      a
#> [1,] 6
```
