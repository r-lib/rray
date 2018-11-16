
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rray

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/DavisVaughan/rray.svg?branch=master)](https://travis-ci.org/DavisVaughan/rray)

## Introduction

The goal of rray is to provide stricter matrix and array classes in R,
similar in spirit to `tibble`.

It also supports *broadcasting* semantics for matrices and arrays using
the `xtensor` C++ library. This greatly extends the flexibility of
calculations you can perform with them. This does require C++14.

## Installation

You can install from Github with:

``` r
devtools::install_github("DavisVaughan/rray")
```

## mtrx and rray

``` r
library(rray)
```

The rray package introduces two new classes. An `rray`, which is an
N-dimensional object and essentially a stricter array, and a `mtrx`,
which is a subclass of an `rray` that restricts the object to only two
dimensions.

rray consistently takes the approach that all inputs have at least two
dimensions, with scalars and vectors being treated as 1 column matrices.

The easiest way to create a `mtrx` is to use the constructor. It takes
in (potentially named) vectors and transforms them into the columns of
the matrix.

``` r
mtrx_ex <- mtrx(b = 1:2, a = 3:4)

mtrx_ex
#> <vctrs_mtrx<integer>[,2][4]>
#>      b a
#> [1,] 1 3
#> [2,] 2 4
```

mtrices are stricter than base R’s matrix objects in two main ways:

  - You must be explicit about the dimension when selecting columns
    (i.e. `x[,1]` not `x[1]`).
  - Dimensions are never dropped.

<!-- end list -->

``` r
# first column
mtrx_ex[,1]
#> <vctrs_mtrx<integer>[,1][2]>
#>      b
#> [1,] 1
#> [2,] 2

# not this
mtrx_ex[1]
#> Error: Use `x[,j]` to select columns, not `x[j]`.

# individual elements
mtrx_ex[1, "a"]
#> <vctrs_mtrx<integer>[,1][1]>
#>      a
#> [1,] 3
```

These rules also apply to `rray` objects. You can create an `rray` from
a vector, matrix, or array.

``` r
# 2 rows, 3 columns, 2 "deep" (being the 3rd dimension)
x_rray <- rray(1, dim = c(1, 3, 2))

x_rray
#> <vctrs_rray<double>[,3,2][6]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] 1    1    1   
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] 1    1    1
```

``` r
# first col
x_rray[,1]
#> <vctrs_rray<double>[,1,2][2]>
#> , , 1
#> 
#>      [,1]
#> [1,] 1   
#> 
#> , , 2
#> 
#>      [,1]
#> [1,] 1

# first row of each dimension
x_rray[1,]
#> <vctrs_rray<double>[,3,2][6]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] 1    1    1   
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] 1    1    1

# select the first element in the 3rd dimension
x_rray[,,1]
#> <vctrs_rray<double>[,3,1][3]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] 1    1    1
```

## Broadcasting

Broadcasting, the concept of recycling dimensions of one object to match
the dimensions of another, is one of the core differences between
operations with rray and base R.

It allows you to do powerful operations such as:

``` r
x_rray 
#> <vctrs_rray<double>[,3,2][6]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] 1    1    1   
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] 1    1    1

x_rray + 1:3
#> <vctrs_rray<double>[,3,2][18]>
#> , , 1
#> 
#>       
#>        [,1] [,2] [,3]
#>   [1,] 2    2    2   
#>   [2,] 3    3    3   
#>   [3,] 4    4    4   
#> 
#> , , 2
#> 
#>       
#>        [,1] [,2] [,3]
#>   [1,] 2    2    2   
#>   [2,] 3    3    3   
#>   [3,] 4    4    4
```

To learn how this is working, see the broadcasting vignette,
`vignette("broadcasting", "rray")`.

## Motivation

The motivation for rray started with the inability to do this one
operation with base R.

``` r
x <- matrix(1:5)
y <- matrix(1:10, ncol = 2)

x 
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
#> [4,]    4
#> [5,]    5

y
#>      [,1] [,2]
#> [1,]    1    6
#> [2,]    2    7
#> [3,]    3    8
#> [4,]    4    9
#> [5,]    5   10

x + y
#> Error in x + y: non-conformable arrays
```

As shown earlier, broadcasting solves this problem. I believe that
broadcasting is a natural expression of intent when working with arrays,
and is fully *shape-stable* in the fact that the dimensions of the
outputs are always fully defined by the dimensions of the inputs, so I
feel that its adoption in R would be worthwhile.

A second motivation is the handling of dimension names in base R. While
consistent, it lacks in expressibility that I feel should be there, and
results can be surprising.

``` r
x <- mtrx(a = 1:2, b = 3:4)
y <- mtrx(5:6, 7:8, row_names = c("r1", "r2"))

x_mat <- as.matrix(x)
y_mat <- as.matrix(y)

x_mat
#>      a b
#> [1,] 1 3
#> [2,] 2 4

y_mat
#>    [,1] [,2]
#> r1    5    7
#> r2    6    8

# Dimension names of x_mat are used
x_mat + y_mat
#>      a  b
#> [1,] 6 10
#> [2,] 8 12

# Dimension names of y_mat are used
y_mat + x_mat
#>    [,1] [,2]
#> r1    6   10
#> r2    8   12
```

With rray, dimension names can come from both `x` and `y`. The rules for
determining dimension names in rray are spelled out in
`?rray_dim_names_common`.

``` r
x + y
#> <vctrs_mtrx<integer>[,2][4]>
#>     
#>      a  b 
#>   r1  6 10
#>   r2  8 12
```

## Alternatives

I don’t know of any other R packages that attempt to implement
broadcasting semantics.

The original motivation for this package, and even for `xtensor`, is the
excellent Python library, `numpy`. As far as I know, it has the original
implementation of broadcasting, and is a core library that a huge number
of others are built on top of.
