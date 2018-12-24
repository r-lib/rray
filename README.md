
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

## The rray

``` r
library(rray)
library(magrittr)
```

The rray package introduces a new array-like object, the `rray`. This is
an N-dimensional object and essentially a stricter array.

rray allows the creation of 1D objects, which look similar to vectors,
but function similar to 1 column matrices (in terms of how operations
are broadcasted).

The easiest way to create an `rray` is to use the constructor. It takes
an object which could be a vector, matrix, or array, and creates a new
`rray` with optional reshaping.

``` r
x_rray <- rray(1:4, c(2, 2)) %>%
  set_col_names(c("a", "b"))

x_rray
#> <vctrs_rray<integer>[,2][4]>
#>      a b
#> [1,] 1 3
#> [2,] 2 4
```

rrays are stricter than base R’s rray objects in two main ways:

  - You must be explicit about the dimension when selecting columns
    (i.e. `x[,1]` not `x[1]`).
  - Dimensions are never dropped.

<!-- end list -->

``` r
# first column
x_rray[,1]
#> <vctrs_rray<integer>[,1][2]>
#>      a
#> [1,] 1
#> [2,] 2

# not this
x_rray[1]
#> Error: Use `x[,j]` to select columns, not `x[j]`.

# individual elements
x_rray[1, "a"]
#> <vctrs_rray<integer>[,1][1]>
#>      a
#> [1,] 1
```

It’s easy to create 3D objects (and higher dimensions) as well.

``` r
# 2 rows, 3 columns, 2 "deep" (being the 3rd dimension)
y_rray <- rray(1, dim = c(1, 3, 2))

y_rray
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

The same subsetting rules apply here, allowing for more intuitive subset
operations.

``` r
# first col
y_rray[,1]
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
y_rray[1,]
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
y_rray[,,1]
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
y_rray 
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

y_rray + 1:3
#> <vctrs_rray<double>[,3,2][18]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] 2    2    2   
#> [2,] 3    3    3   
#> [3,] 4    4    4   
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] 2    2    2   
#> [2,] 3    3    3   
#> [3,] 4    4    4
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
z_rray <- rray(5:8, c(2, 2)) %>%
  set_row_names(c("r1", "r2"))

x_mat <- as.matrix(x_rray)
z_mat <- as.matrix(z_rray)

x_mat
#>      a b
#> [1,] 1 3
#> [2,] 2 4

z_mat
#>    [,1] [,2]
#> r1    5    7
#> r2    6    8

# Dimension names of x_mat are used
x_mat + z_mat
#>      a  b
#> [1,] 6 10
#> [2,] 8 12

# Dimension names of z_mat are used
z_mat + x_mat
#>    [,1] [,2]
#> r1    6   10
#> r2    8   12
```

With rray, dimension names can come from both objects. The rules for
determining dimension names in rray are spelled out in
`?rray_dim_names_common`.

``` r
x_rray + z_rray
#> <vctrs_rray<integer>[,2][4]>
#>    a  b 
#> r1  6 10
#> r2  8 12
```

## Alternatives

The `Matrix` package implements a small subset of column-wise
broadcasting operations. `rray` fully supports broadcasting in all
operations.

The original motivation for this package, and even for `xtensor`, is the
excellent Python library, `numpy`. As far as I know, it has the original
implementation of broadcasting, and is a core library that a huge number
of others are built on top of.
