
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rray <a href='https:/rray.r-lib.org'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

<!-- badges: end -->

rray (said: “r-ray”) is an array manipulation library for R. rray’s goal
is to provide a consistent, powerful toolkit for array manipulation,
usable on base R arrays. It supports broadcasting throughout the entire
package, which allows for novel array operations popularized by
[NumPy](https://numpy.org/) that have been missing from the R ecosystem.

View [the website](https://rray.r-lib.org) to learn more about how to
use rray.

``` r
library(rray)
```

## Installation

You can install the development version of rray from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("r-lib/rray")
```

## What can it do?

In short, rray tries to make array manipulation in R more intuitive by
combining the idea of broadcasting with knowing when to *not* drop
dimensions. This results in operations such as:

``` r
x <- array(1:6, dim = c(3, 2))

# Compute proportions along the 1st dimension
rray_divide(x, rray_sum(x, axes = 1))
#>           [,1]      [,2]
#> [1,] 0.1666667 0.2666667
#> [2,] 0.3333333 0.3333333
#> [3,] 0.5000000 0.4000000

# Equivalent base R syntax
sweep(x, 2, apply(x, 2, sum), "/")
#>           [,1]      [,2]
#> [1,] 0.1666667 0.2666667
#> [2,] 0.3333333 0.3333333
#> [3,] 0.5000000 0.4000000
```

These concepts are baked into every part of rray, and show up in other
functions such as `rray_combine()`. Using broadcasting, `rray_combine()`
can bind arrays together in ways that base R cannot with the native
`cbind()` and `rbind()` functions.

``` r
a <- array(c(1, 2), dim = c(2, 1))
b <- array(c(3, 4), dim = c(1, 2))

a
#>      [,1]
#> [1,]    1
#> [2,]    2

b
#>      [,1] [,2]
#> [1,]    3    4

# Error
cbind(a, b)
#> Error in `cbind()`:
#> ! number of rows of matrices must match (see arg 2)

# `a` is first broadcast to have dimensions: (2, 2)
rray_combine(a, b, .axis = 1)
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    2    2
#> [3,]    3    4

# Error
rbind(a, b)
#> Error in `rbind()`:
#> ! number of columns of matrices must match (see arg 2)

# `b` is first broadcast to have dimensions: (2, 2)
rray_combine(a, b, .axis = 2)
#>      [,1] [,2] [,3]
#> [1,]    1    3    4
#> [2,]    2    3    4
```

## Acknowledgements

rray was originally written in 2018 and was backed by
[`xtensor`](https://github.com/QuantStack/xtensor). That original
implementation is archived as
[DavisVaughan/rray1](https://github.com/DavisVaughan/rray1), but its API
served as the foundation for this rewrite.

rray uses a large amount of theory from
[`vctrs`](https://github.com/r-lib/vctrs) to be consistent and type
stable. For example, broadcasting is just an extension of the [tidyverse
recycling
rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html),
applied to all axes of the array.

The original motivation for this package, and even for xtensor, is the
excellent Python library, NumPy. As far as I know, it has the original
implementation of broadcasting, and is a core library that a huge number
of others are built on top of.

In the past, the workhorse for flexibly binding arrays together has been
the abind package. This package has been a great source of inspiration
and has served as a nice benchmark for rray.

## Limitations

- No S3 support. rray is currently a closed system
