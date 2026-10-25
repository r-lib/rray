# Terminology

``` r

library(rray)
```

## Introduction

Words like “dimension” and “size” get used loosely when talking about
arrays. rray gives each of them one precise meaning, and uses that
meaning everywhere, from function names to argument names to error
messages. This vignette defines each term.

We’ll use this array for most examples:

``` r

x <- array(1:12, dim = c(2, 3, 2))
x
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    7    9   11
#> [2,]    8   10   12
```

## Axis

An *axis* is a single integer that picks a direction along the array.
Axis 1 runs down the rows, axis 2 runs across the columns, and so on.

Many rray functions work “along an axis”. Summing along axis 1 adds up
the rows, so only 1 row is left:

``` r

rray_sum(x, axes = 1)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    3    7   11
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]   15   19   23
```

Summing along axis 3 conceptually adds the two matrices embedded in `x`
together, so only one is left:

``` r

rray_sum(x, axes = 3)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    8   12   16
#> [2,]   10   14   18
```

Notice that only the axis you pick changes. Every other axis keeps its
original shape.

You can also provide an axis by its [title](#titles) rather than its
position. See `?faq-axes` for more.

## Dimension

A *dimension* is the number of elements along one axis. For `x`:

- Axis 1 has a dimension of 2.

- Axis 2 has a dimension of 3.

- Axis 3 has a dimension of 2.

To get the dimension of one axis, index into the
[dimensions](#dimensions):

``` r

axis <- 2
rray_dimensions(x)[[axis]]
#> [1] 3
```

## Dimensions

The *dimensions* are an integer vector holding the dimension of every
axis.

``` r

rray_dimensions(x)
#> [1] 2 3 2
```

This is like [`dim()`](https://rdrr.io/r/base/dim.html), except that it
also works on plain vectors, which act like 1-D arrays:

``` r

dim(1:5)
#> NULL

rray_dimensions(1:5)
#> [1] 5
```

In the other vignettes, dimensions are often written out in parentheses,
like `(2, 3, 2)` for `x`.

You can change the dimensions of an array with
[`rray_set_dimensions()`](https://rray.r-lib.org/dev/reference/rray_set_dimensions.md),
as long as the total number of elements stays the same:

``` r

rray_set_dimensions(1:6, c(2, 3))
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
```

## Dimensionality

The *dimensionality* of an array is the length of the
[dimensions](#dimensions) vector. This also corresponds with the number
of axes it has.

``` r

rray_dimensions(x)
#> [1] 2 3 2

rray_dimensionality(x)
#> [1] 3
```

A vector has a dimensionality of 1, and a matrix has a dimensionality of
2:

``` r

rray_dimensionality(1:5)
#> [1] 1

rray_dimensionality(matrix(1:6, nrow = 2))
#> [1] 2
```

The terms so far have been carefully chosen so that programming with
these concepts lends itself to object names that feel natural, like
this:

``` r

dimensions <- rray_dimensions(x)
dimensionality <- rray_dimensionality(x)
axes <- seq_len(dimensionality)

for (axis in axes) {
  dimension <- dimensions[[axis]]
  print(dimension)
}
#> [1] 2
#> [1] 3
#> [1] 2
```

## Size

The *size* is the total number of elements in the array. It is the
product of the [dimensions](#dimensions):

``` r

rray_dimensions(x)
#> [1] 2 3 2

2 * 3 * 2
#> [1] 12

rray_size(x)
#> [1] 12
```

For an array, this is the same as `length(x)`:

``` r

length(x)
#> [1] 12
```

Be careful not to mix up size and [dimension](#dimension). Axis 2 of `x`
has a dimension of 3, but `x` has a size of 12.

## Names

The *names* are a list with one element per axis. Each element is either
a character vector naming the elements along that axis, or `NULL`. This
is the same as [`dimnames()`](https://rdrr.io/r/base/dimnames.html).

``` r

y <- rray_set_names(
  x,
  list(
    c("a", "b"),
    c("c", "d", "e"),
    c("f", "g")
  )
)
y
#> , , f
#> 
#>   c d e
#> a 1 3 5
#> b 2 4 6
#> 
#> , , g
#> 
#>   c  d  e
#> a 7  9 11
#> b 8 10 12

rray_names(y)
#> [[1]]
#> [1] "a" "b"
#> 
#> [[2]]
#> [1] "c" "d" "e"
#> 
#> [[3]]
#> [1] "f" "g"
```

To get the names along one axis, use
[`rray_axis_names()`](https://rray.r-lib.org/dev/reference/rray_axis_names.md):

``` r

rray_axis_names(y, axis = 3)
#> [1] "f" "g"
```

The first two axes are common enough to get their own helpers:

``` r

rray_row_names(y)
#> [1] "a" "b"

rray_column_names(y)
#> [1] "c" "d" "e"
```

An array without any names has `NULL` names:

``` r

rray_names(x)
#> NULL
```

## Titles

The *titles* are the names on the list of [names](#names). There is one
title per axis, and they show up when the array is printed.

``` r

z <- rray_set_titles(y, c("row", "col", "page"))
z
#> , , page = f
#> 
#>    col
#> row c d e
#>   a 1 3 5
#>   b 2 4 6
#> 
#> , , page = g
#> 
#>    col
#> row c  d  e
#>   a 7  9 11
#>   b 8 10 12

rray_titles(z)
#> [1] "row"  "col"  "page"
```

Titles are useful because you can refer to an [axis](#axis) by its title
rather than its position:

``` r

rray_sum(z, axes = "col")
#> , , page = f
#> 
#>    col
#> row [,1]
#>   a    9
#>   b   12
#> 
#> , , page = g
#> 
#>    col
#> row [,1]
#>   a   27
#>   b   30
```

## Broadcasting

*Broadcasting* is how rray works with inputs that have different
[dimensions](#dimensions). Axes with a dimension of 1 are repeated to
match the other inputs. Here, `b` has 1 column, so it is repeated to
match the 2 columns of `a`:

``` r

a <- matrix(1:6, nrow = 3)
a
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6

b <- matrix(c(10L, 20L, 30L))
b
#>      [,1]
#> [1,]   10
#> [2,]   20
#> [3,]   30

rray_add(a, b)
#>      [,1] [,2]
#> [1,]   11   14
#> [2,]   22   25
#> [3,]   33   36
```

Broadcasting is important enough to get its own vignette. See
[`vignette("broadcasting")`](https://rray.r-lib.org/dev/articles/broadcasting.md)
to learn more.
