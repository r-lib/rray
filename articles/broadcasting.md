# Broadcasting

``` r

library(rray)
```

## Introduction

The idea for rray sprung from frustration with the following example:

``` r

x <- matrix(1:6, nrow = 3)
x
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6

x + 1
#>      [,1] [,2]
#> [1,]    2    5
#> [2,]    3    6
#> [3,]    4    7

y <- matrix(1)
x + y
#> Error in `x + y`:
#> ! non-conformable arrays
```

To someone who has worked with matrices in R before, this error message
is probably nothing new. It’s stating that because `x` has dimensions
`(3, 2)` and is being added to an object with dimensions `(1, 1)`, which
is a matrix that does not have *exactly the same dimensions*, the
operation cannot be completed. My frustration lies with the fact that I
had this “feeling” that I knew the answer to this operation. It
shouldn’t be an error, it should have the same answer as `x + 1`. Why
does this work, when the other doesn’t?

In one sentence, the answer is that R *recycles*, but I want it to
*broadcast*.

This vignette’s goal is to introduce the concept of broadcasting.
Broadcasting is the idea of repeating dimensions of one object to match
the dimensions of another, so that an operation can be applied between
the two inputs. Later you will learn two rules which formalize this
idea.

## What did I expect?

For this first example, I expected this result:

``` r

x
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6

x + 1
#>      [,1] [,2]
#> [1,]    2    5
#> [2,]    3    6
#> [3,]    4    7

rray_add(x, y)
#>      [,1] [,2]
#> [1,]    2    5
#> [2,]    3    6
#> [3,]    4    7
```

rray implements broadcasting throughout the package, which is what
allows this to work. It solves this motivating example, but is generally
useful in variety of ways.

## Rules

Broadcasting has two steps:

1.  The *dimensionality* of the inputs are matched by *appending 1’s* to
    the input with a lower dimensionality.
2.  The *dimensions* of the inputs are matched by *recycling* the
    dimension of each axis as needed.

By *recycling*, we mean that an axis of dimension 1 can be repeated to
any other dimension. If you’re familiar with the [tidyverse recycling
rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html),
then this should feel familiar. It’s the same idea, just applied to all
axes of the array.

## Example - `x + y`

Let’s revisit the original example, but use the broadcasting rules to
understand how we got the result.

``` r

x
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6

y
#>      [,1]
#> [1,]    1
```

When adding these two matrices together, it’s useful to write out the
dimensions explicitly, using the general notation of:

    (rows, cols) | object

    (3, 2) | x
    (1, 1) | y
    -------|-------
    (3, 2) | result

To understand how we get from `x + y -> result`, compare axes
vertically, and apply the broadcasting rules.

- 1st dimension:

  - `x` has `3` rows
  - `y` has `1` row
  - Recycle the 1 to 3

- 2nd dimension:

  - `x` has `2` column
  - `y` has `1` column
  - Recycle the 1 to 2

&nbsp;

    (     3,      2) | x
    (1 -> 3, 1 -> 2) | y

You can explicitly see these rules in action with
[`rray_dimensions_common()`](https://rray.r-lib.org/reference/rray_dimensions_common.md)

``` r

dimensions <- rray_dimensions_common(x, y)
dimensions
#> [1] 3 2
```

And you can explicitly broadcast with
[`rray_broadcast()`](https://rray.r-lib.org/reference/rray_broadcast.md),
which generates “conformable” arrays that base R can add together:

``` r

x_broadcast <- rray_broadcast(x, dimensions)
x_broadcast
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6

y_broadcast <- rray_broadcast(y, dimensions)
y_broadcast
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1    1
#> [3,]    1    1

x_broadcast + y_broadcast
#>      [,1] [,2]
#> [1,]    2    5
#> [2,]    3    6
#> [3,]    4    7
```

The downside of
[`rray_broadcast()`](https://rray.r-lib.org/reference/rray_broadcast.md)
is that this materializes the fully broadcast version of `y` in memory.
[`rray_add()`](https://rray.r-lib.org/reference/rray-arithmetic.md)
instead iterates over both `x` and `y` without materializing the
intermediate result, which is much more efficient!

## Example - Implicit dimensions

If `y` was a 1-D array, not a 2-D matrix, what would have changed?

``` r

x
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6

y <- array(c(1L, 2L, 3L))
y
#> [1] 1 2 3

rray_add(x, y)
#>      [,1] [,2]
#> [1,]    2    5
#> [2,]    4    7
#> [3,]    6    9
```

This operation is still valid, and represents the first broadcasting
rule in action - the *dimensionality* of the inputs are matched by
*appending 1’s* to the input with a lower dimensionality. Here, that’s
`y`:

    (3, 2) | x
    (3)    | y
    -------|-------
    (3, 2) | result

`x` has a dimensionality of 2, `y` has a dimensionality of 1, so we
append 1s to the *right* hand side of `y` until dimensionalities match:

    (3, 2) | x
    (3, 1) | y
    -------|-------
    (3, 2) | result

Now we apply recycling to `y` 2nd axis:

    (3, 2) | x
    (3, 2) | y
    -------|-------
    (3, 2) | result

Explicitly:

``` r

y
#> [1] 1 2 3

dimensions <- rray_dimensions_common(x, y)
dimensions
#> [1] 3 2

rray_broadcast(y, dimensions)
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    2    2
#> [3,]    3    3
```

## Example - Higher dimensions

Broadcasting becomes more interesting when higher dimensional objects
are used. Additionally, at this point only 1 object at a time has needed
altering. This example will require both inputs to be changed.

``` r

a <- array(1:3, c(1, 3))
a
#>      [,1] [,2] [,3]
#> [1,]    1    2    3

b <- array(1:8, c(4, 1, 2))
b
#> , , 1
#> 
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
#> [4,]    4
#> 
#> , , 2
#> 
#>      [,1]
#> [1,]    5
#> [2,]    6
#> [3,]    7
#> [4,]    8
```

Perhaps surprisingly, these inputs can be added together using
broadcasting.

``` r

rray_add(a, b)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    2    3    4
#> [2,]    3    4    5
#> [3,]    4    5    6
#> [4,]    5    6    7
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    6    7    8
#> [2,]    7    8    9
#> [3,]    8    9   10
#> [4,]    9   10   11
```

To understand this, write out dimensions and follow the rules:

    (1, 3)    | a
    (4, 1, 2) | b

Match dimensionalities by appending `1`s to `a`:

    (1, 3, 1) | a
    (4, 1, 2) | b

and then recycle the axes with dimension 1 within each vertical pair:

    (1 -> 4,      3, 1 -> 2) | a
    (     4, 1 -> 3,      2) | b

Written compactly:

    (1, 3)    | a
    (4, 1, 2) | b
    --------- | ------
    (4, 3, 2) | result

## Addendum

### Base R Recycling

The only exception to the strict behavior of base R is when an array is
combined with a 1D vector. “Scalar” operations work as one might expect:

``` r

x
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6

x * 2
#>      [,1] [,2]
#> [1,]    2    8
#> [2,]    4   10
#> [3,]    6   12
```

But when a vector and an array are combined, the vector is recycled in
its 1D flat form to fit the total size of the array. *This is very
different from broadcasting.*

``` r

vec <- c(1, 2, 3)

x
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6

x + vec
#>      [,1] [,2]
#> [1,]    2    5
#> [2,]    4    7
#> [3,]    6    9

# Equivalent to:
rep(vec, 2)
#> [1] 1 2 3 1 2 3
x + rep(vec, 2)
#>      [,1] [,2]
#> [1,]    2    5
#> [2,]    4    7
#> [3,]    6    9
```

This example seems intuitive and harmless, but even *partial recycling*
is allowed, which can be quite confusing and dangerous.

``` r

vec <- c(1, 2)

x + vec
#>      [,1] [,2]
#> [1,]    2    6
#> [2,]    4    6
#> [3,]    4    8
```

This is identical to the following, where `vec` is repeatedly recycled
to construct something that can be added to `x`.

``` r

recycled <- matrix(NA, 3, 2)

recycled[1, 1] <- vec[1]
recycled[2, 1] <- vec[2]
recycled[3, 1] <- vec[1]
recycled[1, 2] <- vec[2]
recycled[2, 2] <- vec[1]
recycled[3, 2] <- vec[2]

recycled
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    2    1
#> [3,]    1    2

x + recycled
#>      [,1] [,2]
#> [1,]    2    6
#> [2,]    4    6
#> [3,]    4    8
```

rray never performs partial recycling when it is going through the steps
of broadcasting, and will error if this example is attempted.

``` r

rray_add(x, vec)
#> Error in `rray_add()`:
#> ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 2.
```

### Differences from Python

In the Python world, NumPy is used for broadcasting operations. One
interesting thing to note is that when applying broadcasting rules for
NumPy, implicit dimensions are *prepended to the left hand side* as
`1`s. This is used consistently in the NumPy world, even down to how
they are printed. However, R already uses the concept of *appended*
implicit dimensions on the *right* hand side, and prints with this
concept in mind.

For a simple example of R doing this, try coercing a vector to a matrix:

``` r

# (3) -> (3, 1)
as.matrix(1:3)
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
```

The dimensions went from `(3)` to `(3, 1)`. NumPy actually does the
opposite, and would convert `(3)` to `(1, 3)`. This is fine, because
they use it consistently there, but can be confusing when comparing
broadcasting of rray to NumPy, and is just something to keep in mind.
