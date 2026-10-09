# Toolkit

``` r

library(rray)
```

## Introduction

One of the big goals for rray is to be a general purpose toolkit for
working with base R arrays. In this vignette, we’ll look at a few parts
of this toolkit, and compare against similar concepts from base R.

## Axes

Many of the functions in rray are applied “along an axis”. With base R,
you might be used to the `MARGIN` argument when specifying the axis to
apply a function over. In rray, you’ll use `axes` (or `axis`, depending
on the function). In short, these two are *complements* of one another.
Notice that the values computed in the example below are the same, even
though the axes to compute over look different (ignore the difference in
dimensionality for the moment).

``` r

x <- array(1:8, c(2, 2, 2))

rray_sum(x, axes = 1)
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    3    7
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]   11   15

apply(x, MARGIN = c(2, 3), FUN = sum)
#>      [,1] [,2]
#> [1,]    3   11
#> [2,]    7   15
```

With `axes`, you list the axes that change in some way. In the above
example, `axes = 1` was specified which *guarantees* that the result
will have the same dimensions as `x` everywhere except along the first
axis, which will have dimension 1, no matter what. In other words, the
dimensions change as: `(2, 2, 2) -> (1, 2, 2)`.

## Combining

Reducers like
[`rray_sum()`](https://rray.r-lib.org/reference/rray-reduce.md) aren’t
the only functions with this `axes` guarantee. With
[`rray_combine()`](https://rray.r-lib.org/reference/rray_combine.md),
you specify the `.axis` that you want to combine along. This has the
same guarantee that only the `.axis` specified will be changing. The
only caveat here is that the inputs are first broadcast to common
dimensions (ignoring the `.axis` dimension) before the combine is
carried out. A few examples might be helpful:

``` r

# (5, 1)
x <- matrix(1:5)

# (3)
y <- 6:8

rray_combine(x, y, .axis = 1)
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
#> [4,]    4
#> [5,]    5
#> [6,]    6
#> [7,]    7
#> [8,]    8
```

This works by first finding the common dimensions between `x` and `y`,
ignoring the `.axis` dimension. In this case, the common dimensions are
`(., 1)` where the `.` represents whatever dimension is actually there
for that input (5 for `x` and 3 for `y`). The final result after
combining the inputs together will also have a `(., 1)` shape, where `.`
will be replaced with `5 + 3 = 8`.

Here’s another example, this time combining along the 2nd axis:

``` r

x <- array(1:6, dim = c(3, 2))
x
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6

y <- array(1:9, dim = c(3, 3))
y
#>      [,1] [,2] [,3]
#> [1,]    1    4    7
#> [2,]    2    5    8
#> [3,]    3    6    9

z <- 10L

combined <- rray_combine(x, y, z, .axis = 2)
combined
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    1    4    1    4    7   10
#> [2,]    2    5    2    5    8   10
#> [3,]    3    6    3    6    9   10
```

The inverse of combining is splitting via
[`rray_split()`](https://rray.r-lib.org/reference/rray_split.md).
Splitting requires an `axis` to split along and a vector of `dimensions`
that represent the dimension of each output along `axis`. So, to recover
`x`, `y`, and `z`, we could do:

``` r

rray_split(combined, axis = 2, dimensions = c(2, 3, 1))
#> [[1]]
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6
#> 
#> [[2]]
#>      [,1] [,2] [,3]
#> [1,]    1    4    7
#> [2,]    2    5    8
#> [3,]    3    6    9
#> 
#> [[3]]
#>      [,1]
#> [1,]   10
#> [2,]   10
#> [3,]   10
```

Note that splitting is a precise inverse unless broadcasting was
involved during the original combination process!

## Stacking

When combining arrays, you can only specify an `axis` that *already
exists* along at least one of the inputs. This means that you can’t,
say, combine 2-D matrices along the 3rd axis:

``` r

x <- matrix(1:4, nrow = 2)
x
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4

y <- matrix(5:8, nrow = 2)
y
#>      [,1] [,2]
#> [1,]    5    7
#> [2,]    6    8

rray_combine(x, y, .axis = 3)
#> Error in `rray_combine()`:
#> ! `.axis` must be less than or equal to the dimensionality of 2, not 3.
```

To do this, you’ll instead want
[`rray_stack()`](https://rray.r-lib.org/reference/rray_stack.md):

``` r

stacked <- rray_stack(x, y, .axis = 3)
stacked
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]    5    7
#> [2,]    6    8
```

Conceptually, stacking arrays involves inserting a new axis via
[`rray_insert_axes()`](https://rray.r-lib.org/reference/rray_insert_axes.md)
at the chosen `.axis`, and then combining the results together via
[`rray_combine()`](https://rray.r-lib.org/reference/rray_combine.md).
The `.axis` to stack along can be any axis between `1` to
`dimensionality + 1` of the inputs.

``` r

# Expand to 3D
x_3d <- rray_insert_axes(x, axes = 3)
y_3d <- rray_insert_axes(y, axes = 3)

# Now we can combine
rray_combine(x_3d, y_3d, .axis = 3)
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]    5    7
#> [2,]    6    8
```

Stacking is interesting because you aren’t limited to stacking along
`dimensionality + 1`. You can instead stack along a new 2nd axis, which
results in the columns of each individual array getting grouped together
(i.e. the 1st columns of `x` and `y` are now side by side in the
result):

``` r

x
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4

y
#>      [,1] [,2]
#> [1,]    5    7
#> [2,]    6    8

rray_stack(x, y, .axis = 2)
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    1    5
#> [2,]    2    6
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]    3    7
#> [2,]    4    8
```

The inverse of
[`rray_stack()`](https://rray.r-lib.org/reference/rray_stack.md) is
[`rray_unstack()`](https://rray.r-lib.org/reference/rray_unstack.md).
With [`rray_stack()`](https://rray.r-lib.org/reference/rray_stack.md),
you always get an output with a dimensionality 1 greater than the
inputs. With
[`rray_unstack()`](https://rray.r-lib.org/reference/rray_unstack.md),
you get a list of outputs with a dimensionality 1 less than the input.

``` r

stacked
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]    5    7
#> [2,]    6    8

rray_unstack(stacked, axis = 3)
#> [[1]]
#>      [,1] [,2]
#> [1,]    1    3
#> [2,]    2    4
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    5    7
#> [2,]    6    8
```

If you find yourself reaching for `rray_split(dimensions = 1)` followed
by
[`rray_remove_axes()`](https://rray.r-lib.org/reference/rray_remove_axes.md),
you probably wanted
[`rray_unstack()`](https://rray.r-lib.org/reference/rray_unstack.md)
instead!

## Removing axes

One thing you will immediately notice when working with rray is that it
often tries *not* to remove axes automatically. This is most apparent
with subsetting, and in the reducers like
[`rray_sum()`](https://rray.r-lib.org/reference/rray-reduce.md) and is
in stark contrast to base R.

``` r

x <- matrix(1:6, ncol = 2)

x[1,]
#> [1] 1 4
rray_slice_rows(x, 1)
#>      [,1] [,2]
#> [1,]    1    4

apply(x, 2, sum)
#> [1]  6 15
rray_sum(x, axes = 1)
#>      [,1] [,2]
#> [1,]    6   15
```

The rationale for this has to do with how broadcasting works. When axes
are kept, operations combining multiple rray functions feel natural:

``` r

rray_divide(x, rray_sum(x, axes = 1))
#>           [,1]      [,2]
#> [1,] 0.1666667 0.2666667
#> [2,] 0.3333333 0.3333333
#> [3,] 0.5000000 0.4000000
```

This doesn’t work as you might expect with base R, and can result in a
tragic error since partial recycling kicks in and you don’t get an
error.

``` r

x / apply(x, 2, sum)
#>           [,1]      [,2]
#> [1,] 0.1666667 0.2666667
#> [2,] 0.1333333 0.8333333
#> [3,] 0.5000000 0.4000000

# Equivalent to
col_sums <- apply(x, 2, sum)
partially_recycled <- matrix(rep(col_sums, times = 3), ncol = 2)
partially_recycled
#>      [,1] [,2]
#> [1,]    6   15
#> [2,]   15    6
#> [3,]    6   15

x / partially_recycled
#>           [,1]      [,2]
#> [1,] 0.1666667 0.2666667
#> [2,] 0.1333333 0.8333333
#> [3,] 0.5000000 0.4000000
```

Instead you must remember to use
[`sweep()`](https://rdrr.io/r/base/sweep.html) like:

``` r

sweep(x, 2, apply(x, 2, sum), FUN = "/")
#>           [,1]      [,2]
#> [1,] 0.1666667 0.2666667
#> [2,] 0.3333333 0.3333333
#> [3,] 0.5000000 0.4000000
```

This works nicely in rray because of two reasons, both are necessary:

- When reducing, axes aren’t removed
- When dividing, broadcasting kicks in

If you do want to remove axes, you can explicitly call
[`rray_remove_axes()`](https://rray.r-lib.org/reference/rray_remove_axes.md)
afterwards. As a rule of thumb, it is much easier to remove axes
explicitly than it is to recover them.

``` r

x |>
  rray_sum(axes = 1) |>
  rray_remove_axes(axes = 1)
#> [1]  6 15
```

If you’re a Python user coming from NumPy, you might be used to reducers
dropping the axis you reduce over. I think this is a mistake, and there
have been a number of discussions on the NumPy forums about this choice.
Here is why:

``` r

# This is the result you'd get in a NumPy sum. The 1st axis is dropped.
x_sum_dropped <- rray_remove_axes(rray_sum(x, axes = 1), axes = 1)
x_sum_dropped
#> [1]  6 15

# Now broadcasting doesn't work!
rray_divide(x, x_sum_dropped)
#> Error in `rray_divide()`:
#> ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 2.

# So you have to add back the axis manually
# (NumPy has slightly cleaner ways to do this, but it's still an extra step)
x_sum_reshaped <- rray_insert_axes(x_sum_dropped, axes = 1)
rray_divide(x, x_sum_reshaped)
#>           [,1]      [,2]
#> [1,] 0.1666667 0.2666667
#> [2,] 0.3333333 0.3333333
#> [3,] 0.5000000 0.4000000
```

For the curious, Julia’s implementation of reducers works similarly to
rray.
