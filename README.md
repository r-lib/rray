
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rray

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The goal of `rray` is to provide stricter matrix and array classes in R,
similar in spirit to `tibble`.

It also supports broadcasting semantics for matrices and arrays using
the `xtensor` library. This greatly extends the flexibility of
calculations you can perform with matrices.

**This is still just an experiment.**

## Installation

You can install from Github with:

``` r
devtools::install_github("DavisVaughan/rray")
```

## Creation

``` r
library(rray)
```

Create `mtrx` objects by either coercing existing objects to `mtrx` or
by using the helpful constructor, `mtrx()`.

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
mtrx_ex <- mtrx(b = c(1, 2, 3, 4, 5), a = c(6, 7, 8, 9, 10))

mtrx_ex
#> <vctrs_mtrx[10]>
#>      b  a 
#> [1,]  1  6
#> [2,]  2  7
#> [3,]  3  8
#> [4,]  4  9
#> [5,]  5 10
```

mtrx objects have the same slicing semantics as tibbles. Dimensions are
never dropped.

``` r
# first column
mtrx_ex[1]
#> <vctrs_mtrx[5]>
#>      b
#> [1,] 1
#> [2,] 2
#> [3,] 3
#> [4,] 4
#> [5,] 5

# first row
mtrx_ex[1,]
#> <vctrs_mtrx[2]>
#>      b a
#> [1,] 1 6

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

Broadcast semantics are available through `xtensor`.

    (5, 2)
    (5, 1)
    ------
    (5, 2)

``` r
col_a <- mtrx_ex["a"]
col_a
#> <vctrs_mtrx[5]>
#>      a 
#> [1,]  6
#> [2,]  7
#> [3,]  8
#> [4,]  9
#> [5,] 10

mtrx_ex + col_a
#> <vctrs_mtrx[10]>
#>      [,1] [,2]
#> [1,]  7   12  
#> [2,]  9   14  
#> [3,] 11   16  
#> [4,] 13   18  
#> [5,] 15   20

# Note that you cant do this in base R!
as_matrix(mtrx_ex) + as_matrix(col_a)
#> Error in as_matrix(mtrx_ex) + as_matrix(col_a): non-conformable arrays
```

Outer sums and products are incredibly simple with this broadcast
syntax.

    (5, 1)
    (1, 5)
    ------
    (5, 5)

``` r
col_a + t(col_a)
#> <vctrs_mtrx[25]>
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] 12   13   14   15   16  
#> [2,] 13   14   15   16   17  
#> [3,] 14   15   16   17   18  
#> [4,] 15   16   17   18   19  
#> [5,] 16   17   18   19   20

col_a * t(col_a)
#> <vctrs_mtrx[25]>
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]  36   42   48   54   60 
#> [2,]  42   49   56   63   70 
#> [3,]  48   56   64   72   80 
#> [4,]  54   63   72   81   90 
#> [5,]  60   70   80   90  100
```

It’s not just restricted to matrices either. Let’s create a 2x3x2 rray
object.

``` r
x_rray <- rray(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
x_rray <- rray_reshape(x_rray, c(2, 3, 2))

x_rray
#> <vctrs_rray[12]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]  1    3    5  
#> [2,]  2    4    6  
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]  7    9   11  
#> [2,]  8   10   12
```

Subsetting rray objects is strict too.

``` r
# first col
x_rray[1]
#> <vctrs_rray[4]>
#> , , 1
#> 
#>      [,1]
#> [1,] 1   
#> [2,] 2   
#> 
#> , , 2
#> 
#>      [,1]
#> [1,] 7   
#> [2,] 8

# first row of each dimension
x_rray[1,]
#> <vctrs_rray[6]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]  1    3    5  
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]  7    9   11

# only the first 3rd dimension object
x_rray[,,1]
#> <vctrs_rray[6]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] 1    3    5   
#> [2,] 2    4    6
```

Broadcasting operations are more powerful here.

``` r
x_rray[1]
#> <vctrs_rray[4]>
#> , , 1
#> 
#>      [,1]
#> [1,] 1   
#> [2,] 2   
#> 
#> , , 2
#> 
#>      [,1]
#> [1,] 7   
#> [2,] 8
```

    (2, 3, 2)
    (2, 1, 2)
    ---------
    (2, 3, 2)

``` r
x_rray - x_rray[1]
#> <vctrs_rray[12]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] 0    2    4   
#> [2,] 0    2    4   
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] 0    2    4   
#> [2,] 0    2    4
```

    (2, 3, 2)
    (1, 3, 2)
    ---------
    (2, 3, 2)

``` r
x_rray + x_rray[1,]
#> <vctrs_rray[12]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]  2    6   10  
#> [2,]  3    7   11  
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] 14   18   22  
#> [2,] 15   19   23
```

You can mix rray and mtrx objects, and more importantly you can mix both
with base R types.

``` r
two_rows <- col_a[1:2,]
two_rows
#> <vctrs_mtrx[2]>
#>      a
#> [1,] 6
#> [2,] 7

# (2, 3, 2)
# (2, 1,  )
# ---------
# (2, 3, 2)
x_rray + two_rows
#> <vctrs_rray[12]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]  7    9   11  
#> [2,]  9   11   13  
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] 13   15   17  
#> [2,] 15   17   19
```

``` r
# Divide by an R matrix
x_rray / matrix(5, nrow = 2, ncol = 3)
#> <vctrs_rray[12]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] 0.2  0.6  1.0 
#> [2,] 0.4  0.8  1.2 
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] 1.4  1.8  2.2 
#> [2,] 1.6  2.0  2.4

# Multiply an R "scalar"
x_rray / 2
#> <vctrs_rray[12]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] 0.5  1.5  2.5 
#> [2,] 1.0  2.0  3.0 
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] 3.5  4.5  5.5 
#> [2,] 4.0  5.0  6.0

# R vectors are treated as 1 column matrices
x_rray + c(10, 11)
#> <vctrs_rray[12]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,] 11   13   15  
#> [2,] 13   15   17  
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,] 17   19   21  
#> [2,] 19   21   23
```

You can manually broadcast with `rray_broadcast()`

``` r
rray_broadcast(c(10, 11), c(2, 10))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]   10   10   10   10   10   10   10   10   10    10
#> [2,]   11   11   11   11   11   11   11   11   11    11

# broadcasting a mtrx to 3 dimensions result in an rray
rray_broadcast(col_a, c(5, 3, 2))
#> <vctrs_rray[30]>
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]  6    6    6  
#> [2,]  7    7    7  
#> [3,]  8    8    8  
#> [4,]  9    9    9  
#> [5,] 10   10   10  
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]  6    6    6  
#> [2,]  7    7    7  
#> [3,]  8    8    8  
#> [4,]  9    9    9  
#> [5,] 10   10   10
```

There are a number of other functions in `xtensor` that would be great
to expose. One of them is `atan()` and it has been exposed already.

``` r
atan(col_a)
#> <vctrs_mtrx[5]>
#>      [,1]    
#> [1,] 1.405648
#> [2,] 1.428899
#> [3,] 1.446441
#> [4,] 1.460139
#> [5,] 1.471128

atan(x_rray)
#> <vctrs_rray[12]>
#>       [,1]     
#>  [1,] 0.7853982
#>  [2,] 1.1071487
#>  [3,] 1.2490458
#>  [4,] 1.3258177
#>  [5,] 1.3734008
#>  [6,] 1.4056476
#>  [7,] 1.4288993
#>  [8,] 1.4464413
#>  [9,] 1.4601391
#> [10,] 1.4711277
#> [11,] 1.4801364
#> [12,] 1.4876551
```
