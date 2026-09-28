#' Roll an array
#'
#' @description
#' - `rray_roll()` rolls the elements of `x` along one or more `axes`, with one
#'   roll per axis. Everything along an axis moves by the same amount.
#'
#' - `rray_roll_each()` rolls the elements of `x` along a single `axis`, with
#'   a separate roll for each row, column, and so on. For example, with a
#'   matrix and `axis = 2`, each row can move by a different amount.
#'
#' An element pushed off one end circles back on the other.
#'
#' @details
#' A positive `n` moves elements toward the end of the axis, and a negative `n`
#' moves them toward the start. Rolls circle around, so an `n` of 7 along an
#' axis with dimension 5 is the same as an `n` of 2. An `n` of 0, or any
#' multiple of the dimension, leaves the axis unchanged.
#'
#' @section Roll:
#' Rolling an axis with a uniform `n` can be thought of as a carefully crafted
#' [rray_slice()]. Along an axis with dimension `d`, position `i` of the result
#' comes from position `(i - 1 - n) %% d + 1` of `x`:
#'
#' ```r
#' x <- c(a = 1L, b = 2L, c = 3L, d = 4L, e = 5L)
#'
#' n <- 2L
#' d <- length(x)
#' i <- seq_len(d)
#'
#' (i - 1L - n) %% d + 1L
#' #> [1] 4 5 1 2 3
#'
#' rray_roll(x, n = n, axes = 1)
#' #> d e a b c
#' #> 4 5 1 2 3
#'
#' rray_slice_axis(x, c(4, 5, 1, 2, 3), axis = 1)
#' #> d e a b c
#' #> 4 5 1 2 3
#' ```
#'
#' Rolling several axes at once is one `rray_slice()`, with these locations on
#' each rolled axis and `TRUE` on the rest.
#'
#' Since a uniform roll is a slice, names on a rolled axis move with the data,
#' just like they do in `rray_slice()`. Every other axis keeps its names
#' untouched.
#'
#' @section Roll each:
#' `rray_roll_each()` rolls along a single `axis`, but each row, column, and so
#' on can move by its own amount. That is why `n` is an array rather than a
#' vector. It is broadcast to the dimensions of `x` with `axis` set to 1, so it
#' holds one `n` for every row, column, and so on that gets rolled.
#'
#' Variable rolling can be mimicked with the more flexible indexing of
#' [rray_index()]. Along `axis`, the coordinates come from the same
#' `(i - 1 - n) %% d + 1` formula as `rray_roll()`, with a different `n` for
#' each row, column, and so on. Along every other axis, the coordinates are
#' just the positions themselves.
#'
#' `rray_roll_each()` drops names along `axis`, but keeps all other names. When
#' rows move by different amounts, the output's column 1 can hold data from
#' column `c` in one row and from column `b` in another, so the original column
#' names are unlikely to make sense:
#'
#' ```r
#' x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))
#'
#' rray_roll_each(x, n = matrix(c(1, 2), ncol = 1), axis = 2)
#' #>    [,1] [,2] [,3]
#' #> r1    5    1    3
#' #> r2    4    6    2
#' ```
#'
#' ## Two dimensions
#'
#' Start with a matrix:
#'
#' ```r
#' x <- matrix(1:12, nrow = 3, byrow = TRUE)
#'
#' x
#' #>      [,1] [,2] [,3] [,4]
#' #> [1,]    1    2    3    4
#' #> [2,]    5    6    7    8
#' #> [3,]    9   10   11   12
#' ```
#'
#' Rolling along the columns, `axis = 2`, moves elements within each row. `x`
#' has dimensions `c(3, 4)`, so `n` is broadcast to `c(3, 1)`, a one column
#' matrix with one `n` per row:
#'
#' ```r
#' n <- matrix(c(1L, 0L, -1L), ncol = 1)
#'
#' rray_roll_each(x, n = n, axis = 2)
#' #>      [,1] [,2] [,3] [,4]
#' #> [1,]    4    1    2    3
#' #> [2,]    5    6    7    8
#' #> [3,]   10   11   12    9
#' ```
#'
#' The first row moves 1 toward the end, the second row stays put, and the
#' third row moves 1 toward the start.
#'
#' The same result with `rray_index()`. Each row gets its own column
#' coordinates from the formula, and the row coordinates are just `1:3`:
#'
#' ```r
#' rows <- array(1:3, c(3, 1))
#' columns <- outer(c(n), 1:4, \(n, i) (i - 1L - n) %% 4L + 1L)
#'
#' columns
#' #>      [,1] [,2] [,3] [,4]
#' #> [1,]    4    1    2    3
#' #> [2,]    1    2    3    4
#' #> [3,]    2    3    4    1
#'
#' rray_index(x, rows, columns)
#' #>      [,1] [,2] [,3] [,4]
#' #> [1,]    4    1    2    3
#' #> [2,]    5    6    7    8
#' #> [3,]   10   11   12    9
#' ```
#'
#' Rolling along the rows, `axis = 1`, moves elements within each column. Now
#' `n` is broadcast to `c(1, 4)`, so one `n` per column must be a one row
#' matrix. A plain vector of 4 has dimensions `c(4)`, which doesn't fit.
#'
#' ```r
#' rray_roll_each(x, n = matrix(c(0L, 1L, 2L, 3L), nrow = 1), axis = 1)
#' #>      [,1] [,2] [,3] [,4]
#' #> [1,]    1   10    7    4
#' #> [2,]    5    2   11    8
#' #> [3,]    9    6    3   12
#' ```
#'
#' The last column has an `n` of 3 on a dimension of 3, a full circle, so it
#' doesn't move.
#'
#' ## Three dimensions
#'
#' Three dimensions get a bit more complicated. Think of an array with
#' dimensions `c(2, 5, 3)` as 3 sheets, each with 2 rows and 5 columns:
#'
#' ```r
#' x <- array(1:30, c(2, 5, 3))
#'
#' x
#' #> , , 1
#' #>
#' #>      [,1] [,2] [,3] [,4] [,5]
#' #> [1,]    1    3    5    7    9
#' #> [2,]    2    4    6    8   10
#' #>
#' #> , , 2
#' #>
#' #>      [,1] [,2] [,3] [,4] [,5]
#' #> [1,]   11   13   15   17   19
#' #> [2,]   12   14   16   18   20
#' #>
#' #> , , 3
#' #>
#' #>      [,1] [,2] [,3] [,4] [,5]
#' #> [1,]   21   23   25   27   29
#' #> [2,]   22   24   26   28   30
#' ```
#'
#' Rolling along the columns, `axis = 2`, moves elements within each row. There
#' are 2 rows on each of 3 sheets, so there are 6 separate rows that can each
#' roll by their own amount. A vector can't hold one `n` for each of them, but
#' an array can. `n` is broadcast to `c(2, 1, 3)`, which is one `n` for each row
#' on each sheet:
#'
#' ```r
#' n <- array(1:6, c(2, 1, 3))
#'
#' n
#' #> , , 1
#' #>
#' #>      [,1]
#' #> [1,]    1
#' #> [2,]    2
#' #>
#' #> , , 2
#' #>
#' #>      [,1]
#' #> [1,]    3
#' #> [2,]    4
#' #>
#' #> , , 3
#' #>
#' #>      [,1]
#' #> [1,]    5
#' #> [2,]    6
#'
#' rray_roll_each(x, n = n, axis = 2)
#' #> , , 1
#' #>
#' #>      [,1] [,2] [,3] [,4] [,5]
#' #> [1,]    9    1    3    5    7
#' #> [2,]    8   10    2    4    6
#' #>
#' #> , , 2
#' #>
#' #>      [,1] [,2] [,3] [,4] [,5]
#' #> [1,]   15   17   19   11   13
#' #> [2,]   14   16   18   20   12
#' #>
#' #> , , 3
#' #>
#' #>      [,1] [,2] [,3] [,4] [,5]
#' #> [1,]   21   23   25   27   29
#' #> [2,]   30   22   24   26   28
#' ```
#'
#' Row 1 of sheet 1 moves 1, row 2 of sheet 1 moves 2, and so on. Row 1 of
#' sheet 3 has an `n` of 5 on a dimension of 5, a full circle, so it doesn't
#' move. Row 2 of sheet 3 has an `n` of 6, which circles around to 1.
#'
#' You only need a real dimension in `n` where the roll actually changes:
#'
#' ```r
#' # One `n` per row, the same on every sheet
#' rray_roll_each(x, n = c(1, 2), axis = 2)
#'
#' # One `n` per sheet, the same for both rows
#' rray_roll_each(x, n = array(c(0, 1, 2), c(1, 1, 3)), axis = 2)
#' ```
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x An array.
#'
#' @param n For `rray_roll()`, an integer vector indicating the amount to roll,
#'   either size 1 or the size of `axes`.
#'
#'   For `rray_roll_each()`, an integer array indicating the amount to roll,
#'   broadcastable to the dimensions of `x` with `axis` set to 1.
#'
#' @param axes An integer vector of axes to roll along.
#'
#' @param axis A single integer representing the axis to roll along.
#'
#' @returns
#' An array with the same type and dimensions as `x`.
#'
#' @name rray-roll
#' @examples
#' # Positive `n` moves elements toward the end
#' rray_roll(1:5, n = 2, axes = 1)
#'
#' # Negative `n` moves them toward the start
#' rray_roll(1:5, n = -1, axes = 1)
#'
#' # Large `n` circles around
#' rray_roll(1:5, n = 7, axes = 1)
#'
#' x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))
#'
#' # Roll the columns, names move with the data
#' rray_roll(x, n = 1, axes = 2)
#'
#' # Roll the rows
#' rray_roll(x, n = 1, axes = 1)
#'
#' # Roll both axes by the same `n`
#' rray_roll(x, n = 1, axes = c(1, 2))
#'
#' # Or by one `n` per axis
#' rray_roll(x, n = c(1, -1), axes = c(1, 2))
#'
#' y <- matrix(1:12, nrow = 3, byrow = TRUE)
#'
#' # Roll each row by its own `n`, given as a one column matrix
#' rray_roll_each(y, n = matrix(c(1, 0, -1), ncol = 1), axis = 2)
#'
#' # Roll each column by its own `n`, given as a one row matrix
#' rray_roll_each(y, n = matrix(c(0, 1, 2, 3), nrow = 1), axis = 1)
#'
#' # Names on `axis` are dropped, since no single name fits each position
#' rray_roll_each(x, n = matrix(c(1, 2), ncol = 1), axis = 2)
NULL

#' @rdname rray-roll
#' @export
rray_roll <- function(x, ..., n, axes) {
  check_dots_empty0(...)
  .Call(ffi_rray_roll, x, n, axes, environment())
}

#' @rdname rray-roll
#' @export
rray_roll_each <- function(x, ..., n, axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_roll_each, x, n, axis, environment())
}
