# as_array() ------------------------------------------------------------------

#' Coerce to an array
#'
#' `as_array()` coerces `x` to an array
#'
#'
#' @param x An object to coerce to an array.
#' @param ... Objects passed on to methods.
#'
#' @examples
#'
#' as_array(1:10)
#'
#' @export
as_array <- function(x, ...) {
  UseMethod("as_array")
}

#' @export
as_array.array <- function(x, ...) {
  x
}

#' @export
as_array.matrix <- function(x, ...) {
  x
}

#' @export
as_array.double <- function(x, ...) {
  array(
    data = x,
    # enforce column
    dim = c(length(x), 1L),
    dimnames = list(names(x), NULL)
  )
}

#' @export
as_array.integer <- as_array.double

#' @export
as_array.logical <- as_array.double

#' @export
as_array.vctrs_mtrx <- function(x, ...) {
  as.array(as_matrix(x))
}

#' @export
as_array.vctrs_rray <- function(x, ...) {
  new_array(
    .data = vec_data(x),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}

#' @export
as.array.vctrs_rray <- as_array.vctrs_rray

#' @export
as.array.vctrs_mtrx <- as_array.vctrs_mtrx

# as_rray() --------------------------------------------------------------------

#' Coerce to an rray
#'
#' Coerce `x` to an rray object.
#'
#' @param x An object to coerce to an rray.
#' @param ... Objects passed on to methods.
#'
#' @examples
#'
#' as_rray(1)
#'
#' as_rray(1:10, col_name = "x", row_names = letters[1:10])
#'
#' ex <- matrix(1:10, nrow = 5, dimnames = list(NULL, c("a", "b")))
#' as_rray(ex)
#'
#' @export
as_rray <- function(x, ...) {
  UseMethod("as_rray")
}

#' @export
as_rray.vctrs_rray <- function(x, ...) {
  x
}

#' @export
as_rray.vctrs_mtrx <- function(x, ...) {
  new_rray(
    .data = vec_data(x),
    size = vec_size(x),
    shape = rray_shape(x),
    dim_names = dim_names(x)
  )
}

#' @export
as_rray.array <- as_rray.vctrs_mtrx

#' @export
as_rray.matrix <- as_rray.vctrs_mtrx

#' @export
as_rray.double <- function(x, ...) {
  .data <- unname(vec_data(x))
  new_rray(.data, size = vec_size(x), dim_names = dim_names(x))
}

#' @export
as_rray.integer <- as_rray.double

#' @export
as_rray.logical <- as_rray.double
