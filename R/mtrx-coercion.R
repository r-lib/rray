# as_matrix() ------------------------------------------------------------------

#' Coerce to a matrix
#'
#' `as_matrix()` coerces `x` to a matrix
#'
#'
#' @param x An object to coerce to a matrix.
#' @param ... Objects passed on to methods.
#'
#' @examples
#'
#' as_matrix(mtrx(a = 1:10))
#'
#' @export
as_matrix <- function(x, ...) {
  UseMethod("as_matrix")
}

#' @export
as_matrix.matrix <- function(x, ...) {
  x
}

#' @export
as_matrix.array <- function(x, ...) {

  dim <- vec_dim(x)

  if (vec_size(dim) > 2L) {
    abort("Cannot convert a >2 dimensional array into a matrix.")
  }

  new_matrix(
    .data = vec_data(x),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )

}

#' @export
as_matrix.double <- function(x, ...) {
  .data <- unname(vec_data(x))
  new_matrix(
    .data = .data,
    dim = c(length(x), 1L),
    dimnames = list(names(x), NULL)
  )
}

#' @export
as_matrix.integer <- as_matrix.double

#' @export
as_matrix.logical <- as_matrix.double

#' @export
as_matrix.vctrs_mtrx <- function(x, ...) {
  new_matrix(
    .data = vec_data(x),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}

#' @export
as_matrix.vctrs_rray <- function(x, ...) {

  dim <- vec_dim(x)

  if (vec_size(dim) > 2L) {
    abort("Cannot convert a >2 dimensional rray into a matrix.")
  }

  new_matrix(
    .data = vec_data(x),
    dim = dim,
    dimnames = dim_names(x)
  )

}

#' @export
as.matrix.vctrs_mtrx <- as_matrix.vctrs_mtrx

#' @export
as.matrix.vctrs_rray <- as_matrix.vctrs_rray


# as_mtrx() --------------------------------------------------------------------

#' Coerce to a mtrx
#'
#' Coerce `x` to a mtrx object.
#'
#' @param x An object to coerce to a mtrx.
#' @param ... Objects passed on to methods.
#'
#' @examples
#'
#' as_mtrx(1)
#'
#' as_mtrx(1:10, "col1")
#'
#' ex <- matrix(1:10, nrow = 5, dimnames = list(NULL, c("a", "b")))
#' as_mtrx(ex)
#'
#' @export
as_mtrx <- function(x, ...) {
  UseMethod("as_mtrx")
}

#' @export
as_mtrx.vctrs_mtrx <- function(x, ...) {
  x
}

#' @export
as_mtrx.vctrs_rray <- function(x, ...) {
  dims <- vec_dims(x)

  if (dims > 2) {
    abort("Cannot convert a >2 dimensional rray into a mtrx.")
  }

  new_mtrx(
    .data = vec_data(x),
    size = vec_size(x),
    shape = rray_shape(x),
    dim_names = dim_names(x)
  )
}

#' @export
as_mtrx.array <- as_mtrx.vctrs_rray

#' @rdname as_mtrx
#' @export
as_mtrx.matrix <- function(x, ...) {
  new_mtrx(
    .data = vec_data(x),
    size = vec_size(x),
    shape = rray_shape(x),
    dim_names = dim_names(x)
  )
}

#' @export
as_mtrx.double <- function(x, ...) {
  .data <- unname(vec_data(x))
  new_mtrx(.data, size = vec_size(x), dim_names = dim_names(x))
}

#' @export
as_mtrx.integer <- as_mtrx.double

#' @export
as_mtrx.logical <- as_mtrx.double

