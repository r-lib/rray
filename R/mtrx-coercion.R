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
as_matrix.numeric <- function(x, ...) {
  # consistent with R and vctrs
  matrix(x, ncol = 1)
}

#' @export
as_matrix.vctrs_mtrx <- function(x, ...) {
  dim_nms <- dim_names(x)

  # we allow dim_names for 0 dim dimensions for the prototype
  # but base R does not.
  dim_nms[vec_dim(x) == 0] <- list(character())

  class(x) <- "matrix"
  dimnames(x) <- dim_nms
  attr(x, "dim_names") <- NULL
  x
}

#' @export
as.matrix.vctrs_mtrx <- as_matrix.vctrs_mtrx


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
#' as_mtrx(letters)
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

  x_dim_names <- dim_names(x)

  new_mtrx(
    .data = vec_data(x),
    n_row = vec_size(x),
    n_col = rray_shape(x),
    row_names = x_dim_names[[1]],
    col_names = x_dim_names[[2]]
  )
}

#' @rdname as_mtrx
#' @export
as_mtrx.matrix <- function(x, ...) {

  x_dim_names <- dim_names(x)

  new_mtrx(
    .data = vec_data(x),
    n_row = vec_size(x),
    n_col = rray_shape(x),
    row_names = x_dim_names[[1]],
    col_names = x_dim_names[[2]]
  )

}

#' @rdname as_mtrx
#' @export
as_mtrx.numeric <- function(x,
                            ...,
                            col_name = character(),
                            row_names = character()
                            ) {

  x <- unname(x)

  new_mtrx(
    .data = vec_data(x),
    n_row = vec_size(x),
    n_col = 1L,
    row_names = row_names,
    col_names = col_name
  )

}

