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
  class(x) <- "matrix"
  dimnames(x) <- dim_names(x)
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

#' @rdname as_mtrx
#' @export
as_mtrx.matrix <- function(x, ...) {
  dim <- vec_dim(x)

  row_names <- rownames(x) %||% character()
  col_names <- colnames(x) %||% character()

  new_mtrx(
    .data = vec_data(x),
    dim = dim,
    row_names = row_names,
    col_names = col_names
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
    dim = c(vec_size(x), 1L),
    row_names = row_names,
    col_names = col_name
  )

}

