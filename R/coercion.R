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
as_matrix.vctrs_mtrx <- function(x, ...) {
  class(x) <- "matrix"
  dimnames(x) <- list(NULL, attr(x, "col_names"))
  attr(x, "col_names") <- NULL
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
  .dims <- dim(x)
  cnames <- dimnames(x)[[2]] %||% generate_names(.dims[2])
  new_mtrx(vec_data(x), .dims, cnames)
}

#' @rdname as_mtrx
#' @export
#' @param col_name A single character representing the column name
#' to use for the mtrx object.
as_mtrx.numeric <- function(x, col_name = generate_names(1L), ...) {
  new_mtrx(vec_data(x), dim = c(vec_size(x), 1L), col_name)
}

#' @rdname as_mtrx
#' @export
as_mtrx.character <- as_mtrx.numeric

