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
  new_array(
    .data = x,
    dim = rray_dim(x),
    dimnames = dim_names(x)
  )
}

#' @export
as_array.integer <- as_array.double

#' @export
as_array.logical <- as_array.double

#' @export
as_array.vctrs_rray <- function(x, ...) {
  vec_data(x)
}

#' @export
as.array.vctrs_rray <- as_array.vctrs_rray

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
#' as_matrix(rray(1:10))
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
as_matrix.double <- function(x, ...) {
  dim <- rray_dim(x)

  validate_matrix_coercible_dim(dim)

  if (vec_dims(x) > 2L) {
    dim <- dim[c(1L, 2L)]
  }

  new_dim_names <- restore_dim_names(dim_names(x), dim)

  new_matrix(
    .data = x,
    dim = dim,
    dimnames = new_dim_names
  )
}

#' @export
as_matrix.integer <- as_matrix.double

#' @export
as_matrix.logical <- as_matrix.double

#' @export
as_matrix.vctrs_rray <- as_matrix.double

#' @export
as.matrix.vctrs_rray <- as_matrix.vctrs_rray

validate_matrix_coercible_dim <- function(dim) {

  if (vec_size(dim) <= 2L) {
    return(invisible(dim))
  }

  non_matrix_dim <- dim[-c(1L, 2L)]
  bad <- which(non_matrix_dim > 1L)
  ok <- length(bad) == 0L

  if (ok) {
    return(invisible(dim))
  }

  bad <- bad + 2L # readjust after removing matrix dimensions
  bad <- glue::glue_collapse(bad, ", ")

  glubort(
    "A >2D object can only be reduced to a matrix if all ",
    "dimensions except for the first two are 1. This is not the case for ",
    "dimension(s): {bad}."
  )

}

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
as_rray.double <- function(x, ...) {
  new_rray(
    .data = unname(vec_data(x)),
    size = vec_size(x),
    shape = rray_shape(x),
    dim_names = dim_names(x)
  )
}

#' @export
as_rray.integer <- as_rray.double

#' @export
as_rray.logical <- as_rray.double
