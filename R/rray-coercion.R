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
as_array.numeric <- function(x, ...) {
  # enforce column
  array(x, dim = c(length(x), 1L))
}

#' @export
as_array.logical <- as_array.numeric

#' @export
as_array.vctrs_mtrx <- function(x, ...) {
  as.array(as_matrix(x))
}

#' @export
as_array.vctrs_rray <- function(x, ...) {
  # this is cheap, but maybe not the best way
  dim_nms <- dim_names(x)

  # we allow dim_names for 0 dim dimensions for the prototype
  # but base R does not.
  dim_nms[vec_dim(x) == 0] <- list(character())

  class(x) <- "array"
  dimnames(x) <- dim_nms
  attr(x, "dim_names") <- NULL
  x
}

#' @export
as.array.vctrs_rray <- as_array.vctrs_rray

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
#' @inheritParams new_mtrx
#' @param col_name A single character for the column name. The default is
#' to have no column name.
as_rray.numeric <- function(x, ..., row_names = character(), col_name = character()) {
  size <- vec_size(x)
  if (size == 0L) {
    new_rray()
  } else {
    new_rray(vec_data(x), size = size, shape = 1L, dim_names = list(row_names, col_name))
  }
}

#' @export
as_rray.integer <- as_rray.numeric

#' @export
as_rray.logical <- as_rray.numeric
