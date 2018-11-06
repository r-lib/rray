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
  array(x, dim = c(length(x), 1L))
}

#' @export
as_array.vctrs_rray <- function(x, ...) {
  class(x) <- "array"
  dimnames(x) <- attr(x, "dim_names")
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
#' @param x An object to coerce to a rray.
#' @param ... Objects passed on to methods.
#'
#' @examples
#'
#' as_rray(1)
#'
#' as_rray(1:10)
#'
#' as_rray(letters)
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
as_rray.array <- function(x, ...) {
  new_rray(vec_data(x), dim = vec_dim(x), dim_names = dim_names(x))
}

#' @export
as_rray.matrix <- function(x, ...) {
  new_rray(vec_data(x), dim = vec_dim(x), dim_names = dim_names(x))
}

#' @export
as_rray.numeric <- function(x, ...) {
  new_rray(vec_data(x), dim = c(vec_size(x), 1L), )
}

