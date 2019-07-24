#' Find the inner type of a set of vectors
#'
#' @description
#'
#' `vec_ptype_inner()` finds the inner type of a single vector.
#' `vec_ptype_inner_common()` finds the common inner type of multiple
#' vectors.
#'
#' @details
#'
#' The inner type is a 0-size, 0-shape prototype that represents the
#' "inner" type of `x`. The return value is essentially the
#' object form of `typeof()`.
#'
#' For complex types, a method should be written that returns an
#' empty inner type of that input with no shape or size. If that complex type
#' can have multiple inner types, the method should be written so that it
#' returns the correct inner type of the current input. The return value
#' should _not_ have the class of the classed input.
#'
#' The common inner type is useful alongside [vec_cast_inner()].
#' For example, `rray_bind()` uses `vec_ptype_inner_common()`
#' to determine the common inner type, and then uses
#' `vec_cast_inner()` to cast each of the inputs to the same inner
#' type before binding them together.
#'
#' @param x A vector.
#' @param ... Vectors to determine the common inner type of.
#' @param .ptype If not `NULL`, this overrides the common inner type of `...`.
#'
#' @examples
#' # The inner type of base R atomics uses their constructor
#' vec_ptype_inner(1)
#'
#' # The inner type of an rray is an empty base R object
#' vec_ptype_inner(rray(1))
#'
#' # Find the common inner type of multiple inputs
#' # (the double type wins, the container types are disregarded)
#' vec_ptype_inner_common(1, TRUE, rray(1L))
#'
#' @keywords internal
#' @noRd
vec_ptype_inner <- function(x) {

  if (is.null(x)) {
    return(NULL)
  }

  UseMethod("vec_ptype_inner")
}

vec_ptype_inner.default <- function(x) {
  abort("`x` has an unknown inner type.")
}

vec_ptype_inner.logical <- function(x) {
  logical()
}

vec_ptype_inner.integer <- function(x) {
  integer()
}

vec_ptype_inner.double <- function(x) {
  double()
}

vec_ptype_inner.character <- function(x) {
  character()
}

vec_ptype_inner.vctrs_rray_lgl <- function(x) {
  logical()
}

vec_ptype_inner.vctrs_rray_int <- function(x) {
  integer()
}

vec_ptype_inner.vctrs_rray_dbl <- function(x) {
  double()
}

vec_ptype_inner.vctrs_unspecified <- function(x) {
  logical()
}

# ------------------------------------------------------------------------------

vec_ptype_inner2 <- function(x, y) {
  vec_ptype2(vec_ptype_inner(x), vec_ptype_inner(y))
}

# ------------------------------------------------------------------------------

vec_ptype_inner_common <- function(..., .ptype = NULL) {

  if (!is.null(.ptype)) {
    return(vec_ptype_inner(.ptype))
  }

  args <- list2(...)
  n_args <- length(args)

  if (n_args == 0L) {
    return(NULL)
  }

  if (n_args == 1L) {
    return(vec_ptype_inner(args[[1]]))
  }

  reduce(args, vec_ptype_inner2)
}
