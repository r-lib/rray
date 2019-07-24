#' Find the container type of a set of vectors
#'
#' @description
#'
#' `vec_ptype_container()` finds the container type of a single vector.
#' `vec_ptype_container_common()` finds the common container type of multiple
#' vectors.
#'
#' @details
#'
#' The container type is a 0-size, 0-shape prototype that represents the
#' "container" of `x`. A container is generally an S3 class that can hold
#' multiple different "inner" types. For example, an rray is a container
#' that can hold integers, logicals, or doubles as inner types.
#'
#' For base R objects, this returns `logical()`.
#'
#' For more complex types, a method should be written that returns an
#' empty version of the input with no shape or size. If that complex type
#' can have multiple inner types, pick one to return consistently for all
#' inner type variations, as there should be no reliance on the inner type
#' when inspecting the container type.
#'
#' The common container type is useful alongside [vec_cast_container()].
#' For example, `rray_greater()` uses `vec_ptype_container_common()`
#' to determine the common container for the output, and then uses
#' `vec_cast_container()` to restore the logical result of the comparison
#' to either an rray or an array, depending on the container type.
#'
#' Critically, the container type is independent of the _inner_ type of a
#' vector. This means that while `vec_ptype_common(character(), numeric())`
#' is an error, `vec_ptype_container_common(character(), numeric())`
#' returns `logical()` because they are both base R containers.
#'
#' @param x Vector to compute the container type for.
#'
#' @param ... Vectors to compute the common container type for.
#'
#' @param .ptype If not `NULL`, overrides the common container type of `...`.
#'
#' @examples
#' # The container of base R atomics is just logical()
#' vec_ptype_container(1)
#'
#' # The container of an rray is an empty logical rray
#' vec_ptype_container(rray(1))
#'
#' # Find the common container of multiple types
#' # (the rray type is more complex here, and becomes the common container)
#' vec_ptype_container_common(1, TRUE, rray(1L))
#'
#' # Not an error!
#' vec_ptype_container_common(character(), logical())
#'
#' @keywords internal
#' @noRd
vec_ptype_container <- function(x) {

  if (is.null(x)) {
    return(NULL)
  }

  UseMethod("vec_ptype_container")
}

vec_ptype_container.default <- function(x) {
  abort("`x` has an unknown container type.")
}

vec_ptype_container.logical <- function(x) {
  logical()
}

vec_ptype_container.integer <- vec_ptype_container.logical

vec_ptype_container.double <- vec_ptype_container.logical

vec_ptype_container.character <- vec_ptype_container.logical

vec_ptype_container.vctrs_rray <- function(x) {
  shared$empty_rray_lgl
}

vec_ptype_container.vctrs_unspecified <- vec_ptype_container.integer

# ------------------------------------------------------------------------------

vec_ptype_container_common <- function(..., .ptype = NULL) {

  if (!is.null(.ptype)) {
    return(vec_ptype_container(.ptype))
  }

  args <- compact(list2(...))
  n_args <- length(args)

  if (n_args == 0L) {
    return(NULL)
  }

  if (n_args == 1L) {
    return(vec_ptype_container(args[[1]]))
  }

  reduce(args, vec_ptype_container2)
}
