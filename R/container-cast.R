#' Cast to a container type
#'
#' @description
#'
#' `vec_cast_container()` casts `x` to the "container" type of `to`. This should
#' make the following assumptions:
#'
#' - `x` has the correct shape.
#' - `x` has the correct internal type.
#'
#' `vec_cast_container_common()` casts multiple vectors to a common container
#' type.
#'
#' @details
#'
#' `vec_cast_container()` is useful for restoring input that has been modified
#' in some way back to its original container type without altering the internal
#' type or shape of the modified input. For example, the `>` method
#' for rrays takes two inputs, finds the common container type between them
#' and returns a logical vector wrapped in that container type.
#'
#' When casting between base R types, this simply returns `x`.
#'
#' @param x Vector to cast.
#'
#' @param to Container type to cast to.
#'
#' @param ... Vectors to cast to a common container type.
#'
#' @param .to If not `NULL`, overrides the common container
#' type to cast `...` to.
#'
#' @examples
#'
#' # Upcasting to an rray. Still a logical
#' vec_cast_container(TRUE, rray(1))
#'
#' # Downcasting to a double, no longer an rray
#' # (the "container" here is just a base R object)
#' vec_cast_container(rray(1), TRUE)
#'
#' # Shape of `x` is kept
#' vec_cast_container(matrix(1:5), rray(1))
#'
#' # Dim names of `x` are kept
#' x <- rray(1:2, dim_names = list(c("r1", "r2")))
#' vec_cast_container(x, 1)
#'
#' @export
vec_cast_container <- function(x, to) {

  if (is.null(x) || is.null(to)) {
    return(x)
  }

  UseMethod("vec_cast_container", to)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_container
#' @export vec_cast_container.default
#' @method vec_cast_container default
vec_cast_container.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_container
#' @export vec_cast_container.logical
#' @method vec_cast_container logical
vec_cast_container.logical <- function(x, to) {
  UseMethod("vec_cast_container.logical")
}

#' @export
#' @method vec_cast_container.logical default
vec_cast_container.logical.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_container.logical logical
vec_cast_container.logical.logical <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_container.logical double
vec_cast_container.logical.double <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_container.logical integer
vec_cast_container.logical.integer <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_container.logical vctrs_rray
vec_cast_container.logical.vctrs_rray <- function(x, to) {
  vec_data(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_container
#' @export vec_cast_container.double
#' @method vec_cast_container double
vec_cast_container.double <- function(x, to) {
  UseMethod("vec_cast_container.double")
}

#' @export
#' @method vec_cast_container.double default
vec_cast_container.double.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_container.double logical
vec_cast_container.double.logical <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_container.double double
vec_cast_container.double.double <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_container.double integer
vec_cast_container.double.integer <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_container.double vctrs_rray
vec_cast_container.double.vctrs_rray <- function(x, to) {
  vec_data(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_container
#' @export vec_cast_container.integer
#' @method vec_cast_container integer
vec_cast_container.integer <- function(x, to) {
  UseMethod("vec_cast_container.integer")
}

#' @export
#' @method vec_cast_container.integer default
vec_cast_container.integer.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_container.integer logical
vec_cast_container.integer.logical <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_container.integer double
vec_cast_container.integer.double <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_container.integer integer
vec_cast_container.integer.integer <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_container.integer vctrs_rray
vec_cast_container.integer.vctrs_rray <- function(x, to) {
  vec_data(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_container
#' @export vec_cast_container.vctrs_rray
#' @method vec_cast_container vctrs_rray
vec_cast_container.vctrs_rray <- function(x, to) {
  UseMethod("vec_cast_container.vctrs_rray")
}

#' @export
#' @method vec_cast_container.vctrs_rray default
vec_cast_container.vctrs_rray.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_container.vctrs_rray logical
vec_cast_container.vctrs_rray.logical <- function(x, to) {
  dim <- vec_dim(x)
  new_rray(x, size = dim[1], shape = dim[-1], dim_names = rray_dim_names(x))
}

#' @export
#' @method vec_cast_container.vctrs_rray integer
vec_cast_container.vctrs_rray.integer <- vec_cast_container.vctrs_rray.logical

#' @export
#' @method vec_cast_container.vctrs_rray double
vec_cast_container.vctrs_rray.double <- vec_cast_container.vctrs_rray.logical

#' @export
#' @method vec_cast_container.vctrs_rray vctrs_rray
vec_cast_container.vctrs_rray.vctrs_rray <- function(x, to) {
  x
}

# ------------------------------------------------------------------------------

#' @rdname vec_cast_container
#' @export
vec_cast_container_common <- function(..., .to = NULL) {
  args <- list2(...)
  container <- vec_type_container_common(!!!args, .ptype = .to)
  map(args, vec_cast_container, to = container)
}
