#' Find the container type of a pair of vectors
#'
#' `vec_type_container2()` finds the common container type of two vectors.
#' Like `vec_type2()`, `vec_type_container2()` powers coercion but should
#' usually not be called directly. Instead, [vec_type_container_common()]
#' should be used.
#'
#' @param x,y Vectors.
#'
#' @examples
#'
#' vec_type_container2(1, 2L)
#'
#' vec_type_container2(1, rray(2L))
#'
#' @export
vec_type_container2 <- function(x, y) {

  if (is.null(x)) {
    return(vec_type_container(y))
  }

  if (is.null(y)) {
    return(vec_type_container(x))
  }

  UseMethod("vec_type_container2", y)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_type_container2
#' @export vec_type_container2.default
#' @method vec_type_container2 default
vec_type_container2.default <- function(x, y) {
  stop_incompatible_type(x, y)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_type_container2
#' @export vec_type_container2.logical
#' @method vec_type_container2 logical
vec_type_container2.logical <- function(x, y) {
  UseMethod("vec_type_container2.logical")
}

#' @export
#' @method vec_type_container2.logical default
vec_type_container2.logical.default <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @export
#' @method vec_type_container2.logical logical
vec_type_container2.logical.logical <- function(x, y) {
  logical()
}

#' @export
#' @method vec_type_container2.logical double
vec_type_container2.logical.double <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.logical integer
vec_type_container2.logical.integer <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.logical character
vec_type_container2.logical.character <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.logical vctrs_rray
vec_type_container2.logical.vctrs_rray <- function(x, y) {
  shared$empty_rray_lgl
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_type_container2
#' @export vec_type_container2.integer
#' @method vec_type_container2 integer
vec_type_container2.integer <- function(x, y) {
  UseMethod("vec_type_container2.integer")
}

#' @export
#' @method vec_type_container2.integer default
vec_type_container2.integer.default <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @export
#' @method vec_type_container2.integer logical
vec_type_container2.integer.logical <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.integer double
vec_type_container2.integer.double <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.integer integer
vec_type_container2.integer.integer <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.integer character
vec_type_container2.integer.character <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.integer vctrs_rray
vec_type_container2.integer.vctrs_rray <- vec_type_container2.logical.vctrs_rray

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_type_container2
#' @export vec_type_container2.double
#' @method vec_type_container2 double
vec_type_container2.double <- function(x, y) {
  UseMethod("vec_type_container2.double")
}

#' @export
#' @method vec_type_container2.double default
vec_type_container2.double.default <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @export
#' @method vec_type_container2.double logical
vec_type_container2.double.logical <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.double double
vec_type_container2.double.double <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.double integer
vec_type_container2.double.integer <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.double character
vec_type_container2.double.character <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.double vctrs_rray
vec_type_container2.double.vctrs_rray <- vec_type_container2.logical.vctrs_rray

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_type_container2
#' @export vec_type_container2.character
#' @method vec_type_container2 character
vec_type_container2.character <- function(x, y) {
  UseMethod("vec_type_container2.character")
}

#' @export
#' @method vec_type_container2.character default
vec_type_container2.character.default <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @export
#' @method vec_type_container2.character logical
vec_type_container2.character.logical <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.character double
vec_type_container2.character.double <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.character integer
vec_type_container2.character.integer <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.character character
vec_type_container2.character.character <- vec_type_container2.logical.logical

#' @export
#' @method vec_type_container2.character vctrs_rray
vec_type_container2.character.vctrs_rray <- vec_type_container2.logical.vctrs_rray

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_type_container2
#' @export vec_type_container2.vctrs_rray
#' @method vec_type_container2 vctrs_rray
vec_type_container2.vctrs_rray <- function(x, y) {
  UseMethod("vec_type_container2.vctrs_rray")
}

#' @export
#' @method vec_type_container2.vctrs_rray default
vec_type_container2.vctrs_rray.default <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @export
#' @method vec_type_container2.vctrs_rray logical
vec_type_container2.vctrs_rray.logical <- vec_type_container2.logical.vctrs_rray

#' @export
#' @method vec_type_container2.vctrs_rray double
vec_type_container2.vctrs_rray.double <- vec_type_container2.logical.vctrs_rray

#' @export
#' @method vec_type_container2.vctrs_rray integer
vec_type_container2.vctrs_rray.integer <- vec_type_container2.logical.vctrs_rray

#' @export
#' @method vec_type_container2.vctrs_rray character
vec_type_container2.vctrs_rray.character <- vec_type_container2.logical.vctrs_rray

#' @export
#' @method vec_type_container2.vctrs_rray vctrs_rray
vec_type_container2.vctrs_rray.vctrs_rray <- vec_type_container2.logical.vctrs_rray
