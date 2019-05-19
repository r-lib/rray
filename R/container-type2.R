#' Find the container type of a pair of vectors
#'
#' `vec_type_container2()` finds the common container type of two vectors.
#' Like `vec_type2()`, `vec_type_container2()` powers coercion but should
#' usually not be called directly. Instead, [vec_type_container_common()]
#' should be used.
#'
#' @param x,y Vectors.
#'
#' @export
vec_type_container2 <- function(x, y) {

  if (is.null(x) || is.null(y)) {
    return(vec_type_container(x))
  }

  UseMethod("vec_type_container2", y)
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
#' @method vec_type_container2.logical vctrs_rray_int
vec_type_container2.logical.vctrs_rray_int <- function(x, y) {
  shared$empty_rray_lgl
}

#' @export
#' @method vec_type_container2.logical vctrs_rray_dbl
vec_type_container2.logical.vctrs_rray_dbl <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.logical vctrs_rray_lgl
vec_type_container2.logical.vctrs_rray_lgl <- vec_type_container2.logical.vctrs_rray_int

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
#' @method vec_type_container2.integer vctrs_rray_int
vec_type_container2.integer.vctrs_rray_int <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.integer vctrs_rray_dbl
vec_type_container2.integer.vctrs_rray_dbl <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.integer vctrs_rray_lgl
vec_type_container2.integer.vctrs_rray_lgl <- vec_type_container2.logical.vctrs_rray_int

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
#' @method vec_type_container2.double vctrs_rray_int
vec_type_container2.double.vctrs_rray_int <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.double vctrs_rray_dbl
vec_type_container2.double.vctrs_rray_dbl <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.double vctrs_rray_lgl
vec_type_container2.double.vctrs_rray_lgl <- vec_type_container2.logical.vctrs_rray_int

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
#' @method vec_type_container2.character vctrs_rray_int
vec_type_container2.character.vctrs_rray_int <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.character vctrs_rray_dbl
vec_type_container2.character.vctrs_rray_dbl <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.character vctrs_rray_lgl
vec_type_container2.character.vctrs_rray_lgl <- vec_type_container2.logical.vctrs_rray_int

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_type_container2
#' @export vec_type_container2.vctrs_rray_lgl
#' @method vec_type_container2 vctrs_rray_lgl
vec_type_container2.vctrs_rray_lgl <- function(x, y) {
  UseMethod("vec_type_container2.vctrs_rray_lgl")
}

#' @export
#' @method vec_type_container2.vctrs_rray_lgl default
vec_type_container2.vctrs_rray_lgl.default <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @export
#' @method vec_type_container2.vctrs_rray_lgl logical
vec_type_container2.vctrs_rray_lgl.logical <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_lgl double
vec_type_container2.vctrs_rray_lgl.double <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_lgl integer
vec_type_container2.vctrs_rray_lgl.integer <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_lgl character
vec_type_container2.vctrs_rray_lgl.character <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_lgl vctrs_rray_int
vec_type_container2.vctrs_rray_lgl.vctrs_rray_int <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_lgl vctrs_rray_dbl
vec_type_container2.vctrs_rray_lgl.vctrs_rray_dbl <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_lgl vctrs_rray_lgl
vec_type_container2.vctrs_rray_lgl.vctrs_rray_lgl <- vec_type_container2.logical.vctrs_rray_int

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_type_container2
#' @export vec_type_container2.vctrs_rray_dbl
#' @method vec_type_container2 vctrs_rray_dbl
vec_type_container2.vctrs_rray_dbl <- function(x, y) {
  UseMethod("vec_type_container2.vctrs_rray_dbl")
}

#' @export
#' @method vec_type_container2.vctrs_rray_dbl default
vec_type_container2.vctrs_rray_dbl.default <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @export
#' @method vec_type_container2.vctrs_rray_dbl logical
vec_type_container2.vctrs_rray_dbl.logical <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_dbl double
vec_type_container2.vctrs_rray_dbl.double <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_dbl integer
vec_type_container2.vctrs_rray_dbl.integer <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_dbl character
vec_type_container2.vctrs_rray_dbl.character <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_dbl vctrs_rray_int
vec_type_container2.vctrs_rray_dbl.vctrs_rray_int <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_dbl vctrs_rray_dbl
vec_type_container2.vctrs_rray_dbl.vctrs_rray_dbl <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_dbl vctrs_rray_lgl
vec_type_container2.vctrs_rray_dbl.vctrs_rray_lgl <- vec_type_container2.logical.vctrs_rray_int

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_type_container2
#' @export vec_type_container2.vctrs_rray_int
#' @method vec_type_container2 vctrs_rray_int
vec_type_container2.vctrs_rray_int <- function(x, y) {
  UseMethod("vec_type_container2.vctrs_rray_int")
}

#' @export
#' @method vec_type_container2.vctrs_rray_int default
vec_type_container2.vctrs_rray_int.default <- function(x, y) {
  stop_incompatible_type(x, y)
}

#' @export
#' @method vec_type_container2.vctrs_rray_int logical
vec_type_container2.vctrs_rray_int.logical <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_int double
vec_type_container2.vctrs_rray_int.double <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_int integer
vec_type_container2.vctrs_rray_int.integer <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_int character
vec_type_container2.vctrs_rray_int.character <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_int vctrs_rray_int
vec_type_container2.vctrs_rray_int.vctrs_rray_int <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_int vctrs_rray_dbl
vec_type_container2.vctrs_rray_int.vctrs_rray_dbl <- vec_type_container2.logical.vctrs_rray_int

#' @export
#' @method vec_type_container2.vctrs_rray_int vctrs_rray_lgl
vec_type_container2.vctrs_rray_int.vctrs_rray_lgl <- vec_type_container2.logical.vctrs_rray_int

# ------------------------------------------------------------------------------

vec_type_container_common <- function(..., .ptype = NULL) {

  if (!is.null(.ptype)) {
    return(vec_type_container(.ptype))
  }

  args <- compact(list2(...))
  n_args <- length(args)

  if (n_args == 0L) {
    return(NULL)
  }

  if (n_args == 1L) {
    return(vec_type_container(args[[1]]))
  }

  reduce(args, vec_type_container2)
}
