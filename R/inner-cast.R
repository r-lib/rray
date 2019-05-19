#' Cast to an inner type
#'
#' @description
#'
#' `vec_cast_inner()` casts `x` to the "inner" type of `to`, maintaining
#' the size and shape of `x`.
#'
#' `vec_cast_inner_common()` casts its input to a common inner type, maintaining
#' the size and shape of each input.
#'
#' @details
#'
#' `vec_cast_inner()` makes no guarantees that the attributes of `x` are
#' retained (with the exception of the `dim`), only that the inner type will
#' be the same after the cast. This function is usually called internally to
#' force a common inner type, perform a manipulation, and then other
#' functionality restores the attributes and container type.
#'
#' As opposed to `vec_cast()`, the _shape_ of `x` is maintained.
#'
#' @param x Vector to cast.
#'
#' @param to Object with the inner type to cast to.
#'
#' @param ... Vectors to cast to a common inner type.
#'
#' @param .to If not `NULL`, the inner type to force all objects in `...`
#' to be cast to.
#'
#' @examples
#'
#' # Casting from logical to double.
#' # Not worrying about the fact that `to`
#' # is an rray.
#' vec_cast_inner(TRUE, rray(1))
#'
#' # Casting from a double to a logical
#' # Note that the rray attributes are lost.
#' # This is expected as the only thing
#' # `vec_cast_inner()` cares about is
#' # the internal type
#' vec_cast_inner(rray(1), TRUE)
#'
#' # Shape of `x` is kept
#' vec_cast_inner(matrix(c(TRUE, FALSE)), rray(1))
#'
#' @export
vec_cast_inner <- function(x, to) {

  if (is.null(x) || is.null(to)) {
    return(x)
  }

  UseMethod("vec_cast_inner", to)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_inner
#' @export vec_cast_inner.default
#' @method vec_cast_inner default
vec_cast_inner.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_inner
#' @export vec_cast_inner.logical
#' @method vec_cast_inner logical
vec_cast_inner.logical <- function(x, to) {
  UseMethod("vec_cast_inner.logical")
}

#' @export
#' @method vec_cast_inner.logical default
vec_cast_inner.logical.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_inner.logical logical
vec_cast_inner.logical.logical <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.logical double
vec_cast_inner.logical.double <- function(x, to) {
  vec_inner_caster(x, logical())
}

#' @export
#' @method vec_cast_inner.logical integer
vec_cast_inner.logical.integer <- function(x, to) {
  vec_inner_caster(x, logical())
}

#' @export
#' @method vec_cast_inner.logical character
vec_cast_inner.logical.character <- function(x, to) {
  vec_inner_caster(x, logical())
}

#' @export
#' @method vec_cast_inner.logical vctrs_rray_int
vec_cast_inner.logical.vctrs_rray_int <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), logical())
}

#' @export
#' @method vec_cast_inner.logical vctrs_rray_dbl
vec_cast_inner.logical.vctrs_rray_dbl <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), logical())
}

#' @export
#' @method vec_cast_inner.logical vctrs_rray_lgl
vec_cast_inner.logical.vctrs_rray_lgl <- function(x, to) {
  x
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_inner
#' @export vec_cast_inner.integer
#' @method vec_cast_inner integer
vec_cast_inner.integer <- function(x, to) {
  UseMethod("vec_cast_inner.integer")
}

#' @export
#' @method vec_cast_inner.integer default
vec_cast_inner.integer.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_inner.integer logical
vec_cast_inner.integer.logical <- function(x, to) {
  vec_inner_caster(x, integer())
}

#' @export
#' @method vec_cast_inner.integer double
vec_cast_inner.integer.double <- function(x, to) {
  vec_inner_caster(x, integer())
}

#' @export
#' @method vec_cast_inner.integer integer
vec_cast_inner.integer.integer <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.integer character
vec_cast_inner.integer.character <- function(x, to) {
  vec_inner_caster(x, integer())
}

#' @export
#' @method vec_cast_inner.integer vctrs_rray_int
vec_cast_inner.integer.vctrs_rray_int <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.integer vctrs_rray_dbl
vec_cast_inner.integer.vctrs_rray_dbl <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), integer())
}

#' @export
#' @method vec_cast_inner.integer vctrs_rray_lgl
vec_cast_inner.integer.vctrs_rray_lgl <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), integer())
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_inner
#' @export vec_cast_inner.double
#' @method vec_cast_inner double
vec_cast_inner.double <- function(x, to) {
  UseMethod("vec_cast_inner.double")
}

#' @export
#' @method vec_cast_inner.double default
vec_cast_inner.double.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_inner.double logical
vec_cast_inner.double.logical <- function(x, to) {
  vec_inner_caster(x, double())
}

#' @export
#' @method vec_cast_inner.double double
vec_cast_inner.double.double <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.double integer
vec_cast_inner.double.integer <- function(x, to) {
  vec_inner_caster(x, double())
}

#' @export
#' @method vec_cast_inner.double character
vec_cast_inner.double.character <- function(x, to) {
  vec_inner_caster(x, double())
}

#' @export
#' @method vec_cast_inner.double vctrs_rray_int
vec_cast_inner.double.vctrs_rray_int <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), double())
}

#' @export
#' @method vec_cast_inner.double vctrs_rray_dbl
vec_cast_inner.double.vctrs_rray_dbl <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.double vctrs_rray_lgl
vec_cast_inner.double.vctrs_rray_lgl <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), double())
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_inner
#' @export vec_cast_inner.character
#' @method vec_cast_inner character
vec_cast_inner.character <- function(x, to) {
  UseMethod("vec_cast_inner.character")
}

#' @export
#' @method vec_cast_inner.character default
vec_cast_inner.character.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_inner.character logical
vec_cast_inner.character.logical <- function(x, to) {
  vec_inner_caster(x, character())
}

#' @export
#' @method vec_cast_inner.character double
vec_cast_inner.character.double <- function(x, to) {
  vec_inner_caster(x, character())
}

#' @export
#' @method vec_cast_inner.character integer
vec_cast_inner.character.integer <- function(x, to) {
  vec_inner_caster(x, character())
}

#' @export
#' @method vec_cast_inner.character character
vec_cast_inner.character.character <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.character vctrs_rray_int
vec_cast_inner.character.vctrs_rray_int <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), character())
}

#' @export
#' @method vec_cast_inner.character vctrs_rray_dbl
vec_cast_inner.character.vctrs_rray_dbl <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), character())
}

#' @export
#' @method vec_cast_inner.character vctrs_rray_lgl
vec_cast_inner.character.vctrs_rray_lgl <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), character())
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_inner
#' @export vec_cast_inner.vctrs_rray_lgl
#' @method vec_cast_inner vctrs_rray_lgl
vec_cast_inner.vctrs_rray_lgl <- function(x, to) {
  UseMethod("vec_cast_inner.vctrs_rray_lgl")
}

#' @export
#' @method vec_cast_inner.vctrs_rray_lgl default
vec_cast_inner.vctrs_rray_lgl.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_inner.vctrs_rray_lgl logical
vec_cast_inner.vctrs_rray_lgl.logical <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.vctrs_rray_lgl double
vec_cast_inner.vctrs_rray_lgl.double <- function(x, to) {
  vec_inner_caster(x, logical())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_lgl integer
vec_cast_inner.vctrs_rray_lgl.integer <- function(x, to) {
  vec_inner_caster(x, logical())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_lgl character
vec_cast_inner.vctrs_rray_lgl.character <- function(x, to) {
  vec_inner_caster(x, logical())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_lgl vctrs_rray_int
vec_cast_inner.vctrs_rray_lgl.vctrs_rray_int <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), logical())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_lgl vctrs_rray_dbl
vec_cast_inner.vctrs_rray_lgl.vctrs_rray_dbl <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), logical())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_lgl vctrs_rray_lgl
vec_cast_inner.vctrs_rray_lgl.vctrs_rray_lgl <- function(x, to) {
  x
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_inner
#' @export vec_cast_inner.vctrs_rray_dbl
#' @method vec_cast_inner vctrs_rray_dbl
vec_cast_inner.vctrs_rray_dbl <- function(x, to) {
  UseMethod("vec_cast_inner.vctrs_rray_dbl")
}

#' @export
#' @method vec_cast_inner.vctrs_rray_dbl default
vec_cast_inner.vctrs_rray_dbl.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_inner.vctrs_rray_dbl logical
vec_cast_inner.vctrs_rray_dbl.logical <- function(x, to) {
  vec_inner_caster(x, double())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_dbl double
vec_cast_inner.vctrs_rray_dbl.double <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.vctrs_rray_dbl integer
vec_cast_inner.vctrs_rray_dbl.integer <- function(x, to) {
  vec_inner_caster(x, double())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_dbl character
vec_cast_inner.vctrs_rray_dbl.character <- function(x, to) {
  vec_inner_caster(x, double())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_dbl vctrs_rray_int
vec_cast_inner.vctrs_rray_dbl.vctrs_rray_int <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), double())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_dbl vctrs_rray_dbl
vec_cast_inner.vctrs_rray_dbl.vctrs_rray_dbl <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.vctrs_rray_dbl vctrs_rray_lgl
vec_cast_inner.vctrs_rray_dbl.vctrs_rray_lgl <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), double())
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname vec_cast_inner
#' @export vec_cast_inner.vctrs_rray_int
#' @method vec_cast_inner vctrs_rray_int
vec_cast_inner.vctrs_rray_int <- function(x, to) {
  UseMethod("vec_cast_inner.vctrs_rray_int")
}

#' @export
#' @method vec_cast_inner.vctrs_rray_int default
vec_cast_inner.vctrs_rray_int.default <- function(x, to) {
  stop_incompatible_cast(x, to)
}

#' @export
#' @method vec_cast_inner.vctrs_rray_int logical
vec_cast_inner.vctrs_rray_int.logical <- function(x, to) {
  vec_inner_caster(x, integer())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_int double
vec_cast_inner.vctrs_rray_int.double <- function(x, to) {
  vec_inner_caster(x, integer())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_int integer
vec_cast_inner.vctrs_rray_int.integer <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.vctrs_rray_int character
vec_cast_inner.vctrs_rray_int.character <- function(x, to) {
  vec_inner_caster(x, integer())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_int vctrs_rray_int
vec_cast_inner.vctrs_rray_int.vctrs_rray_int <- function(x, to) {
  x
}

#' @export
#' @method vec_cast_inner.vctrs_rray_int vctrs_rray_dbl
vec_cast_inner.vctrs_rray_int.vctrs_rray_dbl <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), integer())
}

#' @export
#' @method vec_cast_inner.vctrs_rray_int vctrs_rray_lgl
vec_cast_inner.vctrs_rray_int.vctrs_rray_lgl <- function(x, to) {
  vec_inner_caster(vec_data_fast(x), integer())
}

# ------------------------------------------------------------------------------

#' @rdname vec_cast_inner
#' @export
vec_cast_inner_common <- function(..., .to = NULL) {
  args <- list2(...)
  type <- vec_type_inner_common(!!!args, .ptype = .to)
  map(args, vec_cast_inner, to = type)
}

# ------------------------------------------------------------------------------

vec_inner_caster <- function(x, inner) {
  vec_cast(x, new_shape(inner, shape = rray_shape(x)))
}

new_shape <- function(type, shape = NULL) {
  if (length(shape) == 0L) {
    type
  }
  else {
    structure(type, dim = c(0L, shape))
  }
}
