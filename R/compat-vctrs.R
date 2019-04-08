#' @export
vec_restore.vctrs_rray <- function(x, to, ..., i = NULL) {
  x_dim <- vec_dim(x)

  new_rray(
    .data = vec_data(x),
    size = x_dim[1],
    shape = x_dim[-1],
    dim_names = dim_names(x)
  )
}

# vec_type2 boilerplate --------------------------------------------------------

#' vctrs compatibility functions
#'
#' These functions are the extensions that allow rray objects to
#' work with vctrs.
#'
#' @param x,y Objects.
#' @param to Type to cast to.
#' @param op An arithmetic operator as a string.
#'
#' @name vctrs-compat
#'
NULL

#' @export
#' @rdname vctrs-compat
#' @method vec_type2 vctrs_rray
#' @export vec_type2.vctrs_rray
vec_type2.vctrs_rray <- function(x, y) UseMethod("vec_type2.vctrs_rray", y)

#' @method vec_type2.vctrs_rray default
#' @export
vec_type2.vctrs_rray.default <- function(x, y) stop_incompatible_type(x, y)

#' @method vec_type2.vctrs_rray vctrs_unspecified
#' @export
vec_type2.vctrs_rray.vctrs_unspecified <- function(x, y) x

# vec_type2 vctrs_rray <-> vctrs_rray ------------------------------------------

# vec_type2 makes no attempt to recover dim names, as they are not part of the type.
# type = class + shape (at least for rray objects)

#' @method vec_type2.vctrs_rray vctrs_rray
#' @export
vec_type2.vctrs_rray.vctrs_rray <- function(x, y) {
  inner_type <- rray_type_inner2(x, y)
  new_rray(inner_type, shape = rray_shape2(x, y))
}

# vec_type2 vctrs_rray <-> double/matrix/array ---------------------------------

#' @method vec_type2.vctrs_rray double
#' @export
vec_type2.vctrs_rray.double <- vec_type2.vctrs_rray.vctrs_rray

#' @method vec_type2.double vctrs_rray
#' @export
vec_type2.double.vctrs_rray <- vec_type2.vctrs_rray.vctrs_rray

# vec_type2 vctrs_rray <-> integer/matrix/array --------------------------------

#' @method vec_type2.vctrs_rray integer
#' @export
vec_type2.vctrs_rray.integer <- vec_type2.vctrs_rray.vctrs_rray

#' @method vec_type2.integer vctrs_rray
#' @export
vec_type2.integer.vctrs_rray <- vec_type2.vctrs_rray.vctrs_rray

# vec_type2 vctrs_rray <-> logical/matrix/array --------------------------------

#' @method vec_type2.vctrs_rray logical
#' @export
vec_type2.vctrs_rray.logical <- vec_type2.vctrs_rray.vctrs_rray

#' @method vec_type2.logical vctrs_rray
#' @export
vec_type2.logical.vctrs_rray <- vec_type2.vctrs_rray.vctrs_rray

# vec_cast boilerplate ---------------------------------------------------------

#' @export
#' @rdname vctrs-compat
#' @method vec_cast vctrs_rray
#' @export vec_cast.vctrs_rray
vec_cast.vctrs_rray <- function(x, to) UseMethod("vec_cast.vctrs_rray")

#' @method vec_cast.vctrs_rray default
#' @export
vec_cast.vctrs_rray.default <- function(x, to) stop_incompatible_cast(x, to)

# vec_cast vctrs_rray <-> vctrs_rray -------------------------------------------

# like vec_type2, vec_cast is ONLY about casting to a new type (class + shape)
# and has no regard for names

#' @method vec_cast.vctrs_rray vctrs_rray
#' @export
vec_cast.vctrs_rray.vctrs_rray <- function(x, to) {
  x <- rray_cast_inner(x, to)
  rray_shapecast(x, vec_dim(to))
}

# vec_cast vctrs_rray <-> double -----------------------------------------------

# double to vctrs_rray

#' @method vec_cast.vctrs_rray double
#' @export
vec_cast.vctrs_rray.double <- function(x, to) {
  dim <- vec_dim(x)
  x <- new_rray(vec_data(x), dim[1], dim[-1], dim_names(x))
  x <- rray_cast_inner(x, to)
  rray_shapecast(x, vec_dim(to))
}

# vctrs_rray to double

#' @method vec_cast.double vctrs_rray
#' @export
vec_cast.double.vctrs_rray <- function(x, to) {
  x <- rray_cast_inner(x, to)
  x <- vec_data(x)
  rray_shapecast(x, vec_dim(to))
}

# vec_cast vctrs_rray <-> integer -----------------------------------------------

#' @method vec_cast.vctrs_rray integer
#' @export
vec_cast.vctrs_rray.integer <- vec_cast.vctrs_rray.double

#' @method vec_cast.integer vctrs_rray
#' @export
vec_cast.integer.vctrs_rray <- vec_cast.double.vctrs_rray

# vec_cast vctrs_rray <-> logical -----------------------------------------------

#' @method vec_cast.vctrs_rray logical
#' @export
vec_cast.vctrs_rray.logical <- vec_cast.vctrs_rray.double

#' @method vec_cast.logical vctrs_rray
#' @export
vec_cast.logical.vctrs_rray <- vec_cast.double.vctrs_rray

# ------------------------------------------------------------------------------

rray_shapecast <- function(x, dim) {
  dim[1] <- vec_size(x)
  rray_broadcast(x, dim)
}
