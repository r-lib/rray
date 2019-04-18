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

# ##############################################################################
# vec_type2 boilerplate

# vec_type2 makes no attempt to recover dim names, as they are not part of the type.
# type = class + shape (at least for rray objects)

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

# ##############################################################################
# vec_type2 - vctrs_rray_int

#' @export
#' @rdname vctrs-compat
#' @method vec_type2 vctrs_rray_int
#' @export vec_type2.vctrs_rray_int
vec_type2.vctrs_rray_int <- function(x, y) UseMethod("vec_type2.vctrs_rray_int", y)

#' @method vec_type2.vctrs_rray_int default
#' @export
vec_type2.vctrs_rray_int.default <- function(x, y) stop_incompatible_type(x, y)

#' @method vec_type2.vctrs_rray_int vctrs_unspecified
#' @export
vec_type2.vctrs_rray_int.vctrs_unspecified <- function(x, y) x

# vec_type2 vctrs_rray_int <-> vctrs_rray_int ----------------------------------

#' @method vec_type2.vctrs_rray_int vctrs_rray_int
#' @export
vec_type2.vctrs_rray_int.vctrs_rray_int <- function(x, y) {
  new_rray(integer(), shape = rray_shape2(x, y))
}

# vec_type2 vctrs_rray_int <-> vctrs_rray_dbl ----------------------------------

#' @method vec_type2.vctrs_rray_int vctrs_rray_dbl
#' @export
vec_type2.vctrs_rray_int.vctrs_rray_dbl <- function(x, y) {
  new_rray(double(), shape = rray_shape2(x, y))
}

#' @method vec_type2.vctrs_rray_dbl vctrs_rray_int
#' @export
vec_type2.vctrs_rray_dbl.vctrs_rray_int <- vec_type2.vctrs_rray_int.vctrs_rray_dbl

# vec_type2 vctrs_rray_int <-> vctrs_rray_lgl ----------------------------------

#' @method vec_type2.vctrs_rray_int vctrs_rray_lgl
#' @export
vec_type2.vctrs_rray_int.vctrs_rray_lgl <- vec_type2.vctrs_rray_int.vctrs_rray_int

#' @method vec_type2.vctrs_rray_lgl vctrs_rray_int
#' @export
vec_type2.vctrs_rray_lgl.vctrs_rray_int <- vec_type2.vctrs_rray_int.vctrs_rray_int

# vec_type2 vctrs_rray_int <-> double/matrix/array -----------------------------

#' @method vec_type2.vctrs_rray_int double
#' @export
vec_type2.vctrs_rray_int.double <- vec_type2.vctrs_rray_int.vctrs_rray_dbl

#' @method vec_type2.double vctrs_rray_int
#' @export
vec_type2.double.vctrs_rray_int <- vec_type2.vctrs_rray_int.vctrs_rray_dbl

# vec_type2 vctrs_rray_int <-> integer/matrix/array ----------------------------

#' @method vec_type2.vctrs_rray_int integer
#' @export
vec_type2.vctrs_rray_int.integer <- vec_type2.vctrs_rray_int.vctrs_rray_int

#' @method vec_type2.integer vctrs_rray_int
#' @export
vec_type2.integer.vctrs_rray_int <- vec_type2.vctrs_rray_int.vctrs_rray_int

# vec_type2 vctrs_rray_int <-> logical/matrix/array ----------------------------

#' @method vec_type2.vctrs_rray_int logical
#' @export
vec_type2.vctrs_rray_int.logical <- vec_type2.vctrs_rray_int.vctrs_rray_int

#' @method vec_type2.logical vctrs_rray_int
#' @export
vec_type2.logical.vctrs_rray_int <- vec_type2.vctrs_rray_int.vctrs_rray_int

# ##############################################################################
# vec_type2 - vctrs_rray_dbl

#' @export
#' @rdname vctrs-compat
#' @method vec_type2 vctrs_rray_dbl
#' @export vec_type2.vctrs_rray_dbl
vec_type2.vctrs_rray_dbl <- function(x, y) UseMethod("vec_type2.vctrs_rray_dbl", y)

#' @method vec_type2.vctrs_rray_dbl default
#' @export
vec_type2.vctrs_rray_dbl.default <- function(x, y) stop_incompatible_type(x, y)

#' @method vec_type2.vctrs_rray_dbl vctrs_unspecified
#' @export
vec_type2.vctrs_rray_dbl.vctrs_unspecified <- function(x, y) x

# vec_type2 vctrs_rray_dbl <-> vctrs_rray_dbl ----------------------------------

#' @method vec_type2.vctrs_rray_dbl vctrs_rray_dbl
#' @export
vec_type2.vctrs_rray_dbl.vctrs_rray_dbl <- function(x, y) {
  new_rray(double(), shape = rray_shape2(x, y))
}

# vec_type2 vctrs_rray_dbl <-> vctrs_rray_int ----------------------------------

#' @method vec_type2.vctrs_rray_dbl vctrs_rray_int
#' @export
vec_type2.vctrs_rray_dbl.vctrs_rray_int <- vec_type2.vctrs_rray_dbl.vctrs_rray_dbl

#' @method vec_type2.vctrs_rray_int vctrs_rray_dbl
#' @export
vec_type2.vctrs_rray_int.vctrs_rray_dbl <- vec_type2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_type2 vctrs_rray_dbl <-> vctrs_rray_lgl ----------------------------------

#' @method vec_type2.vctrs_rray_dbl vctrs_rray_lgl
#' @export
vec_type2.vctrs_rray_dbl.vctrs_rray_lgl <- vec_type2.vctrs_rray_dbl.vctrs_rray_dbl

#' @method vec_type2.vctrs_rray_lgl vctrs_rray_dbl
#' @export
vec_type2.vctrs_rray_lgl.vctrs_rray_dbl <- vec_type2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_type2 vctrs_rray_dbl <-> double/matrix/array -----------------------------

#' @method vec_type2.vctrs_rray_dbl double
#' @export
vec_type2.vctrs_rray_dbl.double <- vec_type2.vctrs_rray_dbl.vctrs_rray_dbl

#' @method vec_type2.double vctrs_rray_dbl
#' @export
vec_type2.double.vctrs_rray_dbl <- vec_type2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_type2 vctrs_rray_dbl <-> integer/matrix/array ----------------------------

#' @method vec_type2.vctrs_rray_dbl integer
#' @export
vec_type2.vctrs_rray_dbl.integer <- vec_type2.vctrs_rray_dbl.vctrs_rray_dbl

#' @method vec_type2.integer vctrs_rray_dbl
#' @export
vec_type2.integer.vctrs_rray_dbl <- vec_type2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_type2 vctrs_rray_dbl <-> logical/matrix/array ----------------------------

#' @method vec_type2.vctrs_rray_dbl logical
#' @export
vec_type2.vctrs_rray_dbl.logical <- vec_type2.vctrs_rray_dbl.vctrs_rray_dbl

#' @method vec_type2.logical vctrs_rray_dbl
#' @export
vec_type2.logical.vctrs_rray_dbl <- vec_type2.vctrs_rray_dbl.vctrs_rray_dbl

# ##############################################################################
# vec_type2 - vctrs_rray_lgl

#' @export
#' @rdname vctrs-compat
#' @method vec_type2 vctrs_rray_lgl
#' @export vec_type2.vctrs_rray_lgl
vec_type2.vctrs_rray_lgl <- function(x, y) UseMethod("vec_type2.vctrs_rray_lgl", y)

#' @method vec_type2.vctrs_rray_lgl default
#' @export
vec_type2.vctrs_rray_lgl.default <- function(x, y) stop_incompatible_type(x, y)

#' @method vec_type2.vctrs_rray_lgl vctrs_unspecified
#' @export
vec_type2.vctrs_rray_lgl.vctrs_unspecified <- function(x, y) x

# vec_type2 vctrs_rray_lgl <-> vctrs_rray_lgl ----------------------------------

#' @method vec_type2.vctrs_rray_lgl vctrs_rray_lgl
#' @export
vec_type2.vctrs_rray_lgl.vctrs_rray_lgl <- function(x, y) {
  new_rray(logical(), shape = rray_shape2(x, y))
}

# vec_type2 vctrs_rray_lgl <-> vctrs_rray_int ----------------------------------

#' @method vec_type2.vctrs_rray_lgl vctrs_rray_int
#' @export
vec_type2.vctrs_rray_lgl.vctrs_rray_int <- function(x, y) {
  new_rray(integer(), shape = rray_shape2(x, y))
}

#' @method vec_type2.vctrs_rray_int vctrs_rray_lgl
#' @export
vec_type2.vctrs_rray_int.vctrs_rray_lgl <- vec_type2.vctrs_rray_lgl.vctrs_rray_int

# vec_type2 vctrs_rray_lgl <-> vctrs_rray_dbl ----------------------------------

#' @method vec_type2.vctrs_rray_lgl vctrs_rray_dbl
#' @export
vec_type2.vctrs_rray_lgl.vctrs_rray_dbl <- function(x, y) {
  new_rray(double(), shape = rray_shape2(x, y))
}

#' @method vec_type2.vctrs_rray_dbl vctrs_rray_lgl
#' @export
vec_type2.vctrs_rray_dbl.vctrs_rray_lgl <- vec_type2.vctrs_rray_lgl.vctrs_rray_dbl

# vec_type2 vctrs_rray_lgl <-> double/matrix/array -----------------------------

#' @method vec_type2.vctrs_rray_lgl double
#' @export
vec_type2.vctrs_rray_lgl.double <- vec_type2.vctrs_rray_lgl.vctrs_rray_dbl

#' @method vec_type2.double vctrs_rray_lgl
#' @export
vec_type2.double.vctrs_rray_lgl <- vec_type2.vctrs_rray_lgl.vctrs_rray_dbl

# vec_type2 vctrs_rray_lgl <-> integer/matrix/array ----------------------------

#' @method vec_type2.vctrs_rray_lgl integer
#' @export
vec_type2.vctrs_rray_lgl.integer <- vec_type2.vctrs_rray_lgl.vctrs_rray_int

#' @method vec_type2.integer vctrs_rray_lgl
#' @export
vec_type2.integer.vctrs_rray_lgl <- vec_type2.vctrs_rray_lgl.vctrs_rray_int

# vec_type2 vctrs_rray_lgl <-> logical/matrix/array ----------------------------

#' @method vec_type2.vctrs_rray_lgl logical
#' @export
vec_type2.vctrs_rray_lgl.logical <- vec_type2.vctrs_rray_lgl.vctrs_rray_lgl

#' @method vec_type2.logical vctrs_rray_lgl
#' @export
vec_type2.logical.vctrs_rray_lgl <- vec_type2.vctrs_rray_lgl.vctrs_rray_lgl

# ##############################################################################
# vec_cast boilerplate

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
