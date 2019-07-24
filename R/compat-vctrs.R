
# ##############################################################################
# vec_ptype2 boilerplate

# vec_ptype2 makes no attempt to recover dim names, as they are not part of the type.
# type = class + shape (at least for rray objects)

#' vctrs compatibility functions
#'
#' These functions are the extensions that allow rray objects to
#' work with vctrs.
#'
#' @param x,y Objects.
#' @param to Type to cast to.
#' @param op An arithmetic operator as a string.
#' @param ... Used to pass along error message information.
#' @inheritParams vec_ptype2
#'
#' @return
#'
#' See the corresponding vctrs function for the exact return value.
#'
#' @name vctrs-compat
#'
NULL

# ##############################################################################
# vec_ptype2 - vctrs_rray_dbl

#' @export
#' @rdname vctrs-compat
#' @method vec_ptype2 vctrs_rray_dbl
#' @export vec_ptype2.vctrs_rray_dbl
vec_ptype2.vctrs_rray_dbl <- function(x, y, ...) {
  UseMethod("vec_ptype2.vctrs_rray_dbl", y)
}

#' @method vec_ptype2.vctrs_rray_dbl default
#' @export
vec_ptype2.vctrs_rray_dbl.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.vctrs_rray_dbl vctrs_unspecified
#' @export
vec_ptype2.vctrs_rray_dbl.vctrs_unspecified <- function(x, y, ...) {
  vec_ptype(x)
}

# vec_ptype2 vctrs_rray_dbl <-> vctrs_rray_dbl ----------------------------------

#' @method vec_ptype2.vctrs_rray_dbl vctrs_rray_dbl
#' @export
vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl <- function(x, y, ...) {
  new_rray(double(), shape = rray_shape2(x, y))
}

# vec_ptype2 vctrs_rray_dbl <-> vctrs_rray_int ----------------------------------

#' @method vec_ptype2.vctrs_rray_dbl vctrs_rray_int
#' @export
vec_ptype2.vctrs_rray_dbl.vctrs_rray_int <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_ptype2 vctrs_rray_dbl <-> vctrs_rray_lgl ----------------------------------

#' @method vec_ptype2.vctrs_rray_dbl vctrs_rray_lgl
#' @export
vec_ptype2.vctrs_rray_dbl.vctrs_rray_lgl <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_ptype2 vctrs_rray_dbl <-> double/matrix/array -----------------------------

#' @method vec_ptype2.vctrs_rray_dbl double
#' @export
vec_ptype2.vctrs_rray_dbl.double <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

#' @method vec_ptype2.double vctrs_rray_dbl
#' @export
vec_ptype2.double.vctrs_rray_dbl <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_ptype2 vctrs_rray_dbl <-> integer/matrix/array ----------------------------

#' @method vec_ptype2.vctrs_rray_dbl integer
#' @export
vec_ptype2.vctrs_rray_dbl.integer <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

#' @method vec_ptype2.integer vctrs_rray_dbl
#' @export
vec_ptype2.integer.vctrs_rray_dbl <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_ptype2 vctrs_rray_dbl <-> logical/matrix/array ----------------------------

#' @method vec_ptype2.vctrs_rray_dbl logical
#' @export
vec_ptype2.vctrs_rray_dbl.logical <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

#' @method vec_ptype2.logical vctrs_rray_dbl
#' @export
vec_ptype2.logical.vctrs_rray_dbl <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

# ##############################################################################
# vec_ptype2 - vctrs_rray_int

#' @export
#' @rdname vctrs-compat
#' @method vec_ptype2 vctrs_rray_int
#' @export vec_ptype2.vctrs_rray_int
vec_ptype2.vctrs_rray_int <- function(x, y, ...) {
  UseMethod("vec_ptype2.vctrs_rray_int", y)
}

#' @method vec_ptype2.vctrs_rray_int default
#' @export
vec_ptype2.vctrs_rray_int.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.vctrs_rray_int vctrs_unspecified
#' @export
vec_ptype2.vctrs_rray_int.vctrs_unspecified <- function(x, y, ...) {
  vec_ptype(x)
}

# vec_ptype2 vctrs_rray_int <-> vctrs_rray_int ----------------------------------

#' @method vec_ptype2.vctrs_rray_int vctrs_rray_int
#' @export
vec_ptype2.vctrs_rray_int.vctrs_rray_int <- function(x, y, ...) {
  new_rray(integer(), shape = rray_shape2(x, y))
}

# vec_ptype2 vctrs_rray_int <-> vctrs_rray_dbl ----------------------------------

#' @method vec_ptype2.vctrs_rray_int vctrs_rray_dbl
#' @export
vec_ptype2.vctrs_rray_int.vctrs_rray_dbl <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_ptype2 vctrs_rray_int <-> vctrs_rray_lgl ----------------------------------

#' @method vec_ptype2.vctrs_rray_int vctrs_rray_lgl
#' @export
vec_ptype2.vctrs_rray_int.vctrs_rray_lgl <- vec_ptype2.vctrs_rray_int.vctrs_rray_int

# vec_ptype2 vctrs_rray_int <-> double/matrix/array -----------------------------

#' @method vec_ptype2.vctrs_rray_int double
#' @export
vec_ptype2.vctrs_rray_int.double <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

#' @method vec_ptype2.double vctrs_rray_int
#' @export
vec_ptype2.double.vctrs_rray_int <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_ptype2 vctrs_rray_int <-> integer/matrix/array ----------------------------

#' @method vec_ptype2.vctrs_rray_int integer
#' @export
vec_ptype2.vctrs_rray_int.integer <- vec_ptype2.vctrs_rray_int.vctrs_rray_int

#' @method vec_ptype2.integer vctrs_rray_int
#' @export
vec_ptype2.integer.vctrs_rray_int <- vec_ptype2.vctrs_rray_int.vctrs_rray_int

# vec_ptype2 vctrs_rray_int <-> logical/matrix/array ----------------------------

#' @method vec_ptype2.vctrs_rray_int logical
#' @export
vec_ptype2.vctrs_rray_int.logical <- vec_ptype2.vctrs_rray_int.vctrs_rray_int

#' @method vec_ptype2.logical vctrs_rray_int
#' @export
vec_ptype2.logical.vctrs_rray_int <- vec_ptype2.vctrs_rray_int.vctrs_rray_int

# ##############################################################################
# vec_ptype2 - vctrs_rray_lgl

#' @export
#' @rdname vctrs-compat
#' @method vec_ptype2 vctrs_rray_lgl
#' @export vec_ptype2.vctrs_rray_lgl
vec_ptype2.vctrs_rray_lgl <- function(x, y, ...) {
  UseMethod("vec_ptype2.vctrs_rray_lgl", y)
}

#' @method vec_ptype2.vctrs_rray_lgl default
#' @export
vec_ptype2.vctrs_rray_lgl.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  stop_incompatible_type(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.vctrs_rray_lgl vctrs_unspecified
#' @export
vec_ptype2.vctrs_rray_lgl.vctrs_unspecified <- function(x, y, ...) {
  vec_ptype(x)
}

# vec_ptype2 vctrs_rray_lgl <-> vctrs_rray_lgl ----------------------------------

#' @method vec_ptype2.vctrs_rray_lgl vctrs_rray_lgl
#' @export
vec_ptype2.vctrs_rray_lgl.vctrs_rray_lgl <- function(x, y, ...) {
  new_rray(logical(), shape = rray_shape2(x, y))
}

# vec_ptype2 vctrs_rray_lgl <-> vctrs_rray_int ----------------------------------

#' @method vec_ptype2.vctrs_rray_lgl vctrs_rray_int
#' @export
vec_ptype2.vctrs_rray_lgl.vctrs_rray_int <- vec_ptype2.vctrs_rray_int.vctrs_rray_int

# vec_ptype2 vctrs_rray_lgl <-> vctrs_rray_dbl ----------------------------------

#' @method vec_ptype2.vctrs_rray_lgl vctrs_rray_dbl
#' @export
vec_ptype2.vctrs_rray_lgl.vctrs_rray_dbl <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_ptype2 vctrs_rray_lgl <-> double/matrix/array -----------------------------

#' @method vec_ptype2.vctrs_rray_lgl double
#' @export
vec_ptype2.vctrs_rray_lgl.double <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

#' @method vec_ptype2.double vctrs_rray_lgl
#' @export
vec_ptype2.double.vctrs_rray_lgl <- vec_ptype2.vctrs_rray_dbl.vctrs_rray_dbl

# vec_ptype2 vctrs_rray_lgl <-> integer/matrix/array ----------------------------

#' @method vec_ptype2.vctrs_rray_lgl integer
#' @export
vec_ptype2.vctrs_rray_lgl.integer <- vec_ptype2.vctrs_rray_int.vctrs_rray_int

#' @method vec_ptype2.integer vctrs_rray_lgl
#' @export
vec_ptype2.integer.vctrs_rray_lgl <- vec_ptype2.vctrs_rray_int.vctrs_rray_int

# vec_ptype2 vctrs_rray_lgl <-> logical/matrix/array ----------------------------

#' @method vec_ptype2.vctrs_rray_lgl logical
#' @export
vec_ptype2.vctrs_rray_lgl.logical <- vec_ptype2.vctrs_rray_lgl.vctrs_rray_lgl

#' @method vec_ptype2.logical vctrs_rray_lgl
#' @export
vec_ptype2.logical.vctrs_rray_lgl <- vec_ptype2.vctrs_rray_lgl.vctrs_rray_lgl

# ##############################################################################
# vec_cast - vctrs_rray_int

#' @export
#' @rdname vctrs-compat
#' @method vec_cast vctrs_rray_int
#' @export vec_cast.vctrs_rray_int
vec_cast.vctrs_rray_int <- function(x, to, ...) UseMethod("vec_cast.vctrs_rray_int")

#' @method vec_cast.vctrs_rray_int default
#' @export
vec_cast.vctrs_rray_int.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

# vec_cast vctrs_rray_int <-> vctrs_rray_int -----------------------------------

#' @method vec_cast.vctrs_rray_int vctrs_rray_int
#' @export
vec_cast.vctrs_rray_int.vctrs_rray_int <- function(x, to, ...) {
  rray_shapecast(unname(x), rray_shape(to))
}

# vec_cast vctrs_rray_int <-> vctrs_rray_dbl -----------------------------------

#' @method vec_cast.vctrs_rray_int vctrs_rray_dbl
#' @export
vec_cast.vctrs_rray_int.vctrs_rray_dbl <- function(x, to, ...) {
  shape <- rray_shape(to)
  ptype <- rray_shapecast(integer(), shape)
  x <- vec_cast(vec_data(x), ptype)
  new_rray(x, vec_size(x), shape)
}

# vec_cast vctrs_rray_int <-> vctrs_rray_lgl -----------------------------------

#' @method vec_cast.vctrs_rray_int vctrs_rray_lgl
#' @export
vec_cast.vctrs_rray_int.vctrs_rray_lgl <- function(x, to, ...) {
  shape <- rray_shape(to)
  ptype <- rray_shapecast(integer(), shape)
  x <- vec_cast(vec_data(x), ptype)
  new_rray(x, vec_size(x), shape)
}

# vec_cast vctrs_rray_int <-> double -------------------------------------------

#' @method vec_cast.vctrs_rray_int double
#' @export
vec_cast.vctrs_rray_int.double <- function(x, to, ...) {
  shape <- rray_shape(to)
  ptype <- rray_shapecast(integer(), shape)
  x <- vec_cast(x, ptype)
  new_rray(x, vec_size(x), shape)
}

#' @method vec_cast.double vctrs_rray_int
#' @export
vec_cast.double.vctrs_rray_int <- function(x, to, ...) {
  ptype <- rray_shapecast(double(), rray_shape(to))
  vec_cast(vec_data(x), ptype)
}

# vec_cast vctrs_rray_int <-> integer ------------------------------------------

#' @method vec_cast.vctrs_rray_int integer
#' @export
vec_cast.vctrs_rray_int.integer <- function(x, to, ...) {
  shape <- rray_shape(to)
  x <- rray_shapecast(x, shape)
  new_rray(x, vec_size(x), shape)
}

#' @method vec_cast.integer vctrs_rray_int
#' @export
vec_cast.integer.vctrs_rray_int <- function(x, to, ...) {
  rray_shapecast(unname(unclass(x)), rray_shape(to))
}

# vec_cast vctrs_rray_int <-> logical ------------------------------------------

#' @method vec_cast.vctrs_rray_int logical
#' @export
vec_cast.vctrs_rray_int.logical <- vec_cast.vctrs_rray_int.double

#' @method vec_cast.logical vctrs_rray_int
#' @export
vec_cast.logical.vctrs_rray_int <- function(x, to, ...) {
  ptype <- rray_shapecast(logical(), rray_shape(to))
  vec_cast(vec_data(x), ptype)
}

# ##############################################################################
# vec_cast - vctrs_rray_dbl

#' @export
#' @rdname vctrs-compat
#' @method vec_cast vctrs_rray_dbl
#' @export vec_cast.vctrs_rray_dbl
vec_cast.vctrs_rray_dbl <- function(x, to, ...) UseMethod("vec_cast.vctrs_rray_dbl")

#' @method vec_cast.vctrs_rray_dbl default
#' @export
vec_cast.vctrs_rray_dbl.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

# vec_cast vctrs_rray_dbl <-> vctrs_rray_dbl -----------------------------------

#' @method vec_cast.vctrs_rray_dbl vctrs_rray_dbl
#' @export
vec_cast.vctrs_rray_dbl.vctrs_rray_dbl <- vec_cast.vctrs_rray_int.vctrs_rray_int

# vec_cast vctrs_rray_dbl <-> vctrs_rray_int -----------------------------------

#' @method vec_cast.vctrs_rray_dbl vctrs_rray_int
#' @export
vec_cast.vctrs_rray_dbl.vctrs_rray_int <- function(x, to, ...) {
  shape <- rray_shape(to)
  ptype <- rray_shapecast(double(), shape)
  x <- vec_cast(vec_data(x), ptype)
  new_rray(x, vec_size(x), shape)
}

# vec_cast vctrs_rray_dbl <-> vctrs_rray_lgl -----------------------------------

#' @method vec_cast.vctrs_rray_dbl vctrs_rray_lgl
#' @export
vec_cast.vctrs_rray_dbl.vctrs_rray_lgl <- vec_cast.vctrs_rray_dbl.vctrs_rray_int

# vec_cast vctrs_rray_dbl <-> double -------------------------------------------

#' @method vec_cast.vctrs_rray_dbl double
#' @export
vec_cast.vctrs_rray_dbl.double <- vec_cast.vctrs_rray_int.integer

#' @method vec_cast.double vctrs_rray_dbl
#' @export
vec_cast.double.vctrs_rray_dbl <- vec_cast.integer.vctrs_rray_int

# vec_cast vctrs_rray_dbl <-> integer ------------------------------------------

#' @method vec_cast.vctrs_rray_dbl integer
#' @export
vec_cast.vctrs_rray_dbl.integer <- function(x, to, ...) {
  shape <- rray_shape(to)
  ptype <- rray_shapecast(double(), shape)
  x <- vec_cast(x, ptype)
  new_rray(x, vec_size(x), shape)
}

#' @method vec_cast.integer vctrs_rray_dbl
#' @export
vec_cast.integer.vctrs_rray_dbl <- function(x, to, ...) {
  ptype <- rray_shapecast(integer(), rray_shape(to))
  vec_cast(vec_data(x), ptype)
}

# vec_cast vctrs_rray_dbl <-> logical ------------------------------------------

#' @method vec_cast.vctrs_rray_dbl logical
#' @export
vec_cast.vctrs_rray_dbl.logical <- vec_cast.vctrs_rray_dbl.integer

#' @method vec_cast.logical vctrs_rray_dbl
#' @export
vec_cast.logical.vctrs_rray_dbl <- function(x, to, ...) {
  ptype <- rray_shapecast(logical(), rray_shape(to))
  vec_cast(vec_data(x), ptype)
}

# ##############################################################################
# vec_cast - vctrs_rray_lgl

#' @export
#' @rdname vctrs-compat
#' @method vec_cast vctrs_rray_lgl
#' @export vec_cast.vctrs_rray_lgl
vec_cast.vctrs_rray_lgl <- function(x, to, ...) UseMethod("vec_cast.vctrs_rray_lgl")

#' @method vec_cast.vctrs_rray_lgl default
#' @export
vec_cast.vctrs_rray_lgl.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

# vec_cast vctrs_rray_lgl <-> vctrs_rray_lgl -----------------------------------

#' @method vec_cast.vctrs_rray_lgl vctrs_rray_lgl
#' @export
vec_cast.vctrs_rray_lgl.vctrs_rray_lgl <- vec_cast.vctrs_rray_int.vctrs_rray_int

# vec_cast vctrs_rray_lgl <-> vctrs_rray_dbl -----------------------------------

#' @method vec_cast.vctrs_rray_lgl vctrs_rray_dbl
#' @export
vec_cast.vctrs_rray_lgl.vctrs_rray_dbl <- function(x, to, ...) {
  shape <- rray_shape(to)
  ptype <- rray_shapecast(logical(), shape)
  x <- vec_cast(vec_data(x), ptype)
  new_rray(x, vec_size(x), shape)
}

# vec_cast vctrs_rray_lgl <-> vctrs_rray_int -----------------------------------

#' @method vec_cast.vctrs_rray_lgl vctrs_rray_int
#' @export
vec_cast.vctrs_rray_lgl.vctrs_rray_int <- vec_cast.vctrs_rray_lgl.vctrs_rray_dbl

# vec_cast vctrs_rray_lgl <-> double -------------------------------------------

#' @method vec_cast.vctrs_rray_lgl double
#' @export
vec_cast.vctrs_rray_lgl.double <- function(x, to, ...) {
  shape <- rray_shape(to)
  ptype <- rray_shapecast(logical(), shape)
  x <- vec_cast(x, ptype)
  new_rray(x, vec_size(x), shape)
}

#' @method vec_cast.double vctrs_rray_lgl
#' @export
vec_cast.double.vctrs_rray_lgl <- vec_cast.double.vctrs_rray_int

# vec_cast vctrs_rray_lgl <-> integer ------------------------------------------

#' @method vec_cast.vctrs_rray_lgl integer
#' @export
vec_cast.vctrs_rray_lgl.integer <- vec_cast.vctrs_rray_lgl.double

#' @method vec_cast.integer vctrs_rray_lgl
#' @export
vec_cast.integer.vctrs_rray_lgl <- vec_cast.integer.vctrs_rray_dbl

# vec_cast vctrs_rray_lgl <-> logical ------------------------------------------

#' @method vec_cast.vctrs_rray_lgl logical
#' @export
vec_cast.vctrs_rray_lgl.logical <- vec_cast.vctrs_rray_int.integer

#' @method vec_cast.logical vctrs_rray_lgl
#' @export
vec_cast.logical.vctrs_rray_lgl <- vec_cast.integer.vctrs_rray_int

# ------------------------------------------------------------------------------

rray_shapecast <- function(x, shape) {
  rray_broadcast(x, c(vec_size(x), shape))
}
