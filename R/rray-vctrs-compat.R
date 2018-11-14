#' @export
format.vctrs_rray <- function(x, ...) {
  format(as_array(x))
}

#' @export
t.vctrs_rray <- function(x) {
  as_rray(t(as_array(x)))
}

# vec_restore restores ONLY the type
# no attempt to "restore" dim names.
# only copies any existing dim_names over

#' @export
vec_restore.vctrs_rray <- function(x, to) {
  new_rray(
    .data = vec_data(x),
    size = vec_size(x),
    shape = rray_shape(x),
    dim_names = dim_names(x)
  )
}

#' @export
`dim<-.vctrs_rray` <- function(x, value) {
  rray_broadcast(x, value)
}

#' @export
vec_ptype_abbr.vctrs_rray <- function(x) {
  "rray"
}

#' @export
vec_ptype_full.vctrs_rray <- function(x) {
  paste0("vctrs_rray<", typeof(x), ">", vec_ptype_shape(x))
}

# from vctrs
vec_ptype_shape <- function(x) {
  dim <- vec_dim(x)
  if (length(dim) == 1) {
    ""
  } else {
    paste0("[,", paste(dim[-1], collapse = ","), "]")
  }
}


# Override the vctrs `[<-` because it does not allow you to pass in more than
# just i AND it calls vec_cast() where `to` is the full x obj, not just on the slice
# you are casting to
# If vctrs did: function(x, ..., value) and vec_cast(value, x[...]) then
# all would be good
`[<-.vctrs_rray` <- function(x, ..., value) {
  value <- vec_cast(value, x[...])
  x_array <- as_array(x)
  x_array[...] <- value
  vec_restore(x_array, x)
}

# vec_type2 boilerplate --------------------------------------------------------

#' vctrs compatibility functions
#'
#' These functions are the extensions that allow mtrx and rray objects to
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
vec_type2.vctrs_rray <- function(x, y) UseMethod("vec_type2.vctrs_rray")

#' @method vec_type2.vctrs_rray default
#' @export
vec_type2.vctrs_rray.default <- function(x, y) stop_incompatible_type(x, y)

#' @method vec_type2.vctrs_rray vctrs_unspecified
#' @export
vec_type2.vctrs_rray.vctrs_unspecified <- function(x, y) x

# vec_type2 vctrs_rray <-> vctrs_rray ------------------------------------------

# vec_type2 makes no attempt to recover dim names, as they are not part of the type.
# type = class + shape (at least for rray and mtrx objects)

#' @method vec_type2.vctrs_rray vctrs_rray
#' @export
vec_type2.vctrs_rray.vctrs_rray <- function(x, y) {
  new_rray(shape = rray_shape2(x, y))
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

# vec_type2 vctrs_rray <-> vctrs_mtrx ------------------------------------------

#' @method vec_type2.vctrs_rray vctrs_mtrx
#' @export
vec_type2.vctrs_rray.vctrs_mtrx <- vec_type2.vctrs_rray.vctrs_rray

#' @method vec_type2.vctrs_mtrx vctrs_rray
#' @export
vec_type2.vctrs_mtrx.vctrs_rray <- vec_type2.vctrs_rray.vctrs_rray

# vec_cast boilerplate ---------------------------------------------------------

#' @export
#' @rdname vctrs-compat
#' @method vec_cast vctrs_rray
#' @export vec_cast.vctrs_rray
vec_cast.vctrs_rray <- function(x, to) UseMethod("vec_cast.vctrs_rray")

#' @method vec_cast.vctrs_rray default
#' @export
vec_cast.vctrs_rray.default <- function(x, to) stop_incompatible_cast(x, to)

#' @method vec_cast.vctrs_rray logical
#' @export
vec_cast.vctrs_rray.logical <- function(x, to) vec_unspecified_cast(x, to)

# vec_cast vctrs_rray <-> vctrs_rray -------------------------------------------

# like vec_type2, vec_cast is ONLY about casting to a new type (class + shape)
# and has no regard for names

#' @method vec_cast.vctrs_rray vctrs_rray
#' @export
vec_cast.vctrs_rray.vctrs_rray <- function(x, to) {
  res <- broadcast(x, vec_dim(to))
  new_rray(
    .data = vec_data(res),
    size = vec_size(res),
    shape = rray_shape(res)
  )
}

# vec_cast vctrs_rray <-> double -----------------------------------------------

# double to vctrs_rray

#' @method vec_cast.vctrs_rray double
#' @export
vec_cast.vctrs_rray.double <- vec_cast.vctrs_rray.vctrs_rray

# vctrs_rray to double

#' @method vec_cast.double vctrs_rray
#' @export
vec_cast.double.vctrs_rray <- function(x, to) {
  dim <- vec_dim(to)
  x <- broadcast(x, dim)
  x <- vec_cast(vec_data(x), double())
  array(x, dim = dim)
}

# vec_cast vctrs_rray <-> integer -----------------------------------------------

#' @method vec_cast.vctrs_rray integer
#' @export
vec_cast.vctrs_rray.integer <- vec_cast.vctrs_rray.vctrs_rray

#' @method vec_cast.integer vctrs_rray
#' @export
vec_cast.integer.vctrs_rray <- function(x, to) {
  dim <- vec_dim(to)
  x <- broadcast(x, dim)
  x <- vec_cast(vec_data(x), integer())
  array(x, dim = dim)
}

# vec_cast vctrs_rray <-> logical -----------------------------------------------

#' @method vec_cast.vctrs_rray logical
#' @export
vec_cast.vctrs_rray.logical <- vec_cast.vctrs_rray.vctrs_rray

#' @method vec_cast.logical vctrs_rray
#' @export
vec_cast.logical.vctrs_rray <- function(x, to) {
  dim <- vec_dim(to)
  x <- broadcast(x, dim)
  x <- vec_cast(vec_data(x), logical())
  array(x, dim = dim)
}

# vec_cast vctrs_rray <-> vctrs_mtrx -------------------------------------------

# mtrx to rray

#' @method vec_cast.vctrs_rray vctrs_mtrx
#' @export
vec_cast.vctrs_rray.vctrs_mtrx <- vec_cast.vctrs_rray.vctrs_rray

# rray to mtrx, need to be careful here

#' @method vec_cast.vctrs_mtrx vctrs_rray
#' @export
vec_cast.vctrs_mtrx.vctrs_rray <- function(x, to) {

  x_dims <- vec_dims(x)

  if (x_dims > 2) {
    abort("Cannot convert a >2 dimensional rray into a mtrx.")
  }

  x <- rray_broadcast(x, vec_dim(to))

  new_mtrx(
    .data = vec_data(x),
    size = vec_size(x),
    shape = rray_shape(x)
  )

}
