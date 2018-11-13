#' @export
format.vctrs_rray <- function(x, ...) {
  format(as_array(x))
}

#' @export
t.vctrs_rray <- function(x) {
  as_rray(t(as_array(x)))
}

#' @export
vec_restore.vctrs_rray <- function(x, to) {
  dim_names <- restore_dim_names(dim_names(to), vec_dim(x))
  new_rray(
    .data = vec_data(x),
    size = vec_size(x),
    shape = rray_shape(x),
    dim_names = dim_names
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

#' @export
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

#' @method vec_type2.vctrs_rray vctrs_rray
#' @export
vec_type2.vctrs_rray.vctrs_rray <- function(x, y) {
  shape <- rray_shape2(x, y)
  dim_names <- rray_dim_names2(x, y)
  # shape_names only
  dim_names[[1]] <- character(0)
  new_rray(shape = shape, dim_names = dim_names)
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

#' @method vec_cast.vctrs_rray vctrs_rray
#' @export
vec_cast.vctrs_rray.vctrs_rray <- function(x, to) {
  res <- rray_broadcast(x, vec_dim(to))
  new_rray(
    .data = vec_data(res),
    size = vec_size(res),
    shape = rray_shape(res),
    dim_names = dim_names(to)
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
  x <- rray_broadcast(x, vec_dim(to))
  x_dbl <- vec_cast(vec_data(x), double())
  array(x_dbl, dim = vec_dim(to), dimnames = dim_names(to))
}

# vec_cast vctrs_rray <-> integer -----------------------------------------------

#' @method vec_cast.vctrs_rray integer
#' @export
vec_cast.vctrs_rray.integer <- vec_cast.vctrs_rray.vctrs_rray

#' @method vec_cast.integer vctrs_rray
#' @export
vec_cast.integer.vctrs_rray <- function(x, to) {
  x <- rray_broadcast(x, vec_dim(to))
  x_dbl <- vec_cast(vec_data(x), integer())
  array(x_dbl, dim = vec_dim(to), dimnames = dim_names(to))
}

# vec_cast vctrs_rray <-> logical -----------------------------------------------

#' @method vec_cast.vctrs_rray logical
#' @export
vec_cast.vctrs_rray.logical <- vec_cast.vctrs_rray.vctrs_rray

#' @method vec_cast.logical vctrs_rray
#' @export
vec_cast.logical.vctrs_rray <- function(x, to) {
  x <- rray_broadcast(x, vec_dim(to))
  x_dbl <- vec_cast(vec_data(x), logical())
  array(x_dbl, dim = vec_dim(to), dimnames = dim_names(to))
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
    dim = vec_dim(x),
    row_names = row_names(to),
    col_names = col_names(to)
  )

}
