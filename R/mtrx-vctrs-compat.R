#' @export
format.vctrs_mtrx <- function(x, ...) {
  format(as_matrix(x))
}

#' @export
t.vctrs_mtrx <- function(x) {
  as_mtrx(t(as_matrix(x)))
}

#' @export
vec_restore.vctrs_mtrx <- function(x, to) {
  new_mtrx(
    .data = vec_data(x),
    size = vec_size(x),
    shape = rray_shape(x),
    dim_names = dim_names(x)
  )
}

#' @export
vec_ptype_abbr.vctrs_mtrx <- function(x) {
  "mtrx"
}

#' @export
vec_ptype_full.vctrs_mtrx <- function(x) {
  paste0("vctrs_mtrx<", typeof(x), ">", vec_ptype_shape(x))
}


# vec_type2 boilerplate --------------------------------------------------------

#' @export
#' @method vec_type2 vctrs_mtrx
#' @export vec_type2.vctrs_mtrx
vec_type2.vctrs_mtrx <- function(x, y) UseMethod("vec_type2.vctrs_mtrx")

#' @method vec_type2.vctrs_mtrx default
#' @export
vec_type2.vctrs_mtrx.default <- function(x, y) stop_incompatible_type(x, y)

#' @method vec_type2.vctrs_mtrx vctrs_unspecified
#' @export
vec_type2.vctrs_mtrx.vctrs_unspecified <- function(x, y) x

# vec_type2 vctrs_mtrx <-> vctrs_mtrx ------------------------------------------

#' @method vec_type2.vctrs_mtrx vctrs_mtrx
#' @export
vec_type2.vctrs_mtrx.vctrs_mtrx <- function(x, y) {
  new_mtrx(shape = rray_shape2(x, y))
}

# vec_type2 vctrs_rray <-> double/matrix/array ---------------------------------

#' @method vec_type2.vctrs_rray double
#' @export
vec_type2.vctrs_rray.double <- vec_type2.vctrs_mtrx.vctrs_mtrx

#' @method vec_type2.double vctrs_mtrx
#' @export
vec_type2.double.vctrs_mtrx <- vec_type2.vctrs_mtrx.vctrs_mtrx

# vec_type2 vctrs_mtrx <-> integer/matrix/array --------------------------------

#' @method vec_type2.vctrs_rray integer
#' @export
vec_type2.vctrs_rray.integer <- vec_type2.vctrs_mtrx.vctrs_mtrx

#' @method vec_type2.integer vctrs_mtrx
#' @export
vec_type2.integer.vctrs_mtrx <- vec_type2.vctrs_mtrx.vctrs_mtrx

# vec_cast vctrs_rray <-> vctrs_mtrx -------------------------------------------

# implemented in rray-vctrs-compat.R

# vec_cast boilerplate ---------------------------------------------------------

#' @export
#' @method vec_cast vctrs_mtrx
#' @export vec_cast.vctrs_mtrx
vec_cast.vctrs_mtrx <- function(x, to) UseMethod("vec_cast.vctrs_mtrx")

#' @method vec_cast.vctrs_mtrx default
#' @export
vec_cast.vctrs_mtrx.default <- function(x, to) stop_incompatible_cast(x, to)

#' @method vec_cast.vctrs_mtrx logical
#' @export
vec_cast.vctrs_mtrx.logical <- function(x, to) vec_unspecified_cast(x, to)

# vec_cast vctrs_mtrx <-> vctrs_mtrx -------------------------------------------

#' @method vec_cast.vctrs_mtrx vctrs_mtrx
#' @export
vec_cast.vctrs_mtrx.vctrs_mtrx <- function(x, to) {
  res <- rray_broadcast(x, vec_dim(to))
  new_mtrx(
    .data = vec_data(res),
    size = vec_size(res),
    shape = rray_shape(res)
  )
}

# vec_cast vctrs_mtrx <-> double -----------------------------------------------

#' @method vec_cast.vctrs_mtrx double
#' @export
vec_cast.vctrs_mtrx.double <- vec_cast.vctrs_mtrx.vctrs_mtrx

#' @method vec_cast.double vctrs_mtrx
#' @export
vec_cast.double.vctrs_mtrx <- function(x, to) {
  vec_cast.double.vctrs_rray(x, to)
}

# vec_cast vctrs_mtrx <-> integer -----------------------------------------------

#' @method vec_cast.vctrs_rray integer
#' @export
vec_cast.vctrs_mtrx.integer <- vec_cast.vctrs_mtrx.vctrs_mtrx

#' @method vec_cast.integer vctrs_mtrx
#' @export
vec_cast.integer.vctrs_mtrx <- function(x, to) {
  vec_cast.integer.vctrs_rray(x, to)
}

# vec_cast vctrs_mtrx <-> logical -----------------------------------------------

#' @method vec_cast.vctrs_rray logical
#' @export
vec_cast.vctrs_mtrx.logical <- vec_cast.vctrs_mtrx.vctrs_mtrx

#' @method vec_cast.logical vctrs_mtrx
#' @export
vec_cast.logical.vctrs_mtrx <- function(x, to) {
  vec_cast.logical.vctrs_rray(x, to)
}

# vec_cast vctrs_rray <-> vctrs_mtrx -------------------------------------------

# implemented in rray-vctrs-compat.R
