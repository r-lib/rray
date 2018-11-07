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
  as_mtrx(x)
}

#' @export
vec_ptype_abbr.vctrs_mtrx <- function(x) {
  "mtrx"
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
  col_dim <- rray_dim_common(x, y)[-1]
  new_mtrx(dim = c(0L, col_dim))
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
    dim = vec_dim(res),
    row_names = row_names(to),
    col_names = col_names(to)
  )
}

# vec_cast vctrs_mtrx <-> double -----------------------------------------------

#' @method vec_cast.vctrs_mtrx double
#' @export
vec_cast.vctrs_mtrx.double <- function(x, to) {
  res <- rray_broadcast(x, vec_dim(to))
  new_mtrx(
    .data = vec_data(res),
    dim = vec_dim(res),
    row_names = row_names(to),
    col_names = col_names(to)
  )
}

#' @method vec_cast.double vctrs_mtrx
#' @export
vec_cast.double.vctrs_mtrx <- function(x, to) {
  # unlike vctrs, we can extend rows
  x <- rray_broadcast(x, vec_dim(to))
  x <- as.double(x)
  dimnames(x) <- dim_names(to)
  x
}

# vec_cast vctrs_mtrx <-> integer -----------------------------------------------

#' @method vec_cast.vctrs_rray integer
#' @export
vec_cast.vctrs_mtrx.integer <- function(x, to) {
  res <- rray_broadcast(x, vec_dim(to))
  new_mtrx(.data = vec_data(res), dim = vec_dim(res))
}

#' @method vec_cast.integer vctrs_mtrx
#' @export
vec_cast.integer.vctrs_mtrx <- function(x, to) {
  # unlike vctrs, we can extend rows
  x <- rray_broadcast(x, vec_dim(to))
  x <- as.integer(x)
  dimnames(x) <- dim_names(to)
  x
}

# vec_cast vctrs_rray <-> vctrs_mtrx -------------------------------------------

# implemented in rray-vctrs-compat.R
