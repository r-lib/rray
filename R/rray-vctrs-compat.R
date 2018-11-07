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
  as_rray(x) # nothing specific to `to`, compute new dims/names
}

#' @export
`dim<-.vctrs_rray` <- function(x, value) {
  validate_reshape(vec_dim(x), value)
  attr(x, "dim") <- value
  x
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
  non_row_dims <- rray_dim_common(x, y)[-1]
  new_rray(dim = c(0L, non_row_dims))
}

# vec_type2 vctrs_rray <-> double/matrix/array ---------------------------------

#' @method vec_type2.vctrs_rray double
#' @export
vec_type2.vctrs_rray.double <- vec_type2.vctrs_rray.vctrs_rray

#' @method vec_type2.double vctrs_rray
#' @export
vec_type2.double.vctrs_rray <- vec_type2.vctrs_rray.vctrs_rray

# vec_type2 vctrs_rray <-> integer/matrix/array ---------------------------------

#' @method vec_type2.vctrs_rray integer
#' @export
vec_type2.vctrs_rray.integer <- vec_type2.vctrs_rray.vctrs_rray

#' @method vec_type2.integer vctrs_rray
#' @export
vec_type2.integer.vctrs_rray <- vec_type2.vctrs_rray.vctrs_rray

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
  rray_broadcast(x, vec_dim(to))
}

# vec_cast vctrs_rray <-> double -----------------------------------------------

#' @method vec_cast.vctrs_rray double
#' @export
vec_cast.vctrs_rray.double <- function(x, to) {
  res <- rray_broadcast(x, vec_dim(to))
  new_rray(.data = vec_data(res), dim = vec_dim(res))
}

#' @method vec_cast.double vctrs_rray
#' @export
vec_cast.double.vctrs_rray <- function(x, to) {
  # unlike vctrs, we can extend rows
  x <- rray_broadcast(x, vec_dim(to))
  class(x) <- "double"
  vec_cast(x, to)
}

# vec_cast vctrs_rray <-> integer -----------------------------------------------

#' @method vec_cast.vctrs_rray integer
#' @export
vec_cast.vctrs_rray.integer <- function(x, to) {
  res <- rray_broadcast(x, vec_dim(to))
  new_rray(.data = vec_data(res), dim = vec_dim(res))
}

#' @method integer vctrs_rray
#' @export
vec_cast.integer.vctrs_rray <- function(x, to) {
  # unlike vctrs, we can extend rows
  x <- rray_broadcast(x, vec_dim(to))
  class(x) <- "integer"
  vec_cast(x, to)
}
