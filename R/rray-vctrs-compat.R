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
  restored_dim_names <- dim_names_restore(x, to)
  new_rray(
    .data = vec_data(x),
    size = vec_size(x),
    shape = rray_shape(x),
    dim_names = restored_dim_names
  )
}

# potentially the same dim_names
dim_names_restore <- function(x, to) {

  x_dim <- vec_dim(x)
  to_dim_names <- dim_names(to)

  restored_dim_names <- new_empty_dim_names(vec_size(x_dim))

  # cant use map2 bc to_dim_names could be
  # shorter than x_dim (i.e. we added a dimension)

  for(i in seq_along(to_dim_names)) {

    nms <- to_dim_names[[i]]
    single_dim <- x_dim[i]

    if (vec_size(nms) == single_dim) {
      restored_dim_names[[i]] <- nms
    }

  }

  restored_dim_names
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
  x <- as.double(x)
  dimnames(x) <- dim_names(to)
  x
}

# vec_cast vctrs_rray <-> integer -----------------------------------------------

#' @method vec_cast.vctrs_rray integer
#' @export
vec_cast.vctrs_rray.integer <- function(x, to) {
  res <- rray_broadcast(x, vec_dim(to))
  new_rray(.data = vec_data(res), dim = vec_dim(res))
}

#' @method vec_cast.integer vctrs_rray
#' @export
vec_cast.integer.vctrs_rray <- function(x, to) {
  # unlike vctrs, we can extend rows
  x <- rray_broadcast(x, vec_dim(to))
  x <- as.integer(x)
  dimnames(x) <- dim_names(to)
  x
}

# vec_cast vctrs_rray <-> vctrs_mtrx -------------------------------------------

#' @method vec_cast.vctrs_rray vctrs_mtrx
#' @export
vec_cast.vctrs_rray.vctrs_mtrx <- function(x, to) {
  res <- rray_broadcast(x, vec_dim(to))
  new_rray(.data = vec_data(res), dim = vec_dim(res), dim_names = dim_names(to))
}

#' @method vec_cast.vctrs_mtrx vctrs_rray
#' @export
vec_cast.vctrs_mtrx.vctrs_rray <- function(x, to) {

  x_dims <- vec_dims(x)

  if (x_dims > 2) {
    abort("Cannot convert a >2 dimensional rray into a mtrx.")
  }

  # unlike vctrs, we can extend rows
  x <- rray_broadcast(x, vec_dim(to))

  new_mtrx(
    .data = vec_data(x),
    dim = vec_dim(x),
    row_names = row_names(to),
    col_names = col_names(to)
  )
}
