# vctrs doesn't quite do what I want here.
# see vctrs issue 136
# Personally, I think it is appropriate to do elementwise conversion
# but keep the dimensionality. That preserves the invariants mentioned there

new_array <- function(.data, dim, dimnames) {
  structure(.data, dim = dim, dimnames = dimnames)
}

# no need for a class? otherwise its not happy if you give it one
new_matrix <- function(.data, dim, dimnames) {
  structure(.data, dim = dim, dimnames = dimnames)
}

#' @export
as.double.vctrs_rray <- function(x, ...) {
  new_array(
    .data = vec_cast(vec_data(x), double()),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}

#' @export
as.integer.vctrs_rray <- function(x, ...) {
  new_array(
    .data = vec_cast(vec_data(x), integer()),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}

#' @export
as.logical.vctrs_rray <- function(x, ...) {
  new_array(
    .data = vec_cast(vec_data(x), logical()),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}

#' @export
as.double.vctrs_mtrx <- function(x, ...) {
  new_matrix(
    .data = vec_cast(vec_data(x), double()),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}

#' @export
as.integer.vctrs_mtrx <- function(x, ...) {
  new_matrix(
    .data = vec_cast(vec_data(x), integer()),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}

#' @export
as.logical.vctrs_mtrx <- function(x, ...) {
  new_matrix(
    .data = vec_cast(vec_data(x), logical()),
    dim = vec_dim(x),
    dimnames = dim_names(x)
  )
}
