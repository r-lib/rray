#' Create a new rray
#'
#' Low level constructor for rray objects
#'
#' @param .data A numeric vector with no attributes representing
#' the data.
#'
#' @param size An integer. The number of _observations_ in the object.
#' This is equivalent to the number of rows.
#'
#' @param shape An integer vector. The shape corresponds to all of the
#' dimensions in the object except for the first one (the `size`).
#'
#' @param dim_names A list. For no names, `NULL`, in which case a list of
#' empty characters will be constructed. Otherwise the list must
#' be the same length as the total number of dimensions
#' (i.e `length(c(size, shape))`). Each element of the list much be either
#' a character vector the same size as the corresponding dimension, or
#' `character(0)` for no names for that dimension.
#'
#' @param ... Name-value pairs defining attributes.
#'
#' @param subclass The name of the subclass.
#'
#' @examples
#'
#' rray_ex <- new_rray(
#'   .data = 1:10,
#'   size = 5L,
#'   shape = 2L,
#'   dim_names = list(character(), c("a", "b"))
#' )
#'
#' rray_ex
#'
#' @export
new_rray <- function(.data = numeric(0),
                     size = 0L,
                     # matches vctrs shape() idea
                     shape = integer(0),
                     dim_names = NULL,
                     ...,
                     subclass = character(0)
                     ) {

  stopifnot(is_bare_integer(size))
  stopifnot(is_bare_integer(shape))
  .dim <- c(size, shape)

  sub_type <- rray_sub_type(.data)

  # enforce list of empty characters
  if (is_null(dim_names)) {
    dim_names <- new_empty_dim_names(vec_size(.dim))
  }

  stopifnot(map_lgl(dim_names, is_character_or_null))

  # n shape dims and n elements of shape name list
  stopifnot(vec_size(.dim) == vec_size(dim_names))

  # dim & dim_names
  dim_name_lengths <- map_int(dim_names, vec_size)
  stopifnot(map2_lgl(.dim, dim_name_lengths, validate_equal_size_or_no_names))

  # new_rray() takes size and shape for compat with vctrs but we lie a bit
  # and actually only store the `dim`. We also store the `dimnames` not
  # `dim_names` because vctrs treats `dim` and `dimnames` special in
  # `vec_restore()`

  new_vctr(
    .data = .data,
    dim = .dim,
    dimnames = dim_names,
    ...,
    class = c(subclass, sub_type, "vctrs_rray")
  )

}

is_character_or_null <- function(x) {
  is_null(x) || is_character(x)
}

#' Build a rray object
#'
#' Constructor for building rray objects. Existing vectors, matrices, and
#' arrays can be used to build the rray, but their dimension names are not
#' retained.
#'
#' The `dim` argument is very flexible.
#'
#' - If `vec_size(x) == prod(dim)`, then a reshape is performed.
#' - Otherwise broadcasting is attempted.
#'
#' This allows quick construction of a wide variety of structures. See the
#' example section for more.
#'
#' rray objects are never reduced to vectors when subsetting using `[` (i.e.
#' dimensions are never dropped).
#'
#' @inheritParams new_rray
#'
#' @param x A numeric vector, matrix, or array to convert to an rray.
#'
#' @param dim An integer vector describing the dimensions of the rray. If `NULL`,
#' the dimensions are taken from the existing object using [vctrs::vec_dim()].
#'
#' @examples
#'
#' # 1D rray. Looks like a vector
#' # functions similar to a 1 column matrix
#' rray(c(1,2,3), dim = c(3))
#'
#' # 3 rows, 4 cols
#' rray(c(1,2,3), dim = c(3, 4))
#'
#'# 3x2x4 array
#' rray(1, dim = c(3, 2, 4))
#'
#' # from a matrix
#' mat <- matrix(c(1, 2, 3, 4), ncol = 2)
#' rray(mat)
#'
#' # from a matrix, with broadcasting
#' rray(mat, dim = c(2, 2, 3))
#'
#' # reshape that matrix during creation
#' # (different from broadcasting)
#' rray(mat, dim = c(1, 4))
#'
#' # from an array, with broadcasting
#' arr <- array(1, c(1, 2, 2))
#' rray(arr, c(3, 2, 2))
#'
#' # with row names
#' rray(c(1, 2, 3), c(3, 2), dim_names = list(c("x", "y", "z"), character()))
#'
#' @export
rray <- function(x = numeric(0), dim = NULL, dim_names = NULL) {

  if (is_null(dim)) {
    dim <- vec_dim(x)
  }

  dim <- vec_cast(dim, integer())

  if (!all(dim >= 0L)) {
    abort("`dim` must be a non-negative vector.")
  }

  x_dim <- vec_dim(x)

  # only broadcast if new dim is different than current dim
  # and you can't reshape to match
  if (!identical(x_dim, dim) && !is_reshapeable(x, dim)) {
    x <- rray_broadcast(x, dim)
  }

  new_rray(
    .data = vec_data(x),
    size = dim[1],
    shape = dim[-1],
    dim_names = dim_names
  )

}

validate_equal_size_or_no_names <- function(n_x, n_names) {
  n_names == 0L || identical(n_x, n_names)
}

is_rray_type <- function(x) {
  is_integer(x) || is_double(x) || is_logical(x)
}

rray_sub_type <- function(x) {
  if (is_integer(x)) {
    "vctrs_rray_int"
  }
  else if (is_double(x)) {
    "vctrs_rray_dbl"
  }
  else if (is_logical(x)) {
    "vctrs_rray_lgl"
  }
  else {
    glubort("Cannot create an rray from a {class(x)[1]}.")
  }
}

#' Check if `x` can be reshaped
#'
#' `x` is reshapeable to `to_dim` if the number of elements in `x` equal
#' the number of elements implied by `to_dim`, i.e. `prod(to_dim)`.
#'
#' @keywords internal
is_reshapeable <- function(x, to_dim) {
  n_x <- rray_elems(x)
  n_x == prod(to_dim)
}

#' Compute the number of elements in an array
#'
#' `rray_elems()` computes the number of individual elements in an array. It
#' generally computes the same thing as `length()`, but has a more predictable
#' name.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @examples
#' rray_elems(1:5)
#'
#' rray_elems(matrix(1, 2, 2))
#'
#' # It is different from `vec_size()`,
#' # which only returns the number of
#' # observations
#' library(vctrs)
#' vec_size(matrix(1, 2, 2))
#'
#' @export
rray_elems <- function(x) {
  prod(vec_dim(x))
}

