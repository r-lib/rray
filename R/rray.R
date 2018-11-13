#' Create a new rray
#'
#' Low level constructor for rray objects
#'
#' @param .data A numeric vector with no attributes representing
#' the data.
#' @param dim An integer vector describing the dimensions of the
#' `rray`.
#' @param dim_names A list. For no names, `NULL`. Otherwise the list must
#' be the same length as `dim`. Each element of the list much be either
#' a character vector the same length as the corresponding dimension in
#' `dim`, or `character(0)` for no names for that dimension.
#' @param ... Name-value pairs defining attributes.
#' @param subclass The name of the subclass.
#'
#' @examples
#'
#' rray_ex <- new_rray(1:10, dim = c(5L, 2L), col_names = c("a", "b"))
#'
#' rray_ex
#'
#' @export
new_rray <- function(.data = numeric(0),
                     # no concept of scalar/vector. At least 2D.
                     size = integer(1),
                     shape = integer(1),
                     dim_names = NULL,
                     ...,
                     subclass = character(0)
                     ) {

  stopifnot(is_rray_type(.data))
  stopifnot(vec_dims(.data) == 1L)
  stopifnot(is_bare_integer(size))
  stopifnot(is_bare_integer(shape))
  .dim <- c(size, shape)

  # enforce list of empty characters
  if (is_null(dim_names)) {
    dim_names <- new_empty_dim_names(vec_size(.dim))
  }

  stopifnot(map_lgl(dim_names, is_character))

  # n shape dims and n elements of shape name list
  stopifnot(vec_size(.dim) == vec_size(dim_names))

  # dim & dim_names
  dim_name_lengths <- map_int(dim_names, vec_size)
  stopifnot(map2_lgl(.dim, dim_name_lengths, are_equal_or_no_name))

  # new_rray() takes size and shape for compat with vctrs but we lie a bit
  # and actually only store the dim because of the nice benefits that gets us

  new_vctr(
    .data = .data,
    dim = .dim,
    dim_names = dim_names,
    ...,
    class = c(subclass, "vctrs_rray")
  )

}

#' Build a rray object
#'
#' Constructor for building rray objects. Existing vectors, matrices, and
#' arrays can be used to build the rray, and they can be broadcast up to
#' a different dimension using `dim`.
#'
#' rray objects are never reduced to vectors when subsetting using `[` (i.e.
#' dimensions are never dropped).
#'
#' @inheritParams new_rray
#'
#' @param x A numeric vector, matrix, or array to convert to an rray. It
#' is brodcast based on `dim` to other dimensions if requested.
#'
#' @param dim An integer vector describing the dimensions of the rray. If `NULL`,
#' the dimensions are taken from the existing object using [vctrs::vec_dim()].
#'
#' @examples
#'
#' # 3 rows
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
    dim <- rray_dim_at_least_2D(x)
  }

  dim <- vec_cast(dim, integer())

  if (!all(dim >= 0L)) {
    abort("`dim` must be a non-negative vector.")
  }

  x_dim <- vec_dim(x)

  if (!identical(x_dim, dim)) {
    x <- rray_broadcast(x, dim)
  }

  new_rray(
    .data = vec_data(x),
    size = dim[1],
    shape = dim[-1],
    dim_names = dim_names
  )

}


is_character_or_null <- function(x) {
  is_character(x) || is_null(x)
}

are_equal_or_no_name <- function(n_x, n_names) {
  n_names == 0L || identical(n_x, n_names)
}

is_rray_type <- function(x) {
  is_integer(x) || is_double(x) || is_logical(x)
}

rray_dim_at_least_2D <- function(x) {

  size <- vec_size(x)

  if (vec_dims(x) == 1) { # vector

    if (size == 0) { # but its empty
      cols <- 0
    } else {
      cols <- 1
    }

    dim(x) <- c(size, cols)
  }

  dim <- vec_dim(x)
  dim
}
