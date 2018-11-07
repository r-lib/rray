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
new_rray <- function(.data = numeric(),
                     dim = integer(1),
                     dim_names = NULL,
                     ...,
                     subclass = character()
                     ) {

  stopifnot(is_vector(.data))
  stopifnot(is_integer(dim))

  if (is_null(dim_names)) {
    dim_names <- new_empty_dim_names(vec_size(dim))
  }

  # no support for rray dim_names to have names
  stopifnot(is_bare_list(dim_names))

  stopifnot(map_lgl(dim_names, is_character))

  stopifnot(length(dim) == length(dim_names))

  dim_name_lengths <- map_int(dim_names, length)
  stopifnot(map2_lgl(dim, dim_name_lengths, are_equal_or_no_name))

  new_vctr(
    .data = .data,
    dim = dim,
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
rray <- function(x = numeric(), dim = NULL, dim_names = NULL) {

  if (is.null(dim)) {
    dim <- vec_dim(x)
  }

  dim <- vec_cast(dim, integer())
  validate_dim(dim)

  x <- vec_data(rray_broadcast(x, dim))

  new_rray(.data = x, dim = dim, dim_names = dim_names)
}


is_character_or_null <- function(x) {
  is_character(x) || is_null(x)
}

are_equal_or_no_name <- function(x, y) {
  y == 0L || identical(x, y)
}
