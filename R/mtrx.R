#' Create a new mtrx
#'
#' Low level constructor for mtrx objects
#'
#' @param x A numeric or character vector with no attributes representing
#' the data in the mtrx.
#' @param dim An integer vector of length 2 describing the dimensions of the
#' `mtrx`.
#' @param col_names A character vector with length equal to `dim[2]` (the
#' number of columns of the mtrx).
#'
#' @examples
#'
#' mtrx_ex <- new_mtrx(1:10, dim = c(5L, 2L), col_names = c("a", "b"))
#'
#' mtrx_ex
#'
#' @export
new_mtrx <- function(x, dim, col_names) {

  stopifnot(is_vector(x))
  stopifnot(is_integer(dim) && length(dim) == 2L)
  stopifnot(is_character(col_names))
  stopifnot(vec_size(col_names) == dim[2])

  new_vctr(
    .data = x,
    dim = dim,
    col_names = col_names,
    class = "vctrs_mtrx"
  )
}


#' Build a mtrx object
#'
#' Helpful constructor for building mtrx objects. Pass in named vectors,
#' where each vector represents a single column in the mtrx. Length `1`
#' vectors are recycled.
#'
#' No rownames are allowed for mtrx objects.
#'
#' mtrx objects are never reduced to vectors when subsetting using `[`.
#'
#' @examples
#'
#' a_mtrx <- mtrx(a = 1:5, b = 6:10)
#'
#' # first column
#' a_mtrx[1]
#' a_mtrx["a"]
#' a_mtrx[,1]
#' a_mtrx[,"a"]
#'
#' # first row
#' a_mtrx[1,]
#'
#' # single element
#' a_mtrx[1, 1]
#'
#' # multiple columns
#' a_mtrx[, c("a", "b")]
#'
#' @export
mtrx <- function(...) {
  .dots <- dots_list(...)

  mtrx_nms <- tidy_names(names2(.dots))
  .dots <- unname(.dots)

  n_cols <- length(.dots)
  common_size <- vec_size_common(!!! .dots)

  mtrx_lst <- vec_recycle_common(!!! .dots, .size = common_size)
  mtrx_vec <- vec_c(!!! mtrx_lst)

  new_mtrx(mtrx_vec, dim = c(common_size, n_cols), mtrx_nms)
}
