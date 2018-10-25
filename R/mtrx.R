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
#' mtrx objects are never reduced to vectors when subsetting using `[` (i.e.
#' dimensions are never dropped).
#'
#' @param ... Vector that represent the columns of the mtrx. These must be
#' the same length, or can be length `1` in which case the vector will be
#' recycled. Vectors are combined with `vec_c()` which performs an implicit
#' cast to enforce a common type. Vectors can optionally be named, which
#' results in a named matrix. Otherwise, default names are generated.
#'
#' @examples
#'
#' mtrx(1:10)
#'
#' a_mtrx <- mtrx(a = 1:5, b = 6:10)
#'
#' a_mtrx
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

  mtrx_nms <- tidy_names(names2(.dots), quiet = TRUE)
  .dots <- unname(.dots)

  n_cols <- length(.dots)
  common_size <- vec_size_common(!!! .dots)

  mtrx_lst <- vec_recycle_common(!!! .dots, .size = common_size)
  mtrx_vec <- vec_c(!!! mtrx_lst)

  new_mtrx(mtrx_vec, dim = c(common_size, n_cols), mtrx_nms)
}

#' Row-wise mtrx creation
#'
#' `mrtrx()` is the equivalent of [tibble::tribble()], but for mtrx objects.
#' It allows for easy row-wise creation of mtrx objects, which is especially
#' helpful for small mtrices where readability is key.
#'
#' @param ... Arguments specifying the structure of a mtrx. Column names should
#' be formulas, and may only appear before the data.
#'
#' @examples
#'
#' mrtrx(
#'   ~col1, ~col2,
#'   1,     3,
#'   5,     2
#' )
#'
#' @export
mrtrx <- function(...) {
  as_mtrx(tibble::frame_matrix(...))
}
