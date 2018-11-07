#' Create a new mtrx
#'
#' Low level constructor for mtrx objects
#'
#' @inheritParams new_rray
#' @param dim A size 2 integer vector specifying the number of rows and columns
#' of the mtrx.
#' @param row_names A character vector of row names. The default is to have
#' no row names.
#' @param col_names A character vector of column names. The default is to
#' have no column names.
#'
#' @examples
#'
#' mtrx_ex <- new_mtrx(1:10, dim = c(5L, 2L), col_names = c("a", "b"))
#'
#' mtrx_ex
#'
#' @export
new_mtrx <- function(.data = numeric(),
                     dim = integer(2),
                     row_names = character(),
                     col_names = character(),
                     ...,
                     subclass = character()) {

  stopifnot(is_vector(.data))
  stopifnot(is_integer(dim))
  stopifnot(vec_size(dim) == 2L)
  stopifnot(is_character(row_names))
  stopifnot(is_character(col_names))

  new_rray(
    .data = .data,
    dim = dim,
    dim_names = list(row_names, col_names),
    ...,
    subclass = c(subclass, "vctrs_mtrx")
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
mtrx <- function(..., row_names = character()) {

  .dots <- dots_list(...)
  n_cols <- length(.dots)

  # prototype
  if (is_empty(.dots)) {
    .dots <- list(numeric())
    n_cols <- 0L
  }

  is_fully_named <- rlang::is_named(.dots)
  nms <- names2(.dots)

  if (any(nms != "")) {
    if (!is_fully_named) {
      abort("All inputs must be named, or none can be.")
    }
  }

  if (is_fully_named) {
    col_names <- nms
  } else {
    col_names <- character()
  }

  .dots <- unname(.dots)
  common_size <- vec_size_common(!!! .dots)

  mtrx_lst <- vec_recycle_common(!!! .dots, .size = common_size)
  mtrx_vec <- vec_c(!!! mtrx_lst)

  new_mtrx(
    .data = mtrx_vec,
    dim = c(common_size, n_cols),
    row_names = row_names,
    col_names = col_names
  )

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
mrtrx <- function(..., row_names = character()) {
  mat <- tibble::frame_matrix(...)
  dimnames(mat) <- list(row_names, colnames(mat))
  as_mtrx(mat)
}
