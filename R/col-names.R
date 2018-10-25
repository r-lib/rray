#' Extract the column names of a mtrx
#'
#' @param x A mtrx.
#'
#' @export
col_names <- function(x) {
  UseMethod("col_names")
}

#' @export
col_names.mtrx <- function(x) {
  attr(x, "col_names")
}
