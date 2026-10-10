#' Get titles for each axis of an array
#'
#' Titles are the names on the list of dimension names. They appear in array
#' print output. `rray_titles()` returns `NULL` if there are no titles.
#'
#' @param x An array.
#'
#' @returns
#' A character vector with one title per axis, or `NULL` if there are no
#' titles. An empty string means that axis has no title.
#'
#' @export
#' @examples
#' x <- matrix(1:6, 2, 3, dimnames = list(Row = NULL, Column = NULL))
#' rray_titles(x)
rray_titles <- function(x) {
  .Call(ffi_rray_titles, x, environment())
}

#' Get the title for one axis of an array
#'
#' `rray_axis_title()` returns the title for `axis`, or `NULL` if there are no
#' titles. If the titles vector exists but that axis has no title, it returns
#' `""`.
#'
#' @param x An array.
#'
#' @param axis A single integer. The axis to get the title for.
#'
#' @returns
#' A single character string, or `NULL` if there are no titles.
#'
#' @export
#' @examples
#' x <- matrix(1:6, 2, 3, dimnames = list(Row = NULL, Column = NULL))
#' rray_axis_title(x, 1)
rray_axis_title <- function(x, axis) {
  .Call(ffi_rray_axis_title, x, axis, environment())
}

#' Set titles for every axis of an array
#'
#' `rray_set_titles()` sets the titles for every axis. It keeps the element
#' names on each axis. Use `NULL` to remove all titles, or `""` for an axis
#' without a title.
#'
#' @param x An array.
#'
#' @param titles A character vector with one title per axis, or `NULL`.
#'
#' @returns
#' `x` with new titles. Plain vectors become one-dimensional arrays.
#'
#' @export
#' @examples
#' x <- matrix(1:6, 2, 3)
#' rray_set_titles(x, c("Row", "Column"))
rray_set_titles <- function(x, titles) {
  .Call(ffi_rray_set_titles, x, titles, environment())
}

#' Set the title for one axis of an array
#'
#' `rray_set_axis_title()` sets the title for `axis`. It keeps element names
#' and titles on the other axes. Use `NULL` or `""` to remove the title.
#'
#' @param x An array.
#'
#' @param axis A single integer. The axis to set the title for.
#'
#' @param title A single character string, or `NULL`.
#'
#' @returns
#' `x` with a new title for `axis`. Plain vectors become one-dimensional
#' arrays.
#'
#' @export
#' @examples
#' x <- matrix(1:6, 2, 3)
#' rray_set_axis_title(x, 2, "Column")
rray_set_axis_title <- function(x, axis, title) {
  .Call(ffi_rray_set_axis_title, x, axis, title, environment())
}
