#' Array titles
#'
#' @description
#' Titles are the names on an array's list of names. They appear in
#' array print output.
#'
#' - `rray_titles()` returns the titles for every axis.
#'
#' - `rray_axis_title()` returns the title for one axis.
#'
#' - `rray_set_titles()` sets the titles for every axis.
#'
#' - `rray_set_axis_title()` sets the title for one axis.
#'
#' @param x An array.
#'
#' @param axis A single integer. The axis to get or set the title for.
#'
#' @param titles A character vector with one title per axis, or `NULL` to
#'   remove all titles.
#'
#' @param title A single character string, or `NULL` to remove the title for
#'   `axis`.
#'
#' @returns
#' - `rray_titles()` returns a character vector, or `NULL` if there are no
#'   titles.
#' - `rray_axis_title()` returns one character string, or `NULL` if there are
#'   no titles. It returns `""` when the titles vector exists but that axis
#'   has no title.
#' - The setters return `x` with new titles. Plain vectors become
#'   one-dimensional arrays.
#'
#' @name rray-titles
#' @examples
#' x <- matrix(1:6, 2, 3)
#' x <- rray_set_titles(x, c("Row", "Column"))
#' rray_titles(x)
#' rray_axis_title(x, 2)
#' rray_set_axis_title(x, 1, NULL)
NULL

#' @rdname rray-titles
#' @export
rray_titles <- function(x) {
  .Call(ffi_rray_titles, x, environment())
}

#' @rdname rray-titles
#' @export
rray_axis_title <- function(x, axis) {
  .Call(ffi_rray_axis_title, x, axis, environment())
}

#' @rdname rray-titles
#' @export
rray_set_titles <- function(x, titles) {
  .Call(ffi_rray_set_titles, x, titles, environment())
}

#' @rdname rray-titles
#' @export
rray_set_axis_title <- function(x, axis, title) {
  .Call(ffi_rray_set_axis_title, x, axis, title, environment())
}
