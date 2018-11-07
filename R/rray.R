#' Create a new rray
#'
#' Low level constructor for rray objects
#'
#' @param .data A numeric vector with no attributes representing
#' the data in the rray.
#' @param dim An integer vector describing the dimensions of the
#' `rray`.
#' @param dim_names A list. For no names, `list()`. Otherwise the list must
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
new_rray <- function(.data, dim, dim_names = list(), ..., subclass = character()) {

  stopifnot(is_vector(.data))
  stopifnot(is_integer(dim))

  # no support for rray dim_names to have names
  stopifnot(is_bare_list(dim_names))

  if (!is_empty(dim_names)) {

    stopifnot(map_lgl(dim_names, is_character))

    stopifnot(length(dim) == length(dim_names))

    dim_name_lengths <- map_int(dim_names, length)
    stopifnot(map2_lgl(dim, dim_name_lengths, are_equal_or_no_name))

  }

  new_vctr(
    .data = .data,
    dim = dim,
    dim_names = dim_names,
    ...,
    class = c(subclass, "vctrs_rray")
  )
}

is_character_or_null <- function(x) {
  is_character(x) || is_null(x)
}

are_equal_or_no_name <- function(x, y) {
  y == 0L || identical(x, y)
}
