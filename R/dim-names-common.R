#' Find common dimension names
#'
#' Obtain a list of common dimension names among a set of objects. For
#' interactive use, `rray_dim_names_common()` is more useful.
#'
#' `rray_dim_names_common()` is the engine that determines what dim names should
#' be used in the result of arithmetic operations and other functions that
#' involve multiple rray objects and return a single result.
#'
#' The rules for determining the set of common dim names between objects
#' `x` and `y` (in that order) are:
#'
#' - Compute the common `dim` between `x` and `y` using `rray_dim_common()`.
#'
#' - For each axis along the common `dim`:
#'    - If `x` has names for that axis _and_ the size of the names vector
#'    is the same as the size of the axis, use those names for that axis.
#'    - Else if `y` has names for that axis _and_ the size of the names vector
#'     is the same as the size of the axis, use those names for that axis.
#'    - Otherwise, the names for that axis is `NULL`.
#'
#' @param x,y,... Objects to find common dimensions for.
#'
#' @return
#'
#' A list of the common dimension names of the inputs.
#'
#' @examples
#' library(magrittr)
#'
#' # 1x2 - Row names but no column names
#' x <- rray(1, dim = c(1, 2)) %>%
#'   rray_set_row_names("r_from_x")
#'
#' # 1x1 - Row names and column names
#' y <- rray(1, dim = c(1, 1)) %>%
#'   rray_set_col_names("c_from_y") %>%
#'   rray_set_row_names("r_from_y")
#'
#' # 1x1 - Row names but no column names
#' z <- rray(1, c(1, 1)) %>%
#'   rray_set_row_names("r_from_z")
#'
#' # Combining y and z
#' # y has names for both dimensions
#' # so they are used
#' rray_dim_names_common(y, z)
#'
#' # Combining z and y
#' # - Row names are found first from z
#' # - But z has no column names
#' # - So column names are found from y
#' rray_dim_names_common(z, y)
#'
#' # Combining x and y
#' # - Row names are found first from x
#' # - x has no column names
#' # - y has column names but they are
#' #   a different length from the common
#' #   column dimension (common size of 2)
#' # - So no column names are used
#' rray_dim_names_common(x, y)
#'
#' @name common-dim-names
#'
NULL

#' @export
#' @rdname common-dim-names
rray_dim_names_common <- function(...) {
  args <- compact(list2(...))

  if (length(args) == 0) {
    return(list())
  }

  dim <- rray_dim_common(!!! args)

  arg_dim_names <- map(args, rray_dim_names)
  arg_dim_names <- map(arg_dim_names, rray_resize_dim_names, dim = dim)

  reduce(arg_dim_names, rray_coalesce_dim_names)
}

#' @export
#' @rdname common-dim-names
rray_dim_names2 <- function(x, y) {
  rray__dim_names2(x, y)
}

rray_coalesce_dim_names <- function(x_dim_names, y_dim_names) {
  rray__coalesce_dim_names(x_dim_names, y_dim_names)
}

rray_resize_dim_names <- function(dim_names, dim) {
  rray__resize_dim_names(dim_names, dim)
}

# returns a list of n empty characters
rray_empty_dim_names <- function(n) {
  n <- vec_cast(n, integer())
  rray__new_empty_dim_names(n)
}
