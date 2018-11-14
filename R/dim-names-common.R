#' Find common dimension names
#'
#' Obtain a common list of dimension names among a set of objects. For
#' interactive use, `rray_dim_names_common()` is more useful.
#'
#' `rray_dim_names_common()` is the engine that determines what dim names should
#' be used in the result of arithmetic operations and other functions that
#' involve multiple rray or mtrx objects and return a single result.
#'
#' The rules for determining the set of common dim names between objects
#' `x` and `y` (in that order) are:
#'
#' 1) Compute the common `dim` between `x` and `y` using `rray_dim_common()`.
#' 1) For each dimension along the common `dim`, if `x` has dim names for that
#' specific dimension and the size of the names is the same as the size
#' of the dimension, use them.
#' 2) If `y` has dim names for that dimension, and the size of the names
#' is the same as the size of the dimension, use them.
#' 3) If there are no dim names found, then the result is `character(0)`.
#'
#' @param x,y,... Objects to find common dimensions for.
#'
#' @examples
#' library(magrittr)
#'
#' # 1x2 - Row names but no column names
#' x <- rray(1, dim = c(1, 2)) %>%
#'   set_row_names("r_from_x")
#'
#' # 1x1 - Row names and column names
#' y <- mtrx(c_from_y = 1) %>%
#'   set_row_names("r_from_y")
#'
#' # 1x1 - Row names but no column names
#' z <- mtrx(1) %>%
#'   set_row_names("r_from_z")
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
#' # - y has column names
#' # - But they are different length from
#' #   the common column dimension (2)
#' # - No column names are used
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
  args_dim_names <- map(args, restore_dim_names, to_dim = dim)

  reduce(args_dim_names, reconcile_dim_names)
}

#' @export
#' @rdname common-dim-names
rray_dim_names2 <- function(x, y) {

  dim <- rray_dim2(vec_dim(x), vec_dim(y))
  x_nms_list <- restore_dim_names(x, dim)
  y_nms_list <- restore_dim_names(y, dim)

  reconcile_dim_names(x_nms_list, y_nms_list)
}

# Given two sets of equal length dim names, find the
# actual "common dim names" between them
# - if x has names for a dimension, use them
# - if x has no names for a dimension, but y does, use them
reconcile_dim_names <- function(x_dim_names, y_dim_names) {

  map2(x_dim_names, y_dim_names, function(x_nms, y_nms) {

    n_x <- vec_size(x_nms)
    n_y <- vec_size(y_nms)

    if (n_x == n_y) {
      x_nms
    }
    else if (n_x == 0L) {
      y_nms
    }
    else if (n_y == 0L) {
      x_nms
    }
    else {
      abort("Imcompatible dim_name lengths.")
    }

  })

}

# Given an object x, and a dim that x is going to be
# changed to, this takes the dim names of x and:
# - if there are names for that dimension already, ensures they are the same
#   length as the dimension, or nukes them
# - if there are no names for that dimension, adds a character() for it
restore_dim_names <- function(x, to_dim) {

  dims <- vec_size(to_dim)

  dim_names <- dim_names(x)

  meta_names <- names2(dim_names)
  meta_names <- c(meta_names, rep("", times = dims - length(meta_names)))

  restored_dim_names <- new_empty_dim_names(dims)
  names(restored_dim_names) <- meta_names

  # cant use map2 bc to_dim_names could be
  # shorter than x_dim (i.e. we added a dimension)

  for(i in seq_along(dim_names)) {

    nms <- dim_names[[i]]
    single_dim <- to_dim[i]

    if (vec_size(nms) == single_dim || single_dim == 0L) {
      restored_dim_names[[i]] <- nms
    }

  }

  restored_dim_names

}

# returns a list of n empty characters
new_empty_dim_names <- function(n) {
  map(seq_len(n), function(x) character())
}
