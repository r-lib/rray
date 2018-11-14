#' Dimension names
#'
#' Extract names of various dimensions.
#'
#' Unlike `dimnames()` which can return `NULL`, `dim_names()` always returns a
#' list the same length as the dimensionality of `x`. If any dimensions do not
#' have names, `character(0)` is returned for that element of the list.
#'
#' @param x The object to extract the dimension names for.
#' @param n The n-th dimension to extract names for.
#'
#' @name dim-names
#'
#' @examples
#'
#' mtrx(x = 1:5, y = 6:10, row_names = letters[1:5])
#'
NULL

#' @export
#' @name dim-names
dim_names <- function(x) {
  UseMethod("dim_names")
}

#' @export
dim_names.default <- function(x) {
  dimnames(x)
}

#' @export
dim_names.array <- function(x) {
  dim_nms <- unname(dimnames(x))

  if (is.null(dim_nms)) {
    dims <- vec_dims(x)
    return(new_empty_dim_names(dims))
  }

  # NULL -> character(0)
  dim_nms <- map(dim_nms, function(x) if(is.null(x)) character() else x)

  dim_nms
}

#' @export
dim_names.matrix <- dim_names.array

#' @export
dim_names.vctrs_rray <- function(x) {
  attr(x, "dim_names")
}

# treat vectors as 1 column matrices

#' @export
dim_names.double <- function(x) {
  list(names(x) %||% character(), character())
}

#' @export
dim_names.integer <- dim_names.double

#' @export
dim_names.logical <- dim_names.double

# ------------------------------------------------------------------------------

`dim_names<-` <- function(x, value) {
  UseMethod("dim_names<-")
}

`dim_names<-.default` <- function(x, value) {
  dimnames(x) <- value
  x
}

`dim_names<-.vctrs_rray` <- function(x, value) {
  attr(x, "dim_names") <- value
  x
}

# ------------------------------------------------------------------------------

rray_dim_names_common <- function(...) {
  args <- compact(list2(...))

  if (length(args) == 0) {
    return(list())
  }

  dim <- rray_dim_common(!!! args)
  args_dim_names <- map(args, restore_dim_names, to_dim = dim)

  reduce(args_dim_names, reconcile_dim_names)
}

rray_dim_names2 <- function(x, y) {

  dim <- rray_dim2(vec_dim(x), vec_dim(y))
  x_nms_list <- restore_dim_names(x, dim)
  y_nms_list <- restore_dim_names(y, dim)

  reconcile_dim_names(x_nms_list, y_nms_list)
}

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

new_empty_dim_names <- function(n) {
  map(seq_len(n), function(x) character())
}

# ------------------------------------------------------------------------------

#' @export
#' @name dim-names
row_names <- function(x) {
  UseMethod("row_names")
}

#' @export
row_names.default <- function(x) {
  n_dim_names(x, 1L)
}

# ------------------------------------------------------------------------------

#' @export
#' @name dim-names
col_names <- function(x) {
  UseMethod("col_names")
}

#' @export
col_names.default <- function(x) {
  n_dim_names(x, 2L)
}

# ------------------------------------------------------------------------------

#' @export
#' @name dim-names
n_dim_names <- function(x, n) {
  UseMethod("n_dim_names")
}

#' @export
n_dim_names.default <- function(x, n) {

  n <- vec_cast(n, integer())
  if (!is_scalar_integer(n)) {
    glubort("`n` must have size 1, not {length(n)}.")
  }

  dims <- vec_dims(x)
  if (dims < n) {
    glubort(
      "The dimensionality of `x` ({dims}) must be ",
      "greater than the requested dimension ({n})")
  }

  dim_names(x)[[n]]
}
