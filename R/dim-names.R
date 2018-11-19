#' Dimension names
#'
#' Extract names of various dimensions.
#'
#' Unlike `dimnames()` which can return `NULL` and contain elements that are
#' `NULL`, `dim_names()` always returns a
#' list the same length as the dimensionality of `x`. If any dimensions do not
#' have names, `character(0)` is returned for that element of the list. This
#' results in a type stable result: a list where the elements are character vectors.
#'
#' A vector is treated as a 1 column matrix (so, 2 dimensions) and `dim_names()`
#' will return the names of the vector as the row names, if it has any.
#'
#' @param x The object to extract the dimension names for.
#' @param n The n-th dimension to use.
#' @param nms A character vector of new dimension names for the n-th dimension.
#' @param value The new dimension names to use for `x`. This is a list of
#' character vectors.
#'
#' @name dim-names
#'
#'
#' @examples
#'
#' x <- mtrx(x = 1:5, y = 6:10, row_names = letters[1:5])
#' dim_names(x)
#'
#' # 2D object, so 2 sets of dim names
#' dim_names(rray())
#'
#' # 3D object, so 3 sets of dim names
#' dim_names(rray(1, dim = c(1,1,1)))
#'
#' # Vectors are 1 column matrices (2D)
#' dim_names(1:5)
#'
#' vec <- c(x = 1, y = 2)
#' dim_names(vec)
#'
#' # You can add dim names using set_dim_names()
#' # and the pipe operator
#' library(magrittr)
#' rray(1, c(1, 2, 1)) %>%
#'   set_dim_names(1, "r1") %>%
#'   set_dim_names(2, c("c1", "c2")) %>%
#'   set_dim_names(3, "3rd dim")
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

# Base R compat

#' @export
dimnames.vctrs_rray <- function(x) {
  dim_names(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname dim-names
`dim_names<-` <- function(x, value) {
  UseMethod("dim_names<-")
}

#' @export
`dim_names<-.default` <- function(x, value) {
  set_full_dim_names(x, value)
}

#' @export
`dim_names<-.vctrs_rray` <- function(x, value) {
  set_full_dim_names(x, value)
}

# Using a helper function internally can help avoid
# copies when the new dim names are identical to the
# old ones. Using an assignment function like dim_names<-
# ALWAYS makes a copy no matter what.

set_full_dim_names <- function(x, value) {
  UseMethod("set_full_dim_names")
}

set_full_dim_names.default <- function(x, value) {

  # potentially avoid copy
  if (identical(dim_names(x), value)) {
    return(x)
  }

  dimnames(x) <- value
  x
}

set_full_dim_names.vctrs_rray <- function(x, value) {

  # potentially avoid copy
  if (identical(dim_names(x), value)) {
    return(x)
  }

  if (is_null(value)) {
    value <- new_empty_dim_names(vec_dims(x))
  }

  stopifnot(map_lgl(value, is_character))

  # n shape dims and n elements of shape name list
  stopifnot(vec_dims(x) == vec_size(value))

  # dim & dim_names
  dim_name_lengths <- map_int(value, vec_size)
  stopifnot(
    map2_lgl(vec_dim(x), dim_name_lengths, validate_equal_size_or_no_names)
  )

  attr(x, "dim_names") <- value
  x

}

# Base R compat

#' @export
`dimnames<-.vctrs_rray` <- function(x, value) {
  dim_names(x) <- value
  x
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname dim-names
set_dim_names <- function(x, n, nms) {

  dim <- vec_dim(x)

  n <- vec_cast(n, integer())
  validate_scalar_n(n)
  validate_requested_dims(x, n)

  nms <- vec_cast(nms, character())
  validate_equal_size_or_no_names(dim[n], vec_size(nms))

  dim_names(x)[[n]] <- nms

  x
}

#' @export
#' @rdname dim-names
set_row_names <- function(x, nms) {
  set_dim_names(x, 1L, nms)
}

#' @export
#' @rdname dim-names
set_col_names <- function(x, nms) {
  set_dim_names(x, 2L, nms)
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
  validate_scalar_n(n)
  validate_requested_dims(x, n)

  dim_names(x)[[n]]
}

# ------------------------------------------------------------------------------

validate_scalar_n <- function(n) {
  if (!is_scalar_integer(n)) {
    glubort("`n` must have size 1, not {length(n)}.")
  }
}

validate_requested_dims <- function(x, n) {

  dim <- rray_dim_at_least_2D(x)
  dims <- vec_size(dim)
  if (dims < n) {
    glubort(
      "The dimensionality of `x` ({dims}) must be ",
      "greater than the requested dimension ({n}).")
  }

}

# Similar to dim2 but takes dim_names and extends
# it to match the number of dims
dim_names_extend <- function(dim_names, dims) {
  nms_dims <- length(dim_names)

  if (nms_dims == dims) {
    dim_names
  } else if (nms_dims < dims) {
    c(dim_names, new_empty_dim_names(dims - nms_dims))
  } else {
    abort("Can not decrease dimensions")
  }

}
