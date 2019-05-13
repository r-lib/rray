#' Dimension names
#'
#' Extract names of various dimensions.
#'
#' Unlike `dimnames()` which can return `NULL`, `rray_dim_names()` always returns a
#' list the same length as the dimensionality of `x`. If any dimensions do not
#' have names, `NULL` is returned for that element of the list. This
#' results in an object that's length always matches the dimensionality of `x`.
#'
#' @param x The object to extract the dimension names for.
#'
#' @param n The n-th dimension to use.
#'
#' @param nms A character vector of new dimension names for the n-th dimension.
#'
#' @param value The new dimension names to use for `x`. This is a list of
#' either character vectors or `NULL`.
#'
#' @name dim-names
#'
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#' rray_dim_names(x) <- list(letters[1:5], character())
#' x
#' rray_dim_names(x)
#'
#' # 1D object, so 1 set of dim names
#' rray_dim_names(rray())
#'
#' # 2D object, so 2 sets of dim names
#' rray_dim_names(rray(1, dim = c(1, 1)))
#'
#' # 3D object, so 3 sets of dim names
#' rray_dim_names(rray(1, dim = c(1, 1, 1)))
#'
#' # NULL is technically 1D
#' rray_dim_names(NULL)
#'
#' # Vectors are treated as 1D arrays
#' rray_dim_names(1:5)
#'
#' vec <- c(x = 1, y = 2)
#' rray_dim_names(vec)
#'
#' # You can add dim names more easily
#' # using set_dim_names()
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
rray_dim_names <- function(x) {
  rray__dim_names(x)
}

#' @export
dimnames.vctrs_rray <- function(x) {
  rray_dim_names(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname dim-names
`rray_dim_names<-` <- function(x, value) {
  UseMethod("rray_dim_names<-")
}

#' @export
`rray_dim_names<-.default` <- function(x, value) {
  set_full_dim_names(x, value)
}

#' @export
`rray_dim_names<-.vctrs_rray` <- function(x, value) {
  set_full_dim_names(x, value)
}

set_full_dim_names <- function(x, value) {
  UseMethod("set_full_dim_names")
}

set_full_dim_names.default <- function(x, value) {

  if (is.null(x)) {
    return(NULL)
  }

  if (is_null(value)) {
    value <- new_empty_dim_names(rray_dims(x))
  }

  # checks for a dim attribute of positive length
  if (is.array(x)) {
    dimnames(x) <- value
  }
  else {
    names(x) <- value[[1]]
  }

  x
}

set_full_dim_names.vctrs_rray <- function(x, value) {

  if (is_null(value)) {
    value <- new_empty_dim_names(rray_dims(x))
  }

  stopifnot(map_lgl(value, is_character_or_null))

  # n shape dims and n elements of shape name list
  stopifnot(rray_dims(x) == vec_size(value))

  # dim & rray_dim_names
  dim_name_lengths <- map_int(value, vec_size)
  stopifnot(
    map2_lgl(rray_dim(x), dim_name_lengths, validate_equal_size_or_no_names)
  )

  attr(x, "dimnames") <- value
  x
}

# Base R compat

#' @export
`dimnames<-.vctrs_rray` <- function(x, value) {
  rray_dim_names(x) <- value
  x
}

#' @export
`names<-.vctrs_rray` <- function(x, value) {

  if (rray_dims(x) > 1L) {
    glubort("Cannot set `names` on a 2D+ object. Use `rray_dim_names<-()` instead.")
  }

  if (!is.null(value)) {
    value <- list(value)
  }

  set_full_dim_names(x, value)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname dim-names
set_dim_names <- function(x, n, nms) {

  dim <- rray_dim(x)

  n <- vec_cast(n, integer())
  validate_scalar_n(n)
  validate_requested_dims(x, n)

  nms <- vec_cast(nms, character())
  validate_equal_size_or_no_names(dim[n], vec_size(nms))

  rray_dim_names(x)[[n]] <- nms

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

  rray_dim_names(x)[[n]]
}

# ------------------------------------------------------------------------------

validate_scalar_n <- function(n) {
  if (!is_scalar_integer(n)) {
    glubort("`n` must have size 1, not {length(n)}.")
  }
}

validate_requested_dims <- function(x, n) {

  dims <- rray_dims(x)
  if (dims < n) {
    glubort(
      "The dimensionality of `x` ({dims}) must be ",
      "greater than the requested dimension ({n}).")
  }

}

# Similar to dim2 but takes rray_dim_names and extends
# it to match the number of dims
dim_names_extend <- function(rray_dim_names, dims) {
  nms_dims <- length(rray_dim_names)

  if (nms_dims == dims) {
    rray_dim_names
  } else if (nms_dims < dims) {
    c(rray_dim_names, new_empty_dim_names(dims - nms_dims))
  } else {
    abort("Can not decrease dimensions")
  }

}
