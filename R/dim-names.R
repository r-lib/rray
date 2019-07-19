#' Dimension names
#'
#' @description
#'
#' This family of functions allows you to get and set dimension names in various
#' ways.
#'
#' - `rray_dim_names()` returns a list of the dimension names.
#'
#' - `rray_axis_names()` returns a character vector or `NULL` containing the
#' names corresponding to the `axis` dimension.
#'
#' - `rray_row_names()` and `rray_col_names()` are helpers for getting the
#' row and column names respectively.
#'
#' - Each of these four functions also has "set" variants: a functional form
#' (i.e. `rray_set_row_names()`), and an assignment
#' form (i.e. `rray_row_names<-()`).
#'
#' @details
#'
#' Unlike `dimnames()` which can return `NULL`, `rray_dim_names()` always returns a
#' list the same length as the dimensionality of `x`. If any dimensions do not
#' have names, `NULL` is returned for that element of the list. This
#' results in an object that's length always matches the dimensionality of `x`.
#'
#' @param x The object to extract the dimension names for.
#'
#' @param dim_names A list of either character vectors or `NULL` representing
#' the new dim names of `x`. If `NULL` is supplied, the dim names of `x` are
#' removed.
#'
#' @param axis A single integer. The axis to select dimension names for.
#'
#' @param names A character vector of new dimension names
#' for the `axis` dimension. This is also allowed to be `NULL` to remove
#' dimension names for the specified axis.
#'
#' @param value For `rray_dim_names<-()`, a list containing either character
#' vectors or `NULL` corresponding to the new dimension names
#' to use for `x`. Otherwise, identical to `names`.
#'
#' @param meta A single character representing an optional "meta" name
#' assigned to the `axis` names. If `NULL`, the current meta name is kept.
#'
#' @return
#'
#' `rray_dim_names()` returns a list of dimension names. The other names
#' functions return character vectors, or `NULL`, corresponding to the
#' names of a particular axis.
#'
#' @name dim-names
#'
#' @examples
#' x <- rray(1:10, c(5, 2))
#' rray_dim_names(x) <- list(letters[1:5], NULL)
#' x
#' rray_dim_names(x)
#'
#' # 3D object, so 3 dim name elements
#' rray_dim_names(rray(1, dim = c(1, 1, 1)))
#'
#' # Vectors are treated as 1D arrays
#' vec <- c(x = 1, y = 2)
#' rray_dim_names(vec)
#'
#' # You can add dim names more easily
#' # using rray_set_axis_names()
#' # and the pipe operator
#' library(magrittr)
#' y <- rray(1, c(1, 2, 1)) %>%
#'   rray_set_axis_names(1, "r1") %>%
#'   rray_set_axis_names(2, c("c1", "c2")) %>%
#'   rray_set_axis_names(3, "3rd dim")
#'
#' y
#'
#' # You can set also set axis names to `NULL` to reset them
#' rray_set_axis_names(y, 2, NULL)
#'
#' # You can set the "meta" names of an axis as well
#' rray_set_axis_names(y, 1, "r1", "row names")
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
  rray_set_dim_names(x, value)
}

#' @export
#' @rdname dim-names
rray_set_dim_names <- function(x, dim_names) {
  dim <- rray_dim(x)
  dim_n <- vec_size(dim)

  if (is_null(dim_names)) {
    dim_names <- rray_empty_dim_names(dim_n)
  }

  validate_dim_names(dim_names, dim)

  rray_set_dim_names_impl(x, dim_names)
}

# `attr<-()` does't double copy like `attributes<-()` does so this is fine
# is.array() just checks for a `dim` attribute of positive length
rray_set_dim_names_impl <- function(x, dim_names) {
  if (is.array(x)) {
    attr(x, which = "dimnames") <- dim_names
  }
  else {
    attr(x, "names") <- dim_names[[1]]
  }

  x
}

validate_dim_names <- function(dim_names, dim) {
  dim_n <- vec_size(dim)
  n_dim_names <- vec_size(dim_names)

  if (dim_n != n_dim_names) {
    glubort(
      "The dimensionality of the object ({dim_n}) must be equal ",
      "to the size of the `dim_names` ({n_dim_names})."
    )
  }

  map2(dim_names, dim, validate_axis_names)

  invisible(dim_names)
}

validate_axis_names <- function(axis_names, n) {

  if (is_null(axis_names)) {
    return(invisible(axis_names))
  }

  if (!is.character(axis_names)) {
    abort("All dim names must be character vectors, or `NULL`.")
  }

  if (vec_size(axis_names) != n) {
    glubort(
      "The size of each dimension's names must be equal to the ",
      "size of the corresponding dimension."
    )
  }

  return(invisible(axis_names))
}

# ------------------------------------------------------------------------------
# Base R compat

#' @export
`dimnames<-.vctrs_rray` <- function(x, value) {
  rray_set_dim_names(x, value)
}

#' @export
`names<-.vctrs_rray` <- function(x, value) {

  if (rray_dim_n(x) > 1L) {
    glubort(
      "Cannot set `names` on a 2D+ object. Use `rray_dim_names<-()` instead."
    )
  }

  if (!is.null(value)) {
    value <- list(value)
  }

  rray_set_dim_names(x, value)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname dim-names
rray_axis_names <- function(x, axis) {
  validate_axis(axis, x)
  rray_dim_names(x)[[axis]]
}

#' @export
#' @rdname dim-names
`rray_axis_names<-` <- function(x, axis, value) {
  rray_set_axis_names(x, axis, value)
}

#' @export
#' @rdname dim-names
rray_set_axis_names <- function(x, axis, names, meta = NULL) {
  axis <- vec_cast(axis, integer())
  vec_assert(axis, size = 1L, arg = "axis")

  validate_axis(axis, x)

  # Done in two steps to allow `rray_set_axis_names(x, axis, NULL)`
  new_dim_names <- rray_dim_names(x)
  new_dim_names[axis] <- list(names)

  if (!is.null(meta)) {
    vec_assert(meta, ptype = character(), size = 1L, arg = "meta")
    names2(new_dim_names)[axis] <- meta
  }

  rray_set_dim_names(x, new_dim_names)
}

# rlang:::names2<-()
`names2<-` <- function (x, value) {
  if (is_null(names(x))) {
    names(x) <- names2(x)
  }
  names(x) <- value
  x
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname dim-names
rray_row_names <- function(x) {
  rray_axis_names(x, 1L)
}

#' @export
#' @rdname dim-names
`rray_row_names<-` <- function(x, value) {
  rray_set_row_names(x, value)
}

#' @export
#' @rdname dim-names
rray_set_row_names <- function(x, names, meta = NULL) {
  rray_set_axis_names(x, 1L, names, meta)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname dim-names
rray_col_names <- function(x) {
  rray_axis_names(x, 2L)
}

#' @export
#' @rdname dim-names
`rray_col_names<-` <- function(x, value) {
  rray_set_col_names(x, value)
}

#' @export
#' @rdname dim-names
rray_set_col_names <- function(x, names, meta = NULL) {
  rray_set_axis_names(x, 2L, names, meta)
}
