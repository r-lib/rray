#' Find duplicated values in an array
#'
#' @description
#'
#' * `rray_duplicate_any()`: returns a logical with the same shape and type
#' as `x` except over the `axes`, which will be reduced to length 1. This
#' function detects the presence of any duplicated values along the `axes`.
#'
#' * `rray_duplicate_detect()`: returns a logical with the same shape and
#' type as `x` describing if that element of `x` is duplicated elsewhere.
#'
#' * `rray_duplicate_id()`: returns an integer with the same shape and
#' type as `x` giving the location of the first occurrence of the value.
#'
#' @param x A vector, matrix, array, or rray.
#'
#' @param axes An integer vector. The default of `NULL` looks for duplicates
#' over all axes.
#'
#' @return
#'
#' See the description for return value details.
#'
#' @seealso
#'
#' [rray_unique()] for functions that work with the dual of duplicated values:
#' unique values.
#'
#' [vctrs::vec_duplicate_any()] for functions that detect duplicates among
#' any type of vector object.
#'
#' @examples
#' x <- rray(c(1, 1, 2, 2), c(2, 2))
#' x <- rray_set_row_names(x, c("r1", "r2"))
#' x <- rray_set_col_names(x, c("c1", "c2"))
#'
#' # Are there duplicates along the rows?
#' rray_duplicate_any(x, 1L)
#'
#' # Are there duplicates along the columns?
#' rray_duplicate_any(x, 2L)
#'
#' # Create a 3d version of x
#' # where the columns are not unique
#' y <- rray_expand(x, 1)
#'
#' # Along the rows, all the values are unique...
#' rray_duplicate_any(y, 1L)
#'
#' # ...but along the columns there are duplicates
#' rray_duplicate_any(y, 2L)
#'
#' # ---------------------------------------------------------------------------
#'
#' z <- rray(c(1, 1, 2, 3, 1, 4, 5, 6), c(2, 2, 2))
#'
#' # rray_duplicate_detect() looks for any
#' # duplicates along the axes of interest
#' # and returns `TRUE` wherever a duplicate is found
#' # (including the first location)
#' rray_duplicate_detect(z, 1)
#'
#' # Positions 1 and 5 are the same!
#' rray_duplicate_detect(z, 3)
#'
#' # ---------------------------------------------------------------------------
#'
#' # rray_duplicate_id() returns the location
#' # of the first occurance along each axis.
#' # Compare to rray_duplicate_detect()!
#' rray_duplicate_detect(z, 1)
#' rray_duplicate_id(z, 1)
#'
#' @name rray_duplicate
#' @export
rray_duplicate_any <- function(x, axes = NULL) {

  axes <- check_duplicate_axes(axes, x)

  x_split_flat <- duplicate_splitter(x, axes)

  flat_res <- map_lgl(x_split_flat, vec_duplicate_any)

  res <- keep_dims(flat_res, x, axes)

  new_dim_names <- rray_resize_dim_names(rray_dim_names(x), rray_dim(res))
  res <- rray_set_dim_names(res, new_dim_names)

  vec_cast_container(res, x)
}

#' @rdname rray_duplicate
#' @export
rray_duplicate_detect <- function(x, axes = NULL) {

  axes <- check_duplicate_axes(axes, x)

  x_split_flat <- duplicate_splitter(x, axes)

  res_lgl_lst <- map(x_split_flat, vec_duplicate_detect)

  res_flat <- rray_unlist(res_lgl_lst)

  res <- restore_shape(res_flat, rray_dim(x), axes)

  res <- rray_set_dim_names(res, rray_dim_names(x))

  vec_cast_container(res, x)
}

#' @rdname rray_duplicate
#' @export
rray_duplicate_id <- function(x, axes = NULL) {

  axes <- check_duplicate_axes(axes, x)

  x_split_flat <- duplicate_splitter(x, axes)

  res_lgl_lst <- map(x_split_flat, vec_duplicate_id)

  res_flat <- rray_unlist(res_lgl_lst)

  res <- restore_shape(res_flat, rray_dim(x), axes)

  res <- rray_set_dim_names(res, rray_dim_names(x))

  vec_cast_container(res, x)
}

# ------------------------------------------------------------------------------

# Decided to be consistent with base R here, to match user expectations.
# If they want the "better" behavior, use the rray functions

#' @export
duplicated.vctrs_rray <- function(x,
                                  incomparables = FALSE,
                                  MARGIN = 1,
                                  fromLast = FALSE,
                                  ...) {

  dups <- duplicated(
    vec_data(x),
    incomparables = incomparables,
    MARGIN = MARGIN,
    fromLast = fromLast,
    ...
  )

  vec_cast_container(dups, x)
}

# ------------------------------------------------------------------------------

# Just returning the length 1 integer

#' @export
anyDuplicated.vctrs_rray <- function(x,
                                     incomparables = FALSE,
                                     MARGIN = 1,
                                     fromLast = FALSE,
                                     ...) {

  anyDuplicated(
    vec_data(x),
    incomparables = incomparables,
    MARGIN = MARGIN,
    fromLast = fromLast,
    ...
  )
}

# ------------------------------------------------------------------------------

check_duplicate_axes <- function(axes, x) {
  axes <- vec_cast(axes, integer())
  validate_axes(axes, x)

  # should work for NULL and integer(0) cases
  if (any(diff(axes) <= 0L)) {
    glubort("`axes` must be unique and in ascending order.")
  }

  if (is.null(axes)) {
    axes <- seq_len(rray_dim_n(x))
  }

  axes
}

rray_unlist <- function(x) {
  unlist(x, recursive = FALSE, use.names = FALSE)
}

duplicate_splitter <- function(x, axes) {

  axes_complement <- get_axes_complement(rray_dim_n(x), axes)

  x_split <- rray_split(x, axes_complement)

  # Flatten because we need to treat each row as a vector, not as a single row
  # otherwise the vctrs functions return 1 value per row
  x_split_flat <- map(x_split, as.vector)

  x_split_flat
}

# flat -> correct shape
# by first pulling axes of interest to the front
# in a reshape, then transposing to undo
restore_shape <- function(x, dim, axes) {

  dim_n <- length(dim)

  axes_complement <- get_axes_complement(dim_n, axes)

  permutation <- c(axes, axes_complement)

  out <- rray_reshape(x, dim[permutation])
  out <- rray_transpose(out, order(permutation))

  out
}

get_axes_complement <- function(dim_n, axes) {
  axes_seq <- seq_len(dim_n)

  if (length(axes) == 0L) {
    axes_seq
  }
  else {
    axes_seq[-axes]
  }
}

keep_dims <- function(res, x, axis) {

  new_dim <- rray_dim(x)

  if (is.null(axis)) {
    new_dim[] <- 1L
  }
  else {
    new_dim[axis] <- 1L
  }

  res <- rray_reshape(res, new_dim)

  res
}

