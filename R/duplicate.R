#' Find duplicated values in an array
#'
#' @description
#'
#' * `rray_duplicate_any()`: detects the presence of duplicated values,
#' similar to [anyDuplicated()] with the `MARGIN` argument.
#'
#' * `rray_duplicate_detect()`: returns a logical vector describing if each
#' element of the vector is duplicated elsewhere. It is similar to
#' [duplicated()] with the `MARGIN` argument, but it reports all duplicated
#' values, not just the second and subsequent repetitions.
#'
#' * `rray_duplicate_id()`: returns an integer vector giving the location of the
#' first occurence of the value.
#'
#' @param x A vector, matrix, array, or rray.
#'
#' @param axis A single integer. The axis to look for duplicate values over.
#'
#' @return
#'
#' * `rray_duplicate_any()`: a logical vector of length 1.
#'
#' * `rray_duplicate_detect()`: a logical vector the same length as `x`.
#'
#' * `rray_duplicate_id()`: an integer vector the same length as `x`.
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
#' x <- set_row_names(x, c("r1", "r2"))
#' x <- set_col_names(x, c("c1", "c2"))
#'
#' # Are there duplicates along the rows?
#' rray_duplicate_any(x, 1L)
#'
#' # Are there duplicates along the columns?
#' rray_duplicate_any(x, 2L)
#'
#' # Create a 3d version of x
#' # where the columns are not unique
#' y <- rray_expand_dims(x, 1)
#'
#' # All of the rows are unique...
#' rray_duplicate_any(y, 1L)
#'
#' # ...but the columns are not
#' rray_duplicate_any(y, 2L)
#'
#' # rray_duplicate_detect() returns
#' # `TRUE` if a duplicate is detected
#' # anywhere in the array (both
#' # columns are registered as
#' # duplicates)
#' rray_duplicate_detect(y, 2)
#'
#' # duplicated() only returns `TRUE`
#' # for the second and all subsequent
#' # duplicates
#' duplicated(as_array(y), MARGIN = 2L)
#'
#' # Find the location of the
#' # first unique values
#' rray_duplicate_id(y, 2L)
#'
#' # Both of the 3rd dimension elements
#' # are unique
#' rray_duplicate_id(y, 3L)
#'
#' @name rray_duplicate
#' @export
rray_duplicate_any <- function(x, axes = NULL) {

  axes <- check_duplicate_axes(axes, x)

  x_split_flat <- duplicate_splitter(x, axes)

  flat_res <- map_lgl(x_split_flat, vec_duplicate_any)

  res <- keep_dims(flat_res, x, axes)

  new_dim_names <- restore_dim_names(dim_names(x), vec_dim(res))
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}

#' @rdname rray_duplicate
#' @export
rray_duplicate_detect <- function(x, axes = NULL) {

  axes <- check_duplicate_axes(axes, x)

  x_split_flat <- duplicate_splitter(x, axes)

  res_lgl_lst <- map(x_split_flat, vec_duplicate_detect)

  res_flat <- rray_unlist(res_lgl_lst)

  res <- restore_shape(res_flat, vec_dim(x), axes)

  res <- set_full_dim_names(res, dim_names(x))

  vec_restore(res, x)
}

#' @rdname rray_duplicate
#' @export
rray_duplicate_id <- function(x, axes = NULL) {

  axes <- check_duplicate_axes(axes, x)

  x_split_flat <- duplicate_splitter(x, axes)

  res_lgl_lst <- map(x_split_flat, vec_duplicate_id)

  res_flat <- rray_unlist(res_lgl_lst)

  res <- restore_shape(res_flat, vec_dim(x), axes)

  res <- set_full_dim_names(res, dim_names(x))

  vec_restore(res, x)
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
    axes <- seq_len(vec_dims(x))
  }

  axes
}

rray_unlist <- function(x) {
  unlist(x, recursive = FALSE, use.names = FALSE)
}

duplicate_splitter <- function(x, axes) {

  axes_complement <- get_axes_complement(vec_dims(x), axes)

  # Get reversed complement of axes
  # These are used to split with
  axes_complement_rev <- rev(axes_complement)

  x_split <- rray_split(x, axes_complement_rev)

  # Flatten because we need to treat each row as a vector, not as a single row
  # otherwise the vctrs functions return 1 value per row
  x_split_flat <- map(x_split, as.vector)

  x_split_flat
}

# flat -> correct shape
# by first pulling axes of interest to the front
# in a reshape, then transposing to undo
restore_shape <- function(x, dim, axes) {

  dims <- length(dim)

  axes_complement <- get_axes_complement(dims, axes)

  permutation <- c(axes, axes_complement)

  out <- rray_reshape(x, dim[permutation])
  out <- rray_transpose(out, order(permutation))

  out
}

get_axes_complement <- function(dims, axes) {
  axes_seq <- seq_len(dims)

  if (length(axes) == 0L) {
    axes_seq
  }
  else {
    axes_seq[-axes]
  }
}
