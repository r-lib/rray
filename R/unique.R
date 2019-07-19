#' Find and count unique values in an array
#'
#' @description
#'
#' * `rray_unique()`: the unique values.
#'
#' * `rray_unique_loc()`: the locations of the unique values.
#'
#' * `rray_unique_count()`: the number of unique values.
#'
#' @param x A vector, matrix, array, or rray.
#'
#' @param axis A single integer. The axis to index `x` by.
#'
#' @return
#'
#' * `rray_unique()`: an array the same type as `x` containing only
#' unique values. The dimensions of the return value are the same as
#' `x` except on the `axis`, which might be smaller than the original
#' dimension size if any duplicate entries were removed.
#'
#' * `rray_unique_loc()`: an integer vector, giving locations of
#' the unique values.
#'
#' * `rray_unique_count()`: an integer vector of length 1, giving the number
#' of unique values.
#'
#' @details
#'
#' The family of unique functions work in the following manner:
#'
#' 1) `x` is split into pieces using the `axis` as the dimension to index along.
#'
#' 2) Each of those pieces is flattened to 1D.
#'
#' 3) The uniqueness test is done between those flattened pieces and the
#' final output is restored from that result.
#'
#' As an example, if `x` has dimensions of `(2, 3, 2)` and `axis = 2`, then
#' you can think of `x` as being broken into `x[, 1]`, `x[, 2]` and `x[, 3]`.
#' Each of those three pieces are then flattened, and a vctrs unique function
#' is called on the list of those flattened inputs.
#'
#' The result of calling `rray_unique()` will always have the same
#' dimensions as `x`, except along `axis`, which is allowed to be less than
#' the original axis size if any duplicate entries are removed.
#'
#' Unlike the duplicate functions, the unique functions only take a singular
#' `axis` argument, rather than `axes`. The reason for this is that if the
#' unique functions were defined in any other way, they would allow for _ragged
#' arrays_, which are not defined in rray.
#'
#' When duplicates are detected, the _first_ unique value is used in the result.
#'
#' @seealso
#'
#' [rray_duplicate_any()] for functions that work with the dual of
#' unique values: duplicated values.
#'
#' [vctrs::vec_unique()] for functions that detect unique values among
#' any type of vector object.
#'
#' @examples
#' x_dup_rows <- rray(c(1, 1, 3, 3, 2, 2, 4, 4), c(2, 2, 2))
#' x_dup_rows <- rray_set_row_names(x_dup_rows, c("r1", "r2"))
#' x_dup_rows <- rray_set_col_names(x_dup_rows, c("c1", "c2"))
#'
#' # Duplicate rows
#' # `x_dup_rows[1] == x_dup_rows[2]`
#' rray_unique(x_dup_rows, 1)
#'
#' # Duplicate cols
#' # `x_dup_cols[, 1] == x_dup_cols[, 2]`
#' x_dup_cols <- rray_transpose(x_dup_rows, c(2, 1, 3))
#' rray_unique(x_dup_cols, 2)
#'
#' # Duplicate 3rd dim
#' # `x_dup_layers[, , 1] == x_dup_layers[, , 2]`
#' x_dup_layers <- rray_transpose(x_dup_rows, c(2, 3, 1))
#' rray_unique(x_dup_layers, 3)
#'
#' # rray_unique_loc() returns an
#' # integer vector you can use
#' # to subset out the unique values along
#' # the axis you are interested in
#' x_dup_cols[, rray_unique_loc(x_dup_cols, 2L)]
#'
#' # Only 1 unique column
#' rray_unique_count(x_dup_cols, 2L)
#'
#' # But 2 unique rows
#' rray_unique_count(x_dup_cols, 1L)
#'
#' @export
rray_unique <- function(x, axis) {
  locs <- rray_unique_loc(x, axis)
  rray_slice(x, locs, axis)
}

#' @rdname rray_unique
#' @export
rray_unique_loc <- function(x, axis) {
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  axes <- get_axes_complement(rray_dim_n(x), axis)
  x_split_flat <- duplicate_splitter(x, axes)

  vec_unique_loc(x_split_flat)
}

#' @rdname rray_unique
#' @export
rray_unique_count <- function(x, axis) {
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  axes <- get_axes_complement(rray_dim_n(x), axis)
  x_split_flat <- duplicate_splitter(x, axes)

  vec_unique_count(x_split_flat)
}


# ------------------------------------------------------------------------------

#' @export
unique.vctrs_rray <- function(x,
                              incomparables = FALSE,
                              MARGIN = 1,
                              fromLast = FALSE,
                              ...) {

  unq <- unique(
    vec_data(x),
    incomparables = incomparables,
    MARGIN = MARGIN,
    fromLast = fromLast,
    ...
  )

  vec_cast_container(unq, x)
}
