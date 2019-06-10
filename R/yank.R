#' Get or set elements of an array by position
#'
#' `rray_yank()` is the counterpart to [rray_extract()]. It extracts elements
#' from an array _by position_. It _always_ drops dimensions
#' (unlike [rray_subset()]), and a 1D vector is always returned. It powers
#' the `[[` method for rrays.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param i One of the following:
#' - An integer vector specifying the positions of the elements to yank.
#' - A 1D logical vector of length 1, or `rray_elems(x)`.
#' - A logical with the same dimension as `x`.
#'
#' @param value A 1D value to be assigned to the location yanked by `i`. It will
#' be cast to the type and length of `x` after being yanked by `i`.
#'
#' @param ... Not used. An error is thrown if extra arguments are supplied here.
#'
#' @details
#'
#' Dimension names are _only_ kept in the special case of calling `rray_yank()`
#' on a 1D object. Otherwise, the method of keeping them is not well defined.
#'
#' `rray_yank()` works with base R objects.
#'
#' `rray_yank()` is meant as a replacement for the traditional behavior of
#' `x[i]` since `[` for rray objects is much stricter. Separating this special
#' behavior into a different function is less surprising.
#'
#' Additionally, base R has `x[[i]]` which restricts `i` to be length 1.
#' For rray objects, `[[` acts more like `x[i]`, always dropping to 1D, but
#' allowing for the selection of multiple positions.
#'
#' You _cannot_ do `x[[i, j, ...]]` with rrays. For that behavior,
#' see [rray_extract()].
#'
#' @examples
#' x <- rray(10:17, c(2, 2, 2))
#'
#' # Resulting dimension is always 1D
#' rray_yank(x, 1:3)
#'
#' # With logical
#' rray_yank(x, FALSE)
#' rray_yank(x, rep(c(TRUE, FALSE), times = rray_elems(x) / 2))
#'
#' # You can assign a 1D vector to these yanked selections
#' rray_yank(x, c(1, 3, 5)) <- 9
#'
#' # Logicals with the same dim as `x`
#' # can also be used as a yank indexer
#' lgl <- rray(c(TRUE, FALSE), c(2, 2, 2))
#' rray_yank(x, lgl)
#'
#' # And you can set elements in these locations
#' rray_yank(x, lgl) <- NA
#'
#' # `[[` for rray objects is powered by
#' # rray_yank().
#' # This can be very useful for
#' # performing assignment
#' # by position.
#' x[[c(1, 3)]] <- NA
#'
#' # Logical arrays with the same shape as `x`
#' # can be assigned to. This is a useful way
#' # to get rid of NA values.
#' idx <- array(is.na(as.vector(x)), c(2, 2, 2))
#'
#' x[[idx]] <- 0
#'
#' @family rray subsetters
#' @export
rray_yank <- function(x, i) {
  rray_yank_impl(x, maybe_missing(i))
}

#' @rdname rray_yank
#' @export
`[[.vctrs_rray` <- function(x, i, ...) {
  validate_empty_yank_dots(...)
  rray_yank_impl(x, maybe_missing(i))
}

rray_yank_impl <- function(x, i) {
  i <- maybe_missing(i, TRUE)
  i <- as_yank_indexer(i, x)

  # TODO
  if (is.integer(i) && is_any_na_int(i)) {
    abort("`NA` indices are not yet supported.")
  }
  else if (is.logical(i) && is_any_na_int(as.integer(i))) {
    abort("`NA` indices are not yet supported.")
  }

  rray__yank(x, i)
}

# ------------------------------------------------------------------------------

as_yank_indexer <- function(i, x) {
  if (is.character(i)) {
    glubort("Cannot yank with a character `i`.")
  }

  # Special case with logical array index
  if (is.logical(i) && identical(rray_dim(i), rray_dim(x))) {
    # Waiting on https://github.com/QuantStack/xtensor/issues/1663
    # short term solution is to precompute the locations:
    return(which(i) - 1L)
    #return(i)
  }

  as_yank_indexer_default(i, x)
}

as_yank_indexer_default <- function(i, x) {
  # Not looking at vctrs "size" here
  proxy <- seq_len(rray_elems(x))
  i <- vctrs:::vec_as_index(i, proxy)
  i - 1L
}

# ------------------------------------------------------------------------------

validate_empty_yank_dots <- function(...) {

  dots <- dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")

  if (length(dots) > 0L) {
    glubort(
      "`[[` selects elements by position. ",
      "Only `x[[i]]` is supported, but {length(dots) + 1} ",
      "indexers were supplied."
    )
  }

  invisible()
}

validate_empty_yank_assign_dots <- function(...) {

  dots <- dots_list(..., .preserve_empty = TRUE, .ignore_empty = "none")

  if (length(dots) > 0L) {
    glubort(
      "`[[<-` assigns elements by position. ",
      "Only `x[[i]] <- value` is supported, but {length(dots) + 1} ",
      "indexers were supplied."
    )
  }

  invisible()
}
