#' Get or set elements of an array _by position_
#'
#' @description
#'
#' - `rray_yank()` gets and sets elements from an array _by position_. It is the
#' complement to [rray_extract()], which gets and sets elements _by index_.
#'
#' - For rrays, `[[` is powered by `rray_yank()`, and allows you to
#' select multiple elements by position with the syntax: `x[[i]]`.
#'
#' - There are three assignment variations that all work essentially the same.
#' They ensures that `value` has the same inner type as `x`, and `value` must be
#' 1D.
#'    - `rray_yank(x, i) <- value`
#'    - `x[[i]] <- value`
#'    - `rray_yank_assign(x, i, value)`
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param i One of the following:
#' - An integer vector specifying the positions of the elements to yank.
#' - A 1D logical vector of either length 1 or `rray_elems(x)`.
#' - A logical with the exact same dimensions as `x`.
#'
#' @param value A 1D value to be assigned to the location yanked by `i`. It will
#' be cast to the type of `rray_yank(x, i)`.
#'
#' @param ... Not used. An error is thrown if extra arguments are supplied here.
#'
#' @return
#'
#' A 1D vector of elements yanked out of `x`.
#'
#' @details
#'
#' `rray_yank()` is meant as a replacement for the traditional behavior of
#' `x[i]`, which extracts multiple elements by their position, and returns a
#' 1D vector. rray is much stricter, and for rrays `x[i]` would return the
#' `i` rows of `x`, without dropping any dimensions. Separating this special
#' behavior of extracting by position into a new function is less surprising,
#' and allows `[` to be more consistent and stable.
#'
#' `rray_yank()` never keeps dimension names. For >1D objects, this would not
#' be well defined to begin with, so the decision was made to keep this behavior
#' for 1D objects as well. Think of `rray_yank()` as a way to rip out the inner
#' elements of `x`. The dimension names and outer type are not a part of
#' this information.
#'
#' @section The Double Bracket `[[`:
#'
#' `rray_yank()` powers `[[` for rray objects. It works a bit differently from
#' base R. As with `[`, base R allows `[[` to perform two roles. It can extract
#' _one_ element by position with `x[[i]]`, or _one_ element by index with
#' `x[[i, j, ...]]`. This felt too flexible, so with rray objects `[[` is
#' directly powered by `rray_yank()`, meaning it can only do `x[[i]]`. However,
#' multiple values of `i` are allowed, rather than just 1, meaning that
#' `x[[c(3, 5)]]` will extract the 3rd and 5th positions in `x` and return
#' them as a 1D array.
#'
#' Notably this means that the index extraction behavior of `x[[i, j, ...]]`
#' is missing with `[[` for rrays. If you want that behavior,
#' see [rray_extract()].
#'
#' @examples
#' x <- rray(10:17, c(2, 2, 2))
#'
#' # Resulting dimension is always 1D, and is a base R array
#' rray_yank(x, 1:3)
#'
#' # Subsetting with a logical is possible if it is either
#' # length 1 or the length of `x`
#' rray_yank(x, FALSE)
#' rray_yank(x, rep(c(TRUE, FALSE), times = rray_elems(x) / 2))
#'
#' # You can assign a 1D vector to these yanked selections
#' # Length 1 values are recycled as required
#' rray_yank(x, c(1, 3, 5)) <- 9
#'
#' # `rray_yank()` powers `[[` as well
#' # Notably, you can yank multiple values in `[[`
#' x[[c(1, 3, 5)]] <- NA
#'
#' # Logicals with the same dim as `x` can also be used as a yank indexer
#' # This comes in handy as a way to remove NA values
#' x[[is.na(x)]] <- 0
#'
#' @name rray_yank
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
  if (is.integer(i) && is_any_na_int(list(i))) {
    abort("`NA` indices are not yet supported.")
  }
  else if (is.logical(i) && is_any_na_int(list(as.integer(i)))) {
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
    return(i)
  }

  vec_as_index(i, rray_elems(x)) - 1L
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
