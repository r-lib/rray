#' @rdname rray_yank
#' @export
`rray_yank<-` <- function(x, i, value) {
  rray_yank_assign(x, i = maybe_missing(i), value = value)
}

#' @rdname rray_yank
#' @export
`[[<-.vctrs_rray` <- function(x, i, ..., value) {
  validate_empty_yank_assign_dots(...)
  rray_yank_assign(x, i = maybe_missing(i), value = value)
}

#' @rdname rray_yank
#' @export
rray_yank_assign <- function(x, i, value) {
  i <- maybe_missing(i, TRUE)
  i <- as_yank_indexer(i, x)

  vec_assert(value, arg = "value")
  value <- vec_cast_inner(value, x)

  # TODO
  if (is.integer(i) && is_any_na_int(list(i))) {
    abort("`NA` indices are not yet supported.")
  }
  else if (is.logical(i) && is_any_na_int(list(as.integer(i)))) {
    abort("`NA` indices are not yet supported.")
  }

  out <- rray__yank_assign(x, i, value)

  vec_cast_container(out, vec_ptype_container(x))
}

# ------------------------------------------------------------------------------

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
