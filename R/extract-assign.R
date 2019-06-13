#' @rdname rray_extract
#' @export
`rray_extract<-` <- function(x, ..., value) {
  rray_extract_assign(x, ..., value = value)
}

#' @rdname rray_extract
#' @export
rray_extract_assign <- function(x, ..., value) {
  indexer <- rray_as_index(x, ...)

  vec_assert(value, arg = "value")
  value <- vec_cast_inner(value, x)

  # TODO
  if (is_any_na_int(indexer)) {
    abort("`NA` indices are not yet supported.")
  }

  out <- rray__extract_assign(x, indexer, value)

  vec_cast_container(out, x)
}
