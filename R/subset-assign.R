#' @rdname rray_subset
#' @export
`rray_subset<-` <- function(x, ..., value) {
  rray_subset_assign(x, ..., value = value)
}

#' @rdname rray_subset
#' @export
`[<-.vctrs_rray` <- function(x, ..., value) {
  rray_subset_assign(x, ..., value = value)
}

# Internally, using `rray_subset_assign()` rather than `rray_subset<-()` results
# in only 1 copy rather than 2 when on R < 3.6.0 due to the way the internal
# R assignment code works

#' @rdname rray_subset
#' @export
rray_subset_assign <- function(x, ..., value) {
  indexer <- rray_as_index(x, ...)

  vec_assert(value, arg = "value")
  value <- vec_cast_inner(value, x)

  # TODO
  if (is_any_na_int(indexer)) {
    abort("`NA` indices are not yet supported.")
  }

  out <- rray__subset_assign(x, indexer, value)

  vec_cast_container(out, x)
}
