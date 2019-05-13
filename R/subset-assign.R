#' @rdname rray_subset
#' @export
`rray_subset<-` <- function(x, ..., value) {
  rray_subset_assign_impl(x, ..., value = value)
}

#' @rdname rray_subset
#' @export
`[<-.vctrs_rray` <- function(x, ..., value) {
  rray_subset_assign_impl(x, ..., value = value)
}

rray_subset_assign_impl <- function(x, ..., value) {
  indexer <- rray_as_index2(x, ...)

  value <- rray_cast_inner(value, x)

  # TODO
  if (is_any_na_int(indexer)) {
    abort("`NA` indices are not yet supported.")
  }

  out <- rray__subset_assign(x, indexer, value)

  out <- set_full_dim_names(out, rray_dim_names(x))

  vec_restore(out, x)
}
