#' @rdname rray_extract
#' @export
`rray_extract<-` <- function(x, ..., value) {
  rray_extract_assign_impl(x, ..., value = value)
}

rray_extract_assign_impl <- function(x, ..., value) {
  vec_assert(value, arg = "value")

  x_extract <- rray_extract_impl(x, ...)
  value <- vec_cast(value, x_extract)
  value <- rray_broadcast(value, rray_dim(x_extract))

  out <- vec_data(x)

  indexer <- rray_as_index(x, ..., with_drop = FALSE)

  eval_bare(expr(out[!!!indexer] <- value))

  vec_cast_container(out, x)
}
