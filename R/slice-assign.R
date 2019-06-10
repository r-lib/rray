#' @rdname rray_slice
#' @export
`rray_slice<-` <- function(x, i, axis, value) {
  rray_slice_assign(x, i = i, axis = axis, value = value)
}

#' @rdname rray_slice
#' @export
rray_slice_assign <- function(x, i, axis, value) {
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  indexer <- front_pad(i, axis)

  rray_subset_assign(x, !!!indexer, value = value)
}
