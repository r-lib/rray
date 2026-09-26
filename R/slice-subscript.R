rray_as_slice_subscript <- function(i, dimension, names = NULL) {
  .Call(ffi_rray_as_slice_subscript, i, dimension, names, environment())
}
