rray_as_extract_subscript <- function(i, dimensions) {
  .Call(ffi_rray_as_extract_subscript, i, dimensions, environment())
}
