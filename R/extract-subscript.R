rray_as_extract_subscript <- function(i, dimensions, missing = "propagate") {
  .Call(ffi_rray_as_extract_subscript, i, dimensions, missing, environment())
}
