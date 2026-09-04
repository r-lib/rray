rray_cast_common <- function(..., .to = NULL) {
  .Call(ffi_rray_cast_common, list2(...), .to, environment())
}
