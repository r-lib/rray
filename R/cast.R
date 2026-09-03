rray_cast <- function(x, to) {
  .Call(ffi_rray_cast, x, to, environment())
}

rray_cast_common <- function(..., .to) {
  .Call(ffi_rray_cast_common, list2(...), .to, environment())
}
