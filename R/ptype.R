rray_ptype2 <- function(x, y) {
  .Call(ffi_rray_ptype2, x, y, environment())
}

rray_ptype_common <- function(...) {
  .Call(ffi_rray_ptype_common, list2(...), environment())
}
