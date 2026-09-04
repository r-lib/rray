rray_ptype2 <- function(x, y) {
  .Call(ffi_rray_ptype2, x, y, environment())
}
