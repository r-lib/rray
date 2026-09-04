rray_cast <- function(x, to) {
  .Call(ffi_rray_cast, x, to, environment())
}
