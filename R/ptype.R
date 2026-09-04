rray_ptype2 <- function(
  x,
  y,
  ...,
  x_arg = caller_arg(x),
  y_arg = caller_arg(y)
) {
  check_dots_empty0(...)
  .Call(ffi_rray_ptype2, x, y, environment())
}
