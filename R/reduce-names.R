# Test only, no validation
rray_reduce_names <- function(x, axes) {
  .Call(ffi_rray_reduce_names, x, axes)
}
