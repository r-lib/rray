# Test only, no validation
rray_split_names <- function(x, axes) {
  .Call(ffi_rray_split_names, x, axes)
}
