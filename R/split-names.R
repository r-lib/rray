# Test only, no validation
rray_split_names <- function(x, dimensions) {
  .Call(ffi_rray_split_names, x, dimensions)
}
