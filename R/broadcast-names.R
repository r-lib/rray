# Test only, no validation
rray_broadcast_names <- function(x, dimensions) {
  .Call(ffi_rray_broadcast_names, x, dimensions)
}

rray_broadcast_names2 <- function(x, y, dimensions) {
  .Call(ffi_rray_broadcast_names2, x, y, dimensions)
}

rray_broadcast_names_common <- function(..., .dimensions) {
  .Call(ffi_rray_broadcast_names_common, list2(...), .dimensions)
}
