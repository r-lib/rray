rray_ptype_common <- function(..., .ptype = NULL) {
  .Call(ffi_rray_ptype_common, list2(...), .ptype, environment())
}
