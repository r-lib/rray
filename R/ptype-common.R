rray_ptype_common <- function(
  ...,
  .ptype = NULL,
  .arg = "",
  .ptype_arg = ".ptype"
) {
  .Call(ffi_rray_ptype_common, list2(...), .ptype, environment())
}
