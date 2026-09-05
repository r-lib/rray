rray_cast_common <- function(
  ...,
  .to = NULL,
  .arg = "",
  .to_arg = ".to",
  .call = caller_env()
) {
  .Call(ffi_rray_cast_common, list2(...), .to, environment())
}
