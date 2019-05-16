rray_bind2 <- function(..., axis) {
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x = numeric(), dims = Inf)

  args <- compact(list2(...))

  if (length(args) == 0L) {
    return(NULL)
  }

  proxy <- rray_type_inner_common(!!!args)
  args <- map(args, rray_cast_inner, to = proxy)

  rray__bind(proxy, args, as_cpp_idx(axis))

}
