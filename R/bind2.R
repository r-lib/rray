rray_bind2 <- function(..., axis) {
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x = numeric(), dims = Inf)

  args <- compact(list2(...))

  if (length(args) == 0L) {
    return(NULL)
  }

  out <- rray_type_inner_common(!!!args)
  args <- map(args, rray_cast_inner, to = out)

  # Remove length 0 args, which have no impact on the result
  # besides the type, which has been captured
  is_zero <- map_lgl(args, function(arg) length(arg) == 0L)
  args <- args[!is_zero]

  dims <- compute_dims(args, as_cpp_idx(axis))

  # out must have the correct number of dimensions for xt::resize()
  out <- new_shape(out, shape = rep(1L, times = dims - 1))

  rray__bind(out, args, as_cpp_idx(axis), dims)

}
