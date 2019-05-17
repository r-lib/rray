rray_bind2 <- function(..., axis) {
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x = numeric(), dims = Inf)

  args <- compact(list2(...))

  if (length(args) == 0L) {
    return(NULL)
  }

  # outer names from `...` are attached
  lst_of_dim_names <- map(args, rray_dim_names)

  proxy <- rray_bind_type_common(args)
  args <- map(args, rray_cast_inner, to = proxy)

  res <- rray__bind(proxy, args, as_cpp_idx(axis), lst_of_dim_names)

  vec_restore(res, proxy)
}

# Takes a 0-slice in every dimension but preserves all attributes.
# We can't call `vec_type_common()` without doing this because
# the `axis` we bind along might have non-broadcastable dimensions
# but that's fine
rray_bind_type_common <- function(args) {

  arg_types <- vector("list", length(args))

  for (i in seq_along(args)) {
    arg <- args[[i]]

    if (is_rray(arg)) {
      arg_types[[i]] <- arg[[0]]
    }
    else {
      arg_types[[i]] <- arg[0]
    }

  }

  vec_type_common(!!! arg_types)
}
