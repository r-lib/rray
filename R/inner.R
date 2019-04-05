rray_type_inner <- function(x) {
  vec_data(x)[0]
}

rray_type_inner2 <- function(x, y) {
  vec_type2(rray_type_inner(x), rray_type_inner(y))
}

rray_type_inner_common <- function(..., .ptype = NULL) {

  if (!vctrs::is_partial(.ptype)) {
    return(rray_type_inner(.ptype))
  }

  args <- compact(list2(.ptype, ...))

  if (length(args) == 0) {
    ptype <- NULL
  }
  else if (length(args) == 1) {
    ptype <- rray_type_inner(args[[1]])
  }
  else {
    ptypes <- map(args, rray_type_inner)
    ptype <- reduce(ptypes, rray_type_inner2)
  }

  vctrs::vec_type_finalise(ptype)
}

# - No change in dim/shape
# - Only changing the inner type of the data
# - Names are kept
rray_cast_inner <- function(x, to) {

  to <- rray_type_inner(to)

  # same as vctrs:::shape_broadcast() in this case
  to <- rray_reshape(to, shape_dim(x))

  res <- vec_cast(vec_data(x), to)

  res <- set_full_dim_names(res, dim_names(x))

  vec_restore(res, x)
}

# Cast inputs to the same _inner_ types
# <vctrs_rray<integer>[,2][2]> -> <vctrs_rray<double>[,2][2]>
# <vctrs_rray<double>[,1][2]>  -> <vctrs_rray<double>[,1][2]>
# notice that shapes haven't been cast to a common shape
rray_cast_inner_common <- function(..., .to = NULL) {
  args <- list2(...)
  inner_type <- rray_type_inner_common(!!!args, .ptype = .to)
  map(args, rray_cast_inner, to = inner_type)
}

shape_dim <- function(x) {
  dim <- vec_dim(x)
  dim[1] <- 0L
  dim
}
