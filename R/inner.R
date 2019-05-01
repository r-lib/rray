# ------------------------------------------------------------------------------
# Find a common inner type

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

# ------------------------------------------------------------------------------
# Cast to a common inner type

# NOTE - These are RAW inner casts. All attributes are stripped from the results.
# These are internal functions, and are generally called from something that
# restores attributes. If these called `vec_restore()` at the end, an extra copy
# would be done. It is necessary to call `vec_cast()` and not use our own version
# of `vec_coerce_bare()` because `vec_cast()` will warn of any lossy casts.

# - No change in dim/shape
# - Only changing the inner type of the data
# - Names are NOT kept, something else should do that
# - Outer type is not kept, should be restored by something else
rray_cast_inner <- function(x, to) {

  to <- rray_type_inner(to)

  to <- new_shape(to, rray_shape(x))

  res <- vec_cast(vec_data(x), to)

  res
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

new_shape <- function(type, shape = NULL) {
  if (length(shape) == 0L) {
    type
  }
  else {
    structure(type, dim = c(0L, shape))
  }
}
