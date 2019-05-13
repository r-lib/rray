#' Combine many arrays together into one array
#'
#' These functions bind multiple vectors, matrices, arrays, or rrays together
#' into one, combining along the `axis`.
#'
#' @param ... Vectors, matrices, arrays, or rrays.
#'
#' @param axis A single integer. The axis to bind along.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' a <- matrix(1:4, ncol = 2)
#' b <- matrix(5:6, ncol = 1)
#'
#' # Bind along columns
#' rray_bind(a, b, axis = 2)
#'
#' # Bind along rows
#' # Broadcasting is done automatically
#' rray_bind(a, b, axis = 1)
#'
#' # You can bind "up" to a new dimension
#' # to stack matrices into an array
#' rray_bind(a, b, axis = 3)
#'
#' # ---------------------------------------------------------------------------
#' # Dimension name example
#'
#' x <- matrix(
#'  1:6,
#'  ncol = 3,
#'  dimnames = list(c("a_r1", "a_r2"), c("a_c1", "a_c2", "a_c3"))
#' )
#'
#' y <- matrix(
#'  7:8,
#'  ncol = 1,
#'  dimnames = list(NULL, c("b_c1"))
#' )
#'
#' # Dimension names come along for the ride
#' # following rray name handling
#' rray_bind(x, y, axis = 2)
#'
#' # But, if any inputs are named
#' # along the dimension you are binding on
#' # then all of them must be. This errors
#' # because `y` doesn't have row names
#' \dontrun{
#' rray_bind(x, y, axis = 1)
#' }
#'
#' # You can add "outer" names to the
#' # axis you are binding along
#' rray_bind(outer = x, y, axis = 2)
#'
#' # They are added to existing names with `..`
#' # Outer names can be used to get around the
#' # fact that `y` isn't named.
#' rray_bind(outer = x, outer_y = y, axis = 1)
#'
#' @export
rray_bind <- function(..., axis) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x = numeric(), dims = Inf)

  args <- compact(list2(...))

  if (length(args) == 0L) {
    return(NULL)
  }

  # Allow for going up in dimension
  dims <- max(rray_dims_common(!!!args), axis)

  # Finalize partial types (including unspecified)
  # (have to do it again after calling vec_type())
  args <- map(args, vec_type_finalise)

  # Get types, expand to the correct dimensions, and set axis dim to 0
  arg_types <- map(args, vec_type)
  arg_types <- map(arg_types, vec_type_finalise)
  arg_types <- map(arg_types, rray_dims_match, dims = dims)
  arg_types <- map(arg_types, set_axis_to_zero, axis = axis)

  axis_sizes <- map_int(args, pull_axis_dim, axis = axis)
  out_axis_size <- sum(axis_sizes)

  if (axis == 1L) {
    out_size <- out_axis_size
  }
  else {
    out_size <- vec_size_common(!!! args)
  }

  # `axis` is currently 0, `size` is also 0 (could be the same axis)
  out_partial <- reduce(arg_types, vec_type2)

  if (axis != 1L) {
    dim(out_partial)[axis] <- out_axis_size
  }

  out <- vec_na(out_partial, n = out_size)

  # Build the assignment expr
  missing <- build_missings(dims, axis)
  assigner <- expr(rray_subset(out, !!!missing$before, at, !!!missing$after) <- arg)

  pos <- 1L
  for (i in seq_along(args)) {
    arg <- args[[i]]
    arg_axis_size <- axis_sizes[i]

    if (arg_axis_size == 0L) {
      next
    }

    # `at` controls where we update `out` at
    at <- pos:(pos + arg_axis_size - 1L)

    eval_bare(assigner)

    pos <- pos + arg_axis_size
  }

  rray_dim_names(out) <- rray_dim_names_common_along_axis(!!!args, axis = axis, dim = rray_dim(out))

  out
}

#' @rdname rray_bind
#' @export
rray_rbind <- function(...) {
  rray_bind(..., axis = 1L)
}

#' @rdname rray_bind
#' @export
rray_cbind <- function(...) {
  rray_bind(..., axis = 2L)
}

# ------------------------------------------------------------------------------
# Helpers

pull_axis_dim <- function(x, axis) {
  if (rray_dims(x) < axis) {
    1L
  }
  else {
    rray_dim(x)[axis]
  }
}

set_axis_to_zero <- function(x, axis) {
  dim(x)[axis] <- 0L
  x
}

build_missings <- function(dims, axis) {

  needs_missing <- seq_len(dims)[-axis]

  times_before <- sum(needs_missing < axis)
  times_after  <- sum(needs_missing > axis)

  before <- rep(list(missing_arg()), times = times_before)
  after  <- rep(list(missing_arg()), times = times_after)

  list(before = before, after = after)
}

# ------------------------------------------------------------------------------
# Names related helpers

rray_dim_names_common_along_axis <- function(..., axis, dim) {

  args <- compact(list2(...))

  dims <- max(rray_dims_common(!!!args), axis)
  axis_sizes <- map_int(args, pull_axis_dim, axis = axis)

  arg_dim_names <- map(args, rray_dim_names)
  arg_dim_names <- map(arg_dim_names, dim_names_extend, dims = dims)

  axis_meta_names <- map(arg_dim_names, get_meta_names, axis = axis)
  axis_meta_names <- reduce(axis_meta_names, coalesce_meta_dim_names)

  axis_outer_names <- names2(args)

  axis_dim_names <- map(arg_dim_names, get_axis_names, axis = axis)
  axis_dim_names <- pmap(list(axis_outer_names, axis_dim_names, axis_sizes), outer_names)
  axis_dim_names <- discard(axis_dim_names, axis_sizes == 0L)
  axis_dim_names <- combine_axis_dim_names(axis_dim_names, axis = axis)

  non_axis_dim_names <- map(arg_dim_names, delete_axis_names, axis = axis)
  non_axis_dim_names <- map(non_axis_dim_names, restore_dim_names, to_dim = dim[-axis])
  non_axis_dim_names <- reduce(non_axis_dim_names, coalesce_dim_names)

  out <- rray_expand_dim_names(non_axis_dim_names, axis)

  if (!is.null(axis_dim_names)) {
    out[[axis]] <- axis_dim_names
  }

  if (!is.null(axis_meta_names)) {
    names(out)[axis] <- axis_meta_names
  }

  out
}

combine_axis_dim_names <- function(axis_dim_names, axis) {
  axis_namedness <- map_lgl(axis_dim_names, axis_is_fully_named)
  ok <- all(axis_namedness) || !any(axis_namedness)

  if (!ok) {
    glubort("If any elements along axis {axis} are named, all must be named.")
  }

  vec_c(!!! axis_dim_names)
}

axis_is_fully_named <- function(names) {
  (!is.null(names)) && all(names != "" & !is.na(names))
}

get_meta_names <- function(x_names, axis) {
  names(x_names)[axis]
}

get_axis_names <- function(x_names, axis) {
  x_names[[axis]]
}

delete_axis_names <- function(x_names, axis) {
  x_names[-axis]
}

outer_names <- function(outer, names, n) {

  has_outer <- !is.null(outer) && !outer %in% c("", NA)

  if (!has_outer) {
    return(names)
  }

  has_inner <- !is.null(names)

  if (has_inner) {
    paste0(outer, "..", names)
  }
  else {
    if (n == 1) {
      outer
    }
    else {
      paste0(outer, seq_len(n))
    }
  }

}
