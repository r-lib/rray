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
#' # dimension names come along for the ride
#' # following rray name handling
#' rray_bind(x, y)
#'
#' rray_bind(x, y, axis = 2)
#'
#' # You can add "outer" names to the
#' # axis you are binding along
#' rray_bind(outer = x, y, axis = 2)
#'
#' # They are added to existing names with `..`
#' # and if names did not exist they are added
#' # along with a number
#' rray_bind(outer = x, outer_y = y, axis = 1)
#'
#' @export
rray_bind <- function(..., axis = 1L) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, dims = Inf)

  args <- compact(rlang::list2(...))

  if (length(args) == 0L) {
    return(NULL)
  }

  dims <- rray_dims_common(!!!args)

  # Allow for going up in dimension
  dims <- max(dims, axis)

  args <- map(args, rray_dims_match, dims)

  args_dim <- map(args, vec_dim)
  axis_ns <- map_int(args_dim, pull_axis, axis)

  args_dim <- map(args_dim, common_axis, axis)
  dim <- reduce(args_dim, rray_dim2)

  args <- map(args, partial_broadcast, dim, axis)

  # Construct container of correct type
  # (must come after the broadcast)
  out <- construct_bind_container(args, axis)

  # build the assignment expr
  assigner <- build_assigner(dims, axis)

  pos <- 1L
  for (i in seq_along(args)) {
    arg <- args[[i]]
    n <- axis_ns[i]

    # `at` controls where we update `out` at
    at <- pos:(pos + n - 1L)

    # Call the assigner using the variables
    # `at`, `out`, and `arg`
    # (so don't change the names)
    eval_bare(assigner)

    pos <- pos + n
  }

  new_dim_names <- rray_dim_names_common_along_axis(!!!args, axis = axis)

  dim_names(out) <- new_dim_names

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

common_axis <- function(dim, axis) {
  dim[axis] <- 0L
  dim
}

pull_axis <- function(dim, axis) {
  dim[axis]
}

# Broadcast everything except for the `x` dim
# corresponding to `axis`
partial_broadcast <- function(x, dim, axis) {
  dim[axis] <- vec_dim(x)[axis]
  rray_broadcast(x, dim)
}

# Useful if combining along an axis
# Assumes all of the dims beside the axis are already
# the same
rray_dim_names_common_along_axis <- function(..., axis) {

  args <- list2(...)

  args_dim <- map(args, vec_dim)
  axis_ns <- map_int(args_dim, pull_axis, axis)

  axis_dim_names <- map(args, n_dim_names, n = axis)

  # Take care of outer names
  axis_dim_names <- pmap(
    list(rlang::names2(axis_dim_names), axis_dim_names, axis_ns),
    outer_names
  )

  # Ensure any `NULL` values that might be left over are made explicit
  any_has_axis_names <- any(map_lgl(axis_dim_names, has_axis_names))
  if (any_has_axis_names) {
    axis_dim_names <- map2(axis_dim_names, axis_ns, make_explicit_dim_names)
  }

  axis_dim_names <- unlist(axis_dim_names, use.names = FALSE)

  # set `axis` dim names for each arg before reconciling
  # so the sizes are the same
  all_dim_names <- map(args, dim_names)

  if (!is.null(axis_dim_names)) {
    all_dim_names <- map(all_dim_names, function(x) {
      x[[axis]] <- axis_dim_names
      x
    })
  }

  new_dim_names <- reduce(all_dim_names, coalesce_dim_names)

  new_dim_names
}

# Turns `NULL` dim names into c("") of the correct length
make_explicit_dim_names <- function(dim_names, n) {
  dim_names %||% rep("", times = n)
}

has_axis_names <- function(dim_names) {
  !is.null(dim_names)
}

construct_bind_container <- function(args, axis) {

  # Get type, but shape and size
  # won't be right
  out_type <- vec_type_common(!!!args)

  args_dim <- map(args, vec_dim)
  axis_ns <- map_int(args_dim, pull_axis, axis)
  out_axis_size <- sum(axis_ns)

  # At this point, they all have the same
  # shape from the broadcast
  # (in every way except the axis of interest)
  out_dim <- args_dim[[1]]
  out_dim[axis] <- out_axis_size

  # Construct `shape` first...
  out_shape <- out_dim
  out_shape[1] <- 0L
  dim(out_type) <- out_shape

  out_size <- out_dim[1]

  # ...then use vec_na() to get the `size`
  out <- vctrs::vec_na(out_type, n = out_size)

  out
}

build_assigner <- function(dims, axis) {

  needs_missing <- seq_len(dims)[-axis]

  times_before <- sum(needs_missing < axis)
  times_after  <- sum(needs_missing > axis)

  before_arg <- rep(list(missing_arg()), times = times_before)
  after_arg  <- rep(list(missing_arg()), times = times_after)

  # Relies on the names used in the for loop (out, at, arg)
  expr(out[!!!before_arg, at, !!!after_arg] <- arg)
}

# ------------------------------------------------------------------------------
# names related functions stolen from vctrs

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
