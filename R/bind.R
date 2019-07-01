#' Combine many arrays together into one array
#'
#' These functions bind multiple vectors, matrices, arrays, or rrays together
#' into one, combining along the `.axis`.
#'
#' @details
#'
#' `rray_bind()` is extremely flexible. It uses broadcasting to combine
#' arrays together in a way that the native functions of `cbind()` and `rbind()`
#' cannot. See the examples section for more explanation!
#'
#' @return
#'
#' An array, or rray, depending on the input.
#'
#' @param ... Vectors, matrices, arrays, or rrays.
#'
#' @param .axis A single integer. The axis to bind along.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' a <- matrix(1:4, ncol = 2)
#' b <- matrix(5:6, ncol = 1)
#'
#' # Bind along columns
#' rray_bind(a, b, .axis = 2)
#'
#' # Bind along rows
#' # Broadcasting is done automatically
#' rray_bind(a, b, .axis = 1)
#'
#' # Notice that this is not possible with rbind()!
#' try(rbind(a, b))
#'
#' # You can bind "up" to a new dimension
#' # to stack matrices into an array
#' rray_bind(a, b, .axis = 3)
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
#' rray_bind(x, y, .axis = 2)
#'
#' # If some inputs are named, and others
#' # are not, the non-named inputs get `""`
#' # as names
#' rray_bind(x, y, .axis = 1)
#'
#' # You can add "outer" names to the
#' # axis you are binding along.
#' # They are added to existing names with `..`
#' rray_bind(outer = x, y, .axis = 2)
#'
#' # Outer names can be used to give unnamed
#' # inputs default names
#' rray_bind(outer = x, outer_y = y, .axis = 1)
#'
#' @export
rray_bind <- function(..., .axis) {
  .axis <- vec_cast(.axis, integer())
  validate_axes(.axis, x = numeric(), n = 1L, nm = ".axis", dim_n = Inf)

  args <- compact(list2(...))

  if (length(args) == 0L) {
    return(NULL)
  }

  # finalize partial types
  args <- map(args, vec_ptype_finalise)

  proxy <- vec_ptype_inner_common(!!!args)
  container <- vec_ptype_container_common(!!!args)

  res <- rray__bind(proxy, args, as_cpp_idx(.axis))

  vec_cast_container(res, container)
}

#' @rdname rray_bind
#' @export
rray_rbind <- function(...) {
  rray_bind(..., .axis = 1L)
}

#' @rdname rray_bind
#' @export
rray_cbind <- function(...) {
  rray_bind(..., .axis = 2L)
}
