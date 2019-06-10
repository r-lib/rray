# #' Reducers
# #'
# #' Apply a generic reducing function to the axes of `.x`. Reducing functions
# #' return a single value by iteratively applying a binary function. For example,
# #' [rray_sum()] is a reducing function that returns the sum along a specified
# #' axis.
# #'
# #' @param .x An array-like object.
# #' @param .f A 2-argument function. The function will be passed the accumulated
# #' value as the first argument and the "next" value as the second argument. It
# #' will be applied in this manner along each of the `axes`.
# #' @param ... Additional arguments passed to `.f`.
# #' @param axes The axes to reduce over.
# #'
# #' @examples
# #'
# #' x <- rray(1:24, c(12, 2))
# #'
# #' # Equivalent
# #' rray_reduce_int(x, sum)
# #' rray_reduce_int(x, ~.x + .y)
# #' rray_sum(x, axes = 1)
# #'
# #' # Same as above, but reduce the columns
# #' rray_reduce_int(x, sum, axes = 2)
# #' rray_reduce_int(x, ~.x + .y, axes = 2)
# #' rray_sum(x, axes = 2)
# #'
# #' # Running max of each column
# #' x_shuffled <- x[c(1:5, 10:6, 12, 11),]
# #' rray_reduce_int(x_shuffled, max)
# #'
# #' # Passing named arguments through
# #' y <- x
# #' y[1,1] <- NA
# #' rray_reduce_int(y, sum, na.rm = TRUE)
# #' rray_reduce_int(y, ~sum(.x, .y, na.rm = TRUE))
# #'
# #' @name reducers
# NULL
#
# #' @rdname reducers
# #' @export
# rray_reduce_dbl <- function(.x, .f, ..., axes = 1) {
#   reducer_impl(.x, .f, ..., axes = axes, type = "double")
# }
#
# #' @rdname reducers
# #' @export
# rray_reduce_int <- function(.x, .f, ..., axes = 1) {
#   reducer_impl(.x, .f, ..., axes = axes, type = "integer")
# }
#
# #' @rdname reducers
# #' @export
# rray_reduce_lgl <- function(.x, .f, ..., axes = 1) {
#   reducer_impl(.x, .f, ..., axes = axes, type = "logical")
# }
#
# reducer_impl <- function(.x, .f, ..., axes, type) {
#
#   .f <- rlang::as_function(.f)
#
#   # Enable passing of ...
#   # Stolen from purrr::accumulate
#   f <- function(x, y) {
#     .f(x, y, ...)
#   }
#
#   # only integer axes
#   axes <- vec_cast(axes, integer())
#   validate_axes(axes, .x)
#
#   # perform the reduction
#   res <- rray_custom_reducer_cpp(.x, f, as_cpp_idx(axes), type)
#
#   # until we get keepdims = True
#   new_dim <- rray_dim(.x)
#   new_dim[axes] <- 1L
#   res <- rray_reshape(res, new_dim)
#
#   new_dim_names <- rray_resize_dim_names(rray_dim_names(.x), new_dim)
#   res <- rray_set_dim_names(res, new_dim_names)
#
#   vec_restore(res, .x)
# }
