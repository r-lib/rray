#' @export
reducer <- function(.x, .f, axes = 1) {

  .x <- as_rray(.x)
  .f <- rlang::as_function(.f)

  # only integer axes
  axes <- vec_cast(axes, integer())
  validate_axes(axes, vec_dims(.x))

  # perform the reduction
  res <- rray_custom_reducer_cpp(.x, .f, as_cpp_idx(axes))

  # restore the type, but not dim_names
  res <- vec_restore(res, .x)

  new_dim <- vec_dim(.x)
  new_dim[axes] <- 1L
  res <- rray_reshape(res, new_dim)

  new_dim_names <- restore_dim_names(.x, new_dim)
  res <- set_full_dim_names(res, new_dim_names)

  res
}

# as_binary_function <- function (x, env = caller_env()) {
#
#   coerce_type(
#     x,
#     friendly_type("function"),
#     primitive = ,
#
#     closure = {
#       x
#     },
#
#     formula = {
#
#       if (length(x) > 2) {
#         abort("Can't convert a two-sided formula to a function")
#       }
#       if (is_quosure(x)) {
#         eval(expr(function(...) eval_tidy(!!x)))
#       }
#       else {
#         args <- list(..., .x = quote(..1), .y = quote(..2), . = quote(..1))
#         fn <- new_function(args, f_rhs(x), f_env(x))
#         structure(fn, class = "rlang_lambda_function")
#       }
#
#     },
#     string = {
#       get(x, envir = env, mode = "function")
#     }
#   )
# }
