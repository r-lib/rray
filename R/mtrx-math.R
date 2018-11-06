# mtrx_math_core <- function(fun, x, ...) {
#   math_fun <- get(paste0("mtrx_", fun, "_cpp"))
#   math_fun(x)
# }
#
# vec_math.vctrs_mtrx <- function(fun, x, ...) {
#   vec_restore(mtrx_math_core(fun, x, ...), x)
# }
