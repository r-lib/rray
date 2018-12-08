# Not ready for this yet.

# # Can this be more generic?
# # it also doesn't seem to do the right thing....
# # row major ordering vs columns
# flatten_3D <- function(x) {
#
#   if (vec_dims(x) != 3L) {
#     abort("`x` must be 3D")
#   }
#
#   dim <- vec_dim(x)
#   flat <- reshape_impl(x, prod(dim))
#   .n_row <- dim[1] * dim[3]
#   .n_col <- dim[2]
#
#   reshape_impl(flat, c(.n_row, .n_col))
# }
#
# rray_squeeze <- function(x, axis = NULL) {
#
#   if (is.null(axis)) {
#     axis <- NA_integer_
#   }
#
#   axis <- vec_cast(axis, integer())
#
#   res <- squeeze_impl(x, axis)
#   res <- vec_restore(res, x)
#   new_dim_names <- restore_dim_names(x, vec_dim(res))
#   res <- set_full_dim_names(res, new_dim_names)
#
#   res
#
# }
#
# squeeze_impl <- function(x, axis) {
#   rray_squeeze_cpp(x, axis)
# }
