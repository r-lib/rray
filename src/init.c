#include <R_ext/Rdynload.h>
#include <stdlib.h>

#include "rlang.h"

extern r_obj* ffi_rray_broadcast(
  r_obj* ffi_x,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_broadcast_common(
  r_obj* ffi_xs,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_index(
  r_obj* ffi_x,
  r_obj* ffi_indices,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_as_index_array(
  r_obj* ffi_x,
  r_obj* ffi_dimension,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_extract(r_obj* ffi_x, r_obj* ffi_i, r_obj* ffi_frame);
extern r_obj* ffi_rray_extract_assign(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_value,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_as_extract_subscript(
  r_obj* ffi_i,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_slice(
  r_obj* ffi_x,
  r_obj* ffi_indices,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_slice_assign(
  r_obj* ffi_x,
  r_obj* ffi_indices,
  r_obj* ffi_value,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_slice_axis(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_axis,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_slice_rows(r_obj* ffi_x, r_obj* ffi_i, r_obj* ffi_frame);
extern r_obj* ffi_rray_slice_columns(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_slice_assign_axis(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_axis,
  r_obj* ffi_value,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_slice_assign_rows(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_value,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_slice_assign_columns(
  r_obj* ffi_x,
  r_obj* ffi_i,
  r_obj* ffi_value,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_as_slice_subscript(
  r_obj* ffi_i,
  r_obj* ffi_dimension,
  r_obj* ffi_names,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_size(r_obj* ffi_x, r_obj* ffi_frame);
extern r_obj* ffi_rray_dimensionality(r_obj* ffi_x, r_obj* ffi_frame);
extern r_obj* ffi_rray_dimensions(r_obj* ffi_x, r_obj* ffi_frame);
extern r_obj* ffi_rray_names(r_obj* ffi_x, r_obj* ffi_frame);
extern r_obj* ffi_rray_axis_names(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_row_names(r_obj* ffi_x, r_obj* ffi_frame);
extern r_obj* ffi_rray_column_names(r_obj* ffi_x, r_obj* ffi_frame);
extern r_obj* ffi_rray_set_names(
  r_obj* ffi_x,
  r_obj* ffi_names,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_set_axis_names(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_names,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_set_row_names(
  r_obj* ffi_x,
  r_obj* ffi_names,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_set_column_names(
  r_obj* ffi_x,
  r_obj* ffi_names,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_broadcast_names(r_obj* ffi_x, r_obj* ffi_dimensions);
extern r_obj* ffi_rray_broadcast_names2(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_dimensions
);
extern r_obj* ffi_rray_broadcast_names_common(
  r_obj* ffi_xs,
  r_obj* ffi_dimensions
);
extern r_obj* ffi_rray_reduce_names(r_obj* ffi_x, r_obj* ffi_axes);
extern r_obj* ffi_rray_dimensions_common(
  r_obj* ffi_xs,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_set_dimensions(
  r_obj* ffi_x,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_combine(
  r_obj* ffi_xs,
  r_obj* ffi_axis,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_split(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_stack(r_obj* ffi_xs, r_obj* ffi_axis, r_obj* ffi_frame);
extern r_obj* ffi_rray_unstack(r_obj* ffi_x, r_obj* ffi_axis, r_obj* ffi_frame);
extern r_obj* ffi_rray_rep(
  r_obj* ffi_x,
  r_obj* ffi_times,
  r_obj* ffi_axes,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_rep_each(
  r_obj* ffi_x,
  r_obj* ffi_times,
  r_obj* ffi_axis,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_roll(
  r_obj* ffi_x,
  r_obj* ffi_n,
  r_obj* ffi_axes,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_roll_each(
  r_obj* ffi_x,
  r_obj* ffi_n,
  r_obj* ffi_axis,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_reverse(r_obj* ffi_x, r_obj* ffi_axes, r_obj* ffi_frame);
extern r_obj* ffi_rray_remove_axes(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_insert_axes(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_permute_axes(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_move_axes(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_to,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_sum(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_prod(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_mean(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_all(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_any(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_max(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_min(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_locate_max(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_locate_min(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_ptype(r_obj* ffi_x, r_obj* ffi_frame);
extern r_obj* ffi_rray_ptype2(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);
extern r_obj* ffi_rray_ptype_common(
  r_obj* ffi_xs,
  r_obj* ffi_ptype,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_cast(r_obj* ffi_x, r_obj* ffi_to, r_obj* ffi_frame);
extern r_obj* ffi_rray_cast_common(
  r_obj* ffi_xs,
  r_obj* ffi_to,
  r_obj* ffi_frame
);

extern r_obj* ffi_rray_add(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);
extern r_obj* ffi_rray_divide(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);
extern r_obj* ffi_rray_exponentiate(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_multiply(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);
extern r_obj* ffi_rray_subtract(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);
extern r_obj* ffi_rray_pmax(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_pmin(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_equal(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);
extern r_obj* ffi_rray_not_equal(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);
extern r_obj* ffi_rray_greater_than(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_greater_than_or_equal(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_less_than(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);
extern r_obj* ffi_rray_less_than_or_equal(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_and(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);
extern r_obj* ffi_rray_or(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);
extern r_obj* ffi_rray_xor(r_obj* ffi_x, r_obj* ffi_y, r_obj* ffi_frame);

extern r_obj* ffi_test_wrap(r_obj* ffi_x);
extern r_obj* ffi_test_wrapper_readonly(r_obj* ffi_x);
extern r_obj* ffi_test_wrapper_writable(r_obj* ffi_x);
extern r_obj* ffi_test_is_wrapper(r_obj* ffi_x);
extern r_obj* ffi_test_wrapper_read_access(r_obj* ffi_x);
extern r_obj* ffi_test_rray_sum_forced_fallback(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_test_rray_mean_forced_fallback(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);

// Defined below
r_obj* ffi_rray4_init_library(r_obj* ffi_ns);

static const R_CallMethodDef CallEntries[] = {
  {"ffi_rray_broadcast", (DL_FUNC) &ffi_rray_broadcast, 3},
  {"ffi_rray_broadcast_common", (DL_FUNC) &ffi_rray_broadcast_common, 3},
  {"ffi_rray_index", (DL_FUNC) &ffi_rray_index, 3},
  {"ffi_rray_as_index_array", (DL_FUNC) &ffi_rray_as_index_array, 3},
  {"ffi_rray_extract", (DL_FUNC) &ffi_rray_extract, 3},
  {"ffi_rray_extract_assign", (DL_FUNC) &ffi_rray_extract_assign, 4},
  {"ffi_rray_as_extract_subscript",
   (DL_FUNC) &ffi_rray_as_extract_subscript,
   3},
  {"ffi_rray_slice", (DL_FUNC) &ffi_rray_slice, 3},
  {"ffi_rray_slice_assign", (DL_FUNC) &ffi_rray_slice_assign, 4},
  {"ffi_rray_slice_axis", (DL_FUNC) &ffi_rray_slice_axis, 4},
  {"ffi_rray_slice_rows", (DL_FUNC) &ffi_rray_slice_rows, 3},
  {"ffi_rray_slice_columns", (DL_FUNC) &ffi_rray_slice_columns, 3},
  {"ffi_rray_slice_assign_axis", (DL_FUNC) &ffi_rray_slice_assign_axis, 5},
  {"ffi_rray_slice_assign_rows", (DL_FUNC) &ffi_rray_slice_assign_rows, 4},
  {"ffi_rray_slice_assign_columns",
   (DL_FUNC) &ffi_rray_slice_assign_columns,
   4},
  {"ffi_rray_as_slice_subscript", (DL_FUNC) &ffi_rray_as_slice_subscript, 4},
  {"ffi_rray_size", (DL_FUNC) &ffi_rray_size, 2},
  {"ffi_rray_dimensions", (DL_FUNC) &ffi_rray_dimensions, 2},
  {"ffi_rray_dimensions_common", (DL_FUNC) &ffi_rray_dimensions_common, 3},
  {"ffi_rray_dimensionality", (DL_FUNC) &ffi_rray_dimensionality, 2},
  {"ffi_rray_names", (DL_FUNC) &ffi_rray_names, 2},
  {"ffi_rray_axis_names", (DL_FUNC) &ffi_rray_axis_names, 3},
  {"ffi_rray_row_names", (DL_FUNC) &ffi_rray_row_names, 2},
  {"ffi_rray_column_names", (DL_FUNC) &ffi_rray_column_names, 2},
  {"ffi_rray_set_names", (DL_FUNC) &ffi_rray_set_names, 3},
  {"ffi_rray_set_axis_names", (DL_FUNC) &ffi_rray_set_axis_names, 4},
  {"ffi_rray_set_row_names", (DL_FUNC) &ffi_rray_set_row_names, 3},
  {"ffi_rray_set_column_names", (DL_FUNC) &ffi_rray_set_column_names, 3},
  {"ffi_rray_broadcast_names", (DL_FUNC) &ffi_rray_broadcast_names, 2},
  {"ffi_rray_broadcast_names2", (DL_FUNC) &ffi_rray_broadcast_names2, 3},
  {"ffi_rray_broadcast_names_common",
   (DL_FUNC) &ffi_rray_broadcast_names_common,
   2},
  {"ffi_rray_reduce_names", (DL_FUNC) &ffi_rray_reduce_names, 2},
  {"ffi_rray_set_dimensions", (DL_FUNC) &ffi_rray_set_dimensions, 3},
  {"ffi_rray_combine", (DL_FUNC) &ffi_rray_combine, 3},
  {"ffi_rray_split", (DL_FUNC) &ffi_rray_split, 4},
  {"ffi_rray_stack", (DL_FUNC) &ffi_rray_stack, 3},
  {"ffi_rray_unstack", (DL_FUNC) &ffi_rray_unstack, 3},
  {"ffi_rray_rep", (DL_FUNC) &ffi_rray_rep, 4},
  {"ffi_rray_rep_each", (DL_FUNC) &ffi_rray_rep_each, 4},
  {"ffi_rray_roll", (DL_FUNC) &ffi_rray_roll, 4},
  {"ffi_rray_roll_each", (DL_FUNC) &ffi_rray_roll_each, 4},
  {"ffi_rray_reverse", (DL_FUNC) &ffi_rray_reverse, 3},
  {"ffi_rray_remove_axes", (DL_FUNC) &ffi_rray_remove_axes, 3},
  {"ffi_rray_insert_axes", (DL_FUNC) &ffi_rray_insert_axes, 3},
  {"ffi_rray_permute_axes", (DL_FUNC) &ffi_rray_permute_axes, 3},
  {"ffi_rray_move_axes", (DL_FUNC) &ffi_rray_move_axes, 4},
  {"ffi_rray_sum", (DL_FUNC) &ffi_rray_sum, 4},
  {"ffi_rray_prod", (DL_FUNC) &ffi_rray_prod, 4},
  {"ffi_rray_mean", (DL_FUNC) &ffi_rray_mean, 4},
  {"ffi_rray_all", (DL_FUNC) &ffi_rray_all, 4},
  {"ffi_rray_any", (DL_FUNC) &ffi_rray_any, 4},
  {"ffi_rray_max", (DL_FUNC) &ffi_rray_max, 4},
  {"ffi_rray_min", (DL_FUNC) &ffi_rray_min, 4},
  {"ffi_rray_locate_max", (DL_FUNC) &ffi_rray_locate_max, 4},
  {"ffi_rray_locate_min", (DL_FUNC) &ffi_rray_locate_min, 4},
  {"ffi_rray_ptype", (DL_FUNC) &ffi_rray_ptype, 2},
  {"ffi_rray_ptype2", (DL_FUNC) &ffi_rray_ptype2, 3},
  {"ffi_rray_ptype_common", (DL_FUNC) &ffi_rray_ptype_common, 3},
  {"ffi_rray_cast", (DL_FUNC) &ffi_rray_cast, 3},
  {"ffi_rray_cast_common", (DL_FUNC) &ffi_rray_cast_common, 3},
  {"ffi_rray_add", (DL_FUNC) &ffi_rray_add, 3},
  {"ffi_rray_divide", (DL_FUNC) &ffi_rray_divide, 3},
  {"ffi_rray_exponentiate", (DL_FUNC) &ffi_rray_exponentiate, 3},
  {"ffi_rray_multiply", (DL_FUNC) &ffi_rray_multiply, 3},
  {"ffi_rray_subtract", (DL_FUNC) &ffi_rray_subtract, 3},
  {"ffi_rray_pmax", (DL_FUNC) &ffi_rray_pmax, 4},
  {"ffi_rray_pmin", (DL_FUNC) &ffi_rray_pmin, 4},
  {"ffi_rray_equal", (DL_FUNC) &ffi_rray_equal, 3},
  {"ffi_rray_not_equal", (DL_FUNC) &ffi_rray_not_equal, 3},
  {"ffi_rray_greater_than", (DL_FUNC) &ffi_rray_greater_than, 3},
  {"ffi_rray_greater_than_or_equal",
   (DL_FUNC) &ffi_rray_greater_than_or_equal,
   3},
  {"ffi_rray_less_than", (DL_FUNC) &ffi_rray_less_than, 3},
  {"ffi_rray_less_than_or_equal", (DL_FUNC) &ffi_rray_less_than_or_equal, 3},
  {"ffi_rray_and", (DL_FUNC) &ffi_rray_and, 3},
  {"ffi_rray_or", (DL_FUNC) &ffi_rray_or, 3},
  {"ffi_rray_xor", (DL_FUNC) &ffi_rray_xor, 3},
  {"ffi_test_wrap", (DL_FUNC) &ffi_test_wrap, 1},
  {"ffi_test_wrapper_readonly", (DL_FUNC) &ffi_test_wrapper_readonly, 1},
  {"ffi_test_wrapper_writable", (DL_FUNC) &ffi_test_wrapper_writable, 1},
  {"ffi_test_is_wrapper", (DL_FUNC) &ffi_test_is_wrapper, 1},
  {"ffi_test_wrapper_read_access", (DL_FUNC) &ffi_test_wrapper_read_access, 1},
  {"ffi_test_rray_sum_forced_fallback",
   (DL_FUNC) &ffi_test_rray_sum_forced_fallback,
   4},
  {"ffi_test_rray_mean_forced_fallback",
   (DL_FUNC) &ffi_test_rray_mean_forced_fallback,
   4},
  {"ffi_rray4_init_library", (DL_FUNC) &ffi_rray4_init_library, 1},
  {NULL, NULL, 0}
};

extern void r_init_wrapper(DllInfo* dll);

void R_init_rray4(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  r_init_wrapper(dll);
}

extern void rray_init_args(r_obj* ns);
extern void rray_init_syms(r_obj* ns);

r_obj* ffi_rray4_init_library(r_obj* ffi_ns) {
  r_init_library(ffi_ns);
  rray_init_syms(ffi_ns);
  rray_init_args(ffi_ns);
  return r_null;
}
