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
extern r_obj* ffi_rray_col_names(r_obj* ffi_x, r_obj* ffi_frame);
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
extern r_obj* ffi_rray_set_col_names(
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
extern r_obj* ffi_rray_split_names(r_obj* ffi_x, r_obj* ffi_axes);
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
extern r_obj* ffi_rray_split(r_obj* ffi_x, r_obj* ffi_axes, r_obj* ffi_frame);
extern r_obj* ffi_rray_squeeze(r_obj* ffi_x, r_obj* ffi_axes, r_obj* ffi_frame);
extern r_obj* ffi_rray_permute_axes(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_sum_along(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_product_along(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_mean_along(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_all_along(
  r_obj* ffi_x,
  r_obj* ffi_axes,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_any_along(
  r_obj* ffi_x,
  r_obj* ffi_axes,
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
extern r_obj* ffi_rray_max(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_na_rm,
  r_obj* ffi_frame
);
extern r_obj* ffi_rray_min(
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

extern r_obj* ffi_test_wrap(r_obj* ffi_x);
extern r_obj* ffi_test_wrapper_readonly(r_obj* ffi_x);
extern r_obj* ffi_test_wrapper_writable(r_obj* ffi_x);
extern r_obj* ffi_test_is_wrapper(r_obj* ffi_x);

// Defined below
r_obj* ffi_rray4_init_library(r_obj* ffi_ns);

static const R_CallMethodDef CallEntries[] = {
  {"ffi_rray_broadcast", (DL_FUNC) &ffi_rray_broadcast, 3},
  {"ffi_rray_broadcast_common", (DL_FUNC) &ffi_rray_broadcast_common, 3},
  {"ffi_rray_size", (DL_FUNC) &ffi_rray_size, 2},
  {"ffi_rray_dimensions", (DL_FUNC) &ffi_rray_dimensions, 2},
  {"ffi_rray_dimensions_common", (DL_FUNC) &ffi_rray_dimensions_common, 3},
  {"ffi_rray_dimensionality", (DL_FUNC) &ffi_rray_dimensionality, 2},
  {"ffi_rray_names", (DL_FUNC) &ffi_rray_names, 2},
  {"ffi_rray_axis_names", (DL_FUNC) &ffi_rray_axis_names, 3},
  {"ffi_rray_row_names", (DL_FUNC) &ffi_rray_row_names, 2},
  {"ffi_rray_col_names", (DL_FUNC) &ffi_rray_col_names, 2},
  {"ffi_rray_set_names", (DL_FUNC) &ffi_rray_set_names, 3},
  {"ffi_rray_set_axis_names", (DL_FUNC) &ffi_rray_set_axis_names, 4},
  {"ffi_rray_set_row_names", (DL_FUNC) &ffi_rray_set_row_names, 3},
  {"ffi_rray_set_col_names", (DL_FUNC) &ffi_rray_set_col_names, 3},
  {"ffi_rray_broadcast_names", (DL_FUNC) &ffi_rray_broadcast_names, 2},
  {"ffi_rray_broadcast_names2", (DL_FUNC) &ffi_rray_broadcast_names2, 3},
  {"ffi_rray_broadcast_names_common",
   (DL_FUNC) &ffi_rray_broadcast_names_common,
   2},
  {"ffi_rray_reduce_names", (DL_FUNC) &ffi_rray_reduce_names, 2},
  {"ffi_rray_split_names", (DL_FUNC) &ffi_rray_split_names, 2},
  {"ffi_rray_set_dimensions", (DL_FUNC) &ffi_rray_set_dimensions, 3},
  {"ffi_rray_split", (DL_FUNC) &ffi_rray_split, 3},
  {"ffi_rray_squeeze", (DL_FUNC) &ffi_rray_squeeze, 3},
  {"ffi_rray_permute_axes", (DL_FUNC) &ffi_rray_permute_axes, 3},
  {"ffi_rray_sum_along", (DL_FUNC) &ffi_rray_sum_along, 4},
  {"ffi_rray_product_along", (DL_FUNC) &ffi_rray_product_along, 4},
  {"ffi_rray_mean_along", (DL_FUNC) &ffi_rray_mean_along, 4},
  {"ffi_rray_all_along", (DL_FUNC) &ffi_rray_all_along, 4},
  {"ffi_rray_any_along", (DL_FUNC) &ffi_rray_any_along, 4},
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
  {"ffi_rray_max", (DL_FUNC) &ffi_rray_max, 4},
  {"ffi_rray_min", (DL_FUNC) &ffi_rray_min, 4},
  {"ffi_rray_equal", (DL_FUNC) &ffi_rray_equal, 3},
  {"ffi_rray_not_equal", (DL_FUNC) &ffi_rray_not_equal, 3},
  {"ffi_rray_greater_than", (DL_FUNC) &ffi_rray_greater_than, 3},
  {"ffi_rray_greater_than_or_equal",
   (DL_FUNC) &ffi_rray_greater_than_or_equal,
   3},
  {"ffi_rray_less_than", (DL_FUNC) &ffi_rray_less_than, 3},
  {"ffi_rray_less_than_or_equal", (DL_FUNC) &ffi_rray_less_than_or_equal, 3},
  {"ffi_test_wrap", (DL_FUNC) &ffi_test_wrap, 1},
  {"ffi_test_wrapper_readonly", (DL_FUNC) &ffi_test_wrapper_readonly, 1},
  {"ffi_test_wrapper_writable", (DL_FUNC) &ffi_test_wrapper_writable, 1},
  {"ffi_test_is_wrapper", (DL_FUNC) &ffi_test_is_wrapper, 1},
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
extern void rray_init_utils(r_obj* ns);

r_obj* ffi_rray4_init_library(r_obj* ffi_ns) {
  r_init_library(ffi_ns);
  rray_init_syms(ffi_ns);
  rray_init_args(ffi_ns);
  rray_init_utils(ffi_ns);
  return r_null;
}
