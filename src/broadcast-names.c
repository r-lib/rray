#include "broadcast-names.h"

#include "dimensionality.h"
#include "dimensions.h"
#include "names.h"

#include "decl/broadcast-names-decl.h"

static const bool V_IGNORED_FALSE[RRAY_MAX_DIMENSIONALITY] = {false};

r_obj* ffi_rray_broadcast_names(r_obj* ffi_x, r_obj* ffi_dimensions) {
  return rray_broadcast_names(ffi_x, ffi_dimensions);
}

r_obj* rray_broadcast_names(r_obj* x, r_obj* dimensions) {
  r_ssize v_out_args[RRAY_MAX_DIMENSIONALITY];
  r_ssize out_i = 0;

  return rray_broadcast_names_merge(
    r_null,
    v_out_args,
    V_IGNORED_FALSE,
    &out_i,
    x,
    0,
    dimensions,
    rray_args.x,
    rray_args.x,
    r_lazy_null
  );
}

r_obj* ffi_rray_broadcast_names2(
  r_obj* ffi_x,
  r_obj* ffi_y,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_broadcast_names2(
    ffi_x,
    ffi_y,
    ffi_dimensions,
    rray_args.x,
    rray_args.y,
    error_call
  );
}

r_obj* rray_broadcast_names2(
  r_obj* x,
  r_obj* y,
  r_obj* dimensions,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  r_ssize v_out_args[RRAY_MAX_DIMENSIONALITY];
  r_ssize out_i = 0;

  r_obj* out = KEEP(rray_broadcast_names_merge(
    r_null,
    v_out_args,
    V_IGNORED_FALSE,
    &out_i,
    x,
    0,
    dimensions,
    x_arg,
    x_arg,
    error_call
  ));

  out = rray_broadcast_names_merge(
    out,
    v_out_args,
    V_IGNORED_FALSE,
    &out_i,
    y,
    1,
    dimensions,
    x_arg,
    y_arg,
    error_call
  );

  FREE(1);
  return out;
}

r_obj* ffi_rray_broadcast_names_common(
  r_obj* ffi_xs,
  r_obj* ffi_dimensions,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_broadcast_names_common_opts(
    ffi_xs,
    ffi_dimensions,
    NULL,
    0,
    rray_args.empty,
    error_call
  );
}

r_obj* rray_broadcast_names_common_opts(
  r_obj* xs,
  r_obj* dimensions,
  const int* v_ignore,
  r_ssize ignore_size,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  const r_ssize n = r_length(xs);
  r_obj* const* v_xs = r_list_cbegin(xs);
  r_obj* xs_names = KEEP(r_names(xs));

  r_ssize x_i = 0;
  struct rray_arg* x_arg = new_subscript_arg(arg, xs_names, n, &x_i);
  KEEP(x_arg->shelter);

  r_ssize out_i = 0;
  struct rray_arg* out_arg = new_subscript_arg(arg, xs_names, n, &out_i);
  KEEP(out_arg->shelter);

  r_ssize v_out_args[RRAY_MAX_DIMENSIONALITY];

  bool v_ignored[RRAY_MAX_DIMENSIONALITY];
  r_memset(v_ignored, 0, sizeof(bool) * RRAY_MAX_DIMENSIONALITY);

  for (r_ssize i = 0; i < ignore_size; ++i) {
    v_ignored[v_ignore[i] - 1] = true;
  }

  r_obj* out = r_null;
  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (; x_i < n; ++x_i) {
    out = rray_broadcast_names_merge(
      out,
      v_out_args,
      v_ignored,
      &out_i,
      v_xs[x_i],
      x_i,
      dimensions,
      out_arg,
      x_arg,
      error_call
    );
    KEEP_AT(out, out_loc);
  }

  FREE(4);
  return out;
}

// Broadcasting of array names
//
// Assumes the inputs are validated arrays and are broadcastable to the
// `dimensions`. The caller typically checks all of this already.
static r_obj* rray_broadcast_names_merge(
  r_obj* out,
  r_ssize* v_out_args,
  const bool* v_ignored,
  r_ssize* p_out_i,
  r_obj* x,
  r_ssize x_i,
  r_obj* dimensions,
  struct rray_arg* out_arg,
  struct rray_arg* x_arg,
  struct r_lazy error_call
) {
  r_obj* x_names = rray_names(x, x_arg, error_call);
  if (x_names == r_null) {
    return out;
  }
  KEEP(x_names);

  r_obj* const* v_x_names = r_list_cbegin(x_names);

  r_obj* x_dimensions = KEEP(rray_dimensions(x, x_arg, error_call));
  const int* v_x_dimensions = r_int_cbegin(x_dimensions);
  const int x_dimensionality =
    rray_dimensionality_from_dimensions(x_dimensions);

  const int* v_dimensions = r_int_cbegin(dimensions);
  const int dimensionality = rray_dimensionality_from_dimensions(dimensions);

  r_keep_loc out_loc;
  KEEP_HERE(out, &out_loc);

  for (int i = 0; i < x_dimensionality; ++i) {
    if (v_ignored[i]) {
      continue;
    }
    if (v_x_names[i] == r_null) {
      // No names to contribute on this axis
      continue;
    }
    if (v_x_dimensions[i] != v_dimensions[i]) {
      // No names to contribute when axis is broadcast
      continue;
    }
    if (out == r_null) {
      out = r_alloc_list(dimensionality);
      KEEP_AT(out, out_loc);
    }

    r_obj* out_names = r_list_get(out, i);

    if (out_names == r_null) {
      r_list_poke(out, i, v_x_names[i]);
      v_out_args[i] = x_i;
      continue;
    }

    *p_out_i = v_out_args[i];

    check_axis_names_equal(
      out_names,
      v_x_names[i],
      i + 1,
      out_arg,
      x_arg,
      error_call
    );
  }

  FREE(3);
  return out;
}

static void check_axis_names_equal(
  r_obj* x_names,
  r_obj* y_names,
  int axis,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  const r_ssize size = r_length(x_names);
  r_obj* const* v_x_names = r_chr_cbegin(x_names);
  r_obj* const* v_y_names = r_chr_cbegin(y_names);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_x_names[i] != v_y_names[i]) {
      stop_incompatible_names(
        v_x_names[i],
        v_y_names[i],
        axis,
        i + 1,
        x_arg,
        y_arg,
        error_call
      );
    }
  }
}

static r_no_return void stop_incompatible_names(
  r_obj* x_name,
  r_obj* y_name,
  int axis,
  r_ssize position,
  struct rray_arg* x_arg,
  struct rray_arg* y_arg,
  struct r_lazy error_call
) {
  const char* x_quote = (x_name == r_globals.na_str) ? "" : "\"";
  const char* y_quote = (y_name == r_globals.na_str) ? "" : "\"";

  r_abort_lazy_call(
    error_call,
    "Can't find common names at axis %d. "
    "%s and %s have different names at position %" R_PRI_SSIZE ": "
    "%s%s%s and %s%s%s.",
    axis,
    rray_arg_format(x_arg),
    rray_arg_format(y_arg),
    position,
    x_quote,
    r_str_c_string(x_name),
    x_quote,
    y_quote,
    r_str_c_string(y_name),
    y_quote
  );
}
