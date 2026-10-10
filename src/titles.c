#include "titles.h"

#include "axes.h"
#include "dimensionality.h"
#include "names.h"
#include "utils.h"
#include "wrapper.h"

#include "decl/titles-decl.h"

r_obj* ffi_rray_titles(r_obj* ffi_x, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_titles(ffi_x, rray_args.x, error_call);
}

r_obj* rray_titles(r_obj* x, struct rray_arg* arg, struct r_lazy error_call) {
  r_obj* names = KEEP(rray_names(x, arg, error_call));
  r_obj* out = names == r_null ? r_null : r_names(names);
  FREE(1);
  return out;
}

r_obj* ffi_rray_axis_title(r_obj* ffi_x, r_obj* ffi_axis, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_axis_title(ffi_x, axis, rray_args.x, error_call);
}

r_obj* rray_axis_title(
  r_obj* x,
  int axis,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  const int dimensionality = rray_dimensionality(x, arg, error_call);
  check_axis(axis, dimensionality, rray_args.axis, error_call);

  r_obj* titles = KEEP(rray_titles(x, arg, error_call));
  r_obj* out =
    titles == r_null ? r_null : r_str_as_character(r_chr_get(titles, axis - 1));

  FREE(2);
  return out;
}

r_obj* ffi_rray_set_titles(r_obj* ffi_x, r_obj* ffi_titles, r_obj* ffi_frame) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  return rray_set_titles(ffi_x, ffi_titles, rray_args.x, error_call);
}

r_obj* rray_set_titles(
  r_obj* x,
  r_obj* titles,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  titles = KEEP(titles);
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  const int dimensionality = rray_dimensionality(x, arg, error_call);

  if (titles != r_null) {
    if (r_typeof(titles) != R_TYPE_character) {
      r_abort_lazy_call(
        error_call,
        "%s must be a character vector or `NULL`, not %s.",
        rray_arg_format(rray_args.titles),
        r_obj_type_friendly(titles)
      );
    }

    if (r_length(titles) != dimensionality) {
      r_abort_lazy_call(
        error_call,
        "%s must have length %d to match the dimensionality of %s, "
        "not length %" R_PRI_SSIZE ".",
        rray_arg_format(rray_args.titles),
        dimensionality,
        rray_arg_format(arg),
        r_length(titles)
      );
    }

    if (!rray_titles_have_any(titles)) {
      titles = r_null;
    }
  }

  r_obj* old_names = r_dim_names(x);

  if (old_names == r_null && titles == r_null) {
    FREE(2);
    return x;
  }

  r_obj* new_names = KEEP(r_alloc_list(dimensionality));

  if (old_names != r_null) {
    r_obj* const* v_old_names = r_list_cbegin(old_names);
    for (int i = 0; i < dimensionality; ++i) {
      r_list_poke(new_names, i, v_old_names[i]);
    }
  }

  if (titles != r_null) {
    r_attrib_poke_names(new_names, titles);
  }

  r_obj* out = KEEP(r_wrap(x));
  r_attrib_poke_dim_names(out, new_names);

  FREE(4);
  return out;
}

r_obj* ffi_rray_set_axis_title(
  r_obj* ffi_x,
  r_obj* ffi_axis,
  r_obj* ffi_title,
  r_obj* ffi_frame
) {
  struct r_lazy error_call = {.x = ffi_frame, .env = r_null};
  const int axis = arg_as_int(ffi_axis, rray_args.axis, error_call);
  return rray_set_axis_title(ffi_x, axis, ffi_title, rray_args.x, error_call);
}

r_obj* rray_set_axis_title(
  r_obj* x,
  int axis,
  r_obj* title,
  struct rray_arg* arg,
  struct r_lazy error_call
) {
  title = KEEP(title);
  check_unclassed(x, arg, error_call);
  x = KEEP(arg_as_array(x, arg, error_call));

  const int dimensionality = rray_dimensionality(x, arg, error_call);
  check_axis(axis, dimensionality, rray_args.axis, error_call);

  if (title != r_null) {
    if (r_typeof(title) != R_TYPE_character) {
      r_abort_lazy_call(
        error_call,
        "%s must be a character vector or `NULL`, not %s.",
        rray_arg_format(rray_args.title),
        r_obj_type_friendly(title)
      );
    }

    if (r_length(title) != 1) {
      r_abort_lazy_call(
        error_call,
        "%s must have length 1, not length %" R_PRI_SSIZE ".",
        rray_arg_format(rray_args.title),
        r_length(title)
      );
    }
  }

  r_obj* names = r_dim_names(x);
  r_obj* old_titles = names == r_null ? r_null : r_names(names);

  if (
    old_titles == r_null &&
    (title == r_null || r_chr_get(title, 0) == r_strs.empty)
  ) {
    FREE(2);
    return x;
  }

  r_obj* new_titles = KEEP(r_alloc_character(dimensionality));
  r_obj* const* v_old_titles =
    old_titles == r_null ? NULL : r_chr_cbegin(old_titles);

  for (int i = 0; i < dimensionality; ++i) {
    r_chr_poke(
      new_titles,
      i,
      old_titles == r_null ? r_strs.empty : v_old_titles[i]
    );
  }

  r_chr_poke(
    new_titles,
    axis - 1,
    title == r_null ? r_strs.empty : r_chr_get(title, 0)
  );

  r_obj* out = KEEP(rray_set_titles(x, new_titles, arg, error_call));

  FREE(4);
  return out;
}

static bool rray_titles_have_any(r_obj* titles) {
  r_obj* const* v_titles = r_chr_cbegin(titles);
  const r_ssize size = r_length(titles);

  for (r_ssize i = 0; i < size; ++i) {
    if (v_titles[i] != r_strs.empty) {
      return true;
    }
  }

  return false;
}
