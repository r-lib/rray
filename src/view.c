#include "view.h"

#include <R_ext/Altrep.h>

#include "clone.h"

#include "decl/view-decl.h"

static char* package = "rray";

// Create a read-only ALTREP view of an R object's data
//
// Useful when ONLY modifying the attributes of the new object, and NEVER the
// underlying data.
//
// The ALTREP view itself is immediately marked as immutable with
// `MARK_NOT_MUTABLE()`. This is extremely important! It ensures that R won't
// ever ask for a writable `DATAPTR()` without first calling `Duplicate()`.
//
// Note that R will VERY often use `REAL()` instead of `REAL_RO()` when it only
// needs a read-only pointer. This is due to `REAL_RO()` being a newer idea that
// was only introduced with ALTREP, and a lot of historical code hasn't been
// ported over. Because of that, our `DATAPTR()` ALTREP methods must return a
// pointer even in the `writable` case, but we expect that R (and all packages)
// hold up their side of the bargain by only actually writing to this pointer
// when they fully own it or when there are `NO_REFERENCES()`, which won't be
// the case since we've used `MARK_NOT_MUTABLE()`. The `NO_REFERENCES()` case is
// relevant for `x + y + 1` where R will attempt to reuse `x + y`'s temporary
// result since there aren't any other owners. We don't want
// `rray_set_dimensions(x) + 1` to look reusable!
r_obj* r_view(r_obj* x) {
  R_altrep_class_t cls = view_class(r_typeof(x));

  // If `x` is a view, take its original data and view that instead. This
  // avoids a proliferation of nested views. Refcounts on `x` itself should also
  // still be correct - each view looks like an individual user of `x`.
  r_obj* data = is_view(x) ? view_data(x) : x;

  // Reference count on `data` is bumped when we create the view here.
  // It now looks at least "referenced", but not necessarily "shared".
  r_obj* out = KEEP(R_new_altrep(cls, data, r_null));

  // The all important immutable marking of `out`, preventing any downstream C
  // code from requesting a writable pointer without duplicating first.
  MARK_NOT_MUTABLE(out);

  FREE(1);
  return out;
}

// -----------------------------------------------------------------------------
// Class

static R_altrep_class_t view_logical_class;
static R_altrep_class_t view_integer_class;
static R_altrep_class_t view_double_class;
static R_altrep_class_t view_complex_class;
static R_altrep_class_t view_raw_class;
static R_altrep_class_t view_character_class;
static R_altrep_class_t view_list_class;

static inline bool is_view(r_obj* x) {
  if (!ALTREP(x)) {
    return false;
  }

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    return R_altrep_inherits(x, view_logical_class);
  case R_TYPE_integer:
    return R_altrep_inherits(x, view_integer_class);
  case R_TYPE_double:
    return R_altrep_inherits(x, view_double_class);
  case R_TYPE_complex:
    return R_altrep_inherits(x, view_complex_class);
  case R_TYPE_raw:
    return R_altrep_inherits(x, view_raw_class);
  case R_TYPE_character:
    return R_altrep_inherits(x, view_character_class);
  case R_TYPE_list:
    return R_altrep_inherits(x, view_list_class);
  default:
    return false;
  }
}

static inline R_altrep_class_t view_class(enum r_type type) {
  switch (type) {
  case R_TYPE_logical:
    return view_logical_class;
  case R_TYPE_integer:
    return view_integer_class;
  case R_TYPE_double:
    return view_double_class;
  case R_TYPE_complex:
    return view_complex_class;
  case R_TYPE_raw:
    return view_raw_class;
  case R_TYPE_character:
    return view_character_class;
  case R_TYPE_list:
    return view_list_class;
  default:
    r_abort("Can't create a view of a %s.", Rf_type2char(type));
  }
}

// -----------------------------------------------------------------------------
// Data

static inline r_obj* view_data(r_obj* x) {
  return R_altrep_data1(x);
}

// -----------------------------------------------------------------------------
// Generic

static r_obj* view_serialized_state(r_obj* x) {
  // Full serialization, lose ALTREP-ness
  return NULL;
}

// Powers `Rf_duplicate()` and `Rf_shallow_duplicate()`
//
// We only need to handle the data. Base R covers the attributes for us.
// https://github.com/wch/r-source/blob/trunk/src/main/altrep.c#L740
static r_obj* view_duplicate(r_obj* x, Rboolean deep) {
  if (deep) {
    return Rf_duplicate(view_data(x));
  } else {
    return r_clone_data(view_data(x));
  }
}

static Rboolean view_inspect(
  r_obj* x,
  int pre,
  int deep,
  int pvec,
  void (*inspect_subtree)(r_obj*, int, int, int)
) {
  Rprintf("<view>\n");
  inspect_subtree(view_data(x), pre, deep, pvec);
  return TRUE;
}

static r_ssize view_length(r_obj* x) {
  return r_length(view_data(x));
}

// -----------------------------------------------------------------------------
// Dataptr

static void* view_logical_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_lgl_cbegin(view_data(x));
}

static void* view_integer_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_int_cbegin(view_data(x));
}

static void* view_double_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_dbl_cbegin(view_data(x));
}

static void* view_complex_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_cpl_cbegin(view_data(x));
}

static void* view_raw_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_raw_cbegin(view_data(x));
}

static void* view_character_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    r_abort("Can't request a writable dataptr to a character view.");
  } else {
    return (void*) r_chr_cbegin(view_data(x));
  }
}

static void* view_list_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    r_abort("Can't request a writable dataptr to a list view.");
  } else {
    return (void*) r_list_cbegin(view_data(x));
  }
}

// -----------------------------------------------------------------------------
// Dataptr_or_null

static const void* view_logical_dataptr_or_null(r_obj* x) {
  return LOGICAL_OR_NULL(view_data(x));
}

static const void* view_integer_dataptr_or_null(r_obj* x) {
  return INTEGER_OR_NULL(view_data(x));
}

static const void* view_double_dataptr_or_null(r_obj* x) {
  return REAL_OR_NULL(view_data(x));
}

static const void* view_complex_dataptr_or_null(r_obj* x) {
  return COMPLEX_OR_NULL(view_data(x));
}

static const void* view_raw_dataptr_or_null(r_obj* x) {
  return RAW_OR_NULL(view_data(x));
}

static const void* view_character_dataptr_or_null(r_obj* x) {
  // There is no `STRING_PTR_RO_OR_NULL`
  return r_chr_cbegin(view_data(x));
}

static const void* view_list_dataptr_or_null(r_obj* x) {
  // There is no `VECTOR_PTR_RO_OR_NULL`
  return r_list_cbegin(view_data(x));
}

// -----------------------------------------------------------------------------
// Elt

static int view_logical_elt(r_obj* x, r_ssize i) {
  return r_lgl_get(view_data(x), i);
}

static int view_integer_elt(r_obj* x, r_ssize i) {
  return r_int_get(view_data(x), i);
}

static double view_double_elt(r_obj* x, r_ssize i) {
  return r_dbl_get(view_data(x), i);
}

static r_complex view_complex_elt(r_obj* x, r_ssize i) {
  return r_cpl_get(view_data(x), i);
}

static Rbyte view_raw_elt(r_obj* x, r_ssize i) {
  return r_raw_get(view_data(x), i);
}

static r_obj* view_character_elt(r_obj* x, r_ssize i) {
  return r_chr_get(view_data(x), i);
}

static r_obj* view_list_elt(r_obj* x, r_ssize i) {
  return r_list_get(view_data(x), i);
}

// -----------------------------------------------------------------------------
// Get_region

static r_ssize view_logical_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  int* buf
) {
  return LOGICAL_GET_REGION(view_data(x), i, n, buf);
}

static r_ssize view_integer_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  int* buf
) {
  return INTEGER_GET_REGION(view_data(x), i, n, buf);
}

static r_ssize view_double_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  double* buf
) {
  return REAL_GET_REGION(view_data(x), i, n, buf);
}

static r_ssize view_complex_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  Rcomplex* buf
) {
  return COMPLEX_GET_REGION(view_data(x), i, n, buf);
}

static r_ssize view_raw_get_region(r_obj* x, r_ssize i, r_ssize n, Rbyte* buf) {
  return RAW_GET_REGION(view_data(x), i, n, buf);
}

// -----------------------------------------------------------------------------
// Set_elt

static void view_character_set_elt(r_obj* x, r_ssize i, r_obj* v) {
  r_abort("Can't set an element of a character view.");
}

static void view_list_set_elt(r_obj* x, r_ssize i, r_obj* v) {
  r_abort("Can't set an element of a list view.");
}

// -----------------------------------------------------------------------------
// FFI test helpers

r_obj* ffi_test_view(r_obj* ffi_x) {
  return r_view(ffi_x);
}

r_obj* ffi_test_view_data(r_obj* ffi_x) {
  if (!is_view(ffi_x)) {
    r_abort("`x` must be a view.");
  }
  return view_data(ffi_x);
}

r_obj* ffi_test_is_view(r_obj* ffi_x) {
  return r_lgl(is_view(ffi_x));
}

r_obj* ffi_test_is_altrep(r_obj* ffi_x) {
  return r_lgl(ALTREP(ffi_x));
}

r_obj* ffi_test_view_pointer_is_stable(r_obj* ffi_x) {
  if (!is_view(ffi_x)) {
    r_abort("`x` must be a view.");
  }

  r_obj* data = view_data(ffi_x);

  switch (r_typeof(ffi_x)) {
  case R_TYPE_logical: {
    const int* v_read = r_lgl_cbegin(ffi_x);
    const int* v_write = r_lgl_begin(ffi_x);
    return r_lgl(v_read == v_write && v_read == r_lgl_cbegin(data));
  }
  case R_TYPE_integer: {
    const int* v_read = r_int_cbegin(ffi_x);
    const int* v_write = r_int_begin(ffi_x);
    return r_lgl(v_read == v_write && v_read == r_int_cbegin(data));
  }
  case R_TYPE_double: {
    const double* v_read = r_dbl_cbegin(ffi_x);
    const double* v_write = r_dbl_begin(ffi_x);
    return r_lgl(v_read == v_write && v_read == r_dbl_cbegin(data));
  }
  case R_TYPE_complex: {
    const r_complex* v_read = r_cpl_cbegin(ffi_x);
    const r_complex* v_write = r_cpl_begin(ffi_x);
    return r_lgl(v_read == v_write && v_read == r_cpl_cbegin(data));
  }
  case R_TYPE_raw: {
    const void* v_read = r_raw_cbegin(ffi_x);
    const void* v_write = r_raw_begin(ffi_x);
    return r_lgl(v_read == v_write && v_read == r_raw_cbegin(data));
  }
  case R_TYPE_character: {
    r_obj* const* v_read = r_chr_cbegin(ffi_x);
    r_obj* const* v_read2 = r_chr_cbegin(ffi_x);
    return r_lgl(v_read == v_read2 && v_read == r_chr_cbegin(data));
  }
  case R_TYPE_list: {
    r_obj* const* v_read = r_list_cbegin(ffi_x);
    r_obj* const* v_read2 = r_list_cbegin(ffi_x);
    return r_lgl(v_read == v_read2 && v_read == r_list_cbegin(data));
  }
  default:
    r_stop_unimplemented_type(r_typeof(ffi_x));
  }
}

r_obj* ffi_test_view_set_elt(r_obj* ffi_x) {
  switch (r_typeof(ffi_x)) {
  case R_TYPE_character:
    SET_STRING_ELT(ffi_x, 0, r_globals.na_str);
    return r_null;
  case R_TYPE_list:
    SET_VECTOR_ELT(ffi_x, 0, r_null);
    return r_null;
  default:
    r_stop_unimplemented_type(r_typeof(ffi_x));
  }
}

r_obj* ffi_test_view_read_access(r_obj* ffi_x) {
  switch (r_typeof(ffi_x)) {
  case R_TYPE_logical: {
    const int* v_x = r_lgl_cbegin(ffi_x);
    return r_lgl(v_x[0] == r_lgl_get(ffi_x, 0));
  }
  case R_TYPE_integer: {
    const int* v_x = r_int_cbegin(ffi_x);
    return r_lgl(v_x[0] == r_int_get(ffi_x, 0));
  }
  case R_TYPE_double: {
    const double* v_x = r_dbl_cbegin(ffi_x);
    return r_lgl(v_x[0] == r_dbl_get(ffi_x, 0));
  }
  case R_TYPE_complex: {
    const r_complex* v_x = r_cpl_cbegin(ffi_x);
    const r_complex elt = r_cpl_get(ffi_x, 0);
    return r_lgl(v_x[0].r == elt.r && v_x[0].i == elt.i);
  }
  default:
    r_stop_unimplemented_type(r_typeof(ffi_x));
  }
}

// -----------------------------------------------------------------------------
// Initializers

static void init_view_logical(DllInfo* dll) {
  R_altrep_class_t cls = R_make_altlogical_class("view_logical", package, dll);
  view_logical_class = cls;

  R_set_altrep_Serialized_state_method(cls, view_serialized_state);
  R_set_altrep_Duplicate_method(cls, view_duplicate);
  R_set_altrep_Inspect_method(cls, view_inspect);
  R_set_altrep_Length_method(cls, view_length);

  R_set_altvec_Dataptr_method(cls, view_logical_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, view_logical_dataptr_or_null);

  R_set_altlogical_Elt_method(cls, view_logical_elt);
  R_set_altlogical_Get_region_method(cls, view_logical_get_region);
}

static void init_view_integer(DllInfo* dll) {
  R_altrep_class_t cls = R_make_altinteger_class("view_integer", package, dll);
  view_integer_class = cls;

  R_set_altrep_Serialized_state_method(cls, view_serialized_state);
  R_set_altrep_Duplicate_method(cls, view_duplicate);
  R_set_altrep_Inspect_method(cls, view_inspect);
  R_set_altrep_Length_method(cls, view_length);

  R_set_altvec_Dataptr_method(cls, view_integer_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, view_integer_dataptr_or_null);

  R_set_altinteger_Elt_method(cls, view_integer_elt);
  R_set_altinteger_Get_region_method(cls, view_integer_get_region);
}

static void init_view_double(DllInfo* dll) {
  R_altrep_class_t cls = R_make_altreal_class("view_double", package, dll);
  view_double_class = cls;

  R_set_altrep_Serialized_state_method(cls, view_serialized_state);
  R_set_altrep_Duplicate_method(cls, view_duplicate);
  R_set_altrep_Inspect_method(cls, view_inspect);
  R_set_altrep_Length_method(cls, view_length);

  R_set_altvec_Dataptr_method(cls, view_double_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, view_double_dataptr_or_null);

  R_set_altreal_Elt_method(cls, view_double_elt);
  R_set_altreal_Get_region_method(cls, view_double_get_region);
}

static void init_view_complex(DllInfo* dll) {
  R_altrep_class_t cls = R_make_altcomplex_class("view_complex", package, dll);
  view_complex_class = cls;

  R_set_altrep_Serialized_state_method(cls, view_serialized_state);
  R_set_altrep_Duplicate_method(cls, view_duplicate);
  R_set_altrep_Inspect_method(cls, view_inspect);
  R_set_altrep_Length_method(cls, view_length);

  R_set_altvec_Dataptr_method(cls, view_complex_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, view_complex_dataptr_or_null);

  R_set_altcomplex_Elt_method(cls, view_complex_elt);
  R_set_altcomplex_Get_region_method(cls, view_complex_get_region);
}

static void init_view_raw(DllInfo* dll) {
  R_altrep_class_t cls = R_make_altraw_class("view_raw", package, dll);
  view_raw_class = cls;

  R_set_altrep_Serialized_state_method(cls, view_serialized_state);
  R_set_altrep_Duplicate_method(cls, view_duplicate);
  R_set_altrep_Inspect_method(cls, view_inspect);
  R_set_altrep_Length_method(cls, view_length);

  R_set_altvec_Dataptr_method(cls, view_raw_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, view_raw_dataptr_or_null);

  R_set_altraw_Elt_method(cls, view_raw_elt);
  R_set_altraw_Get_region_method(cls, view_raw_get_region);
}

static void init_view_character(DllInfo* dll) {
  R_altrep_class_t cls = R_make_altstring_class("view_character", package, dll);
  view_character_class = cls;

  R_set_altrep_Serialized_state_method(cls, view_serialized_state);
  R_set_altrep_Duplicate_method(cls, view_duplicate);
  R_set_altrep_Inspect_method(cls, view_inspect);
  R_set_altrep_Length_method(cls, view_length);

  R_set_altvec_Dataptr_method(cls, view_character_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, view_character_dataptr_or_null);

  R_set_altstring_Elt_method(cls, view_character_elt);
  R_set_altstring_Set_elt_method(cls, view_character_set_elt);
}

static void init_view_list(DllInfo* dll) {
  R_altrep_class_t cls = R_make_altlist_class("view_list", package, dll);
  view_list_class = cls;

  R_set_altrep_Serialized_state_method(cls, view_serialized_state);
  R_set_altrep_Duplicate_method(cls, view_duplicate);
  R_set_altrep_Inspect_method(cls, view_inspect);
  R_set_altrep_Length_method(cls, view_length);

  R_set_altvec_Dataptr_method(cls, view_list_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, view_list_dataptr_or_null);

  R_set_altlist_Elt_method(cls, view_list_elt);
  R_set_altlist_Set_elt_method(cls, view_list_set_elt);
}

void r_init_view(DllInfo* dll) {
  init_view_logical(dll);
  init_view_integer(dll);
  init_view_double(dll);
  init_view_complex(dll);
  init_view_raw(dll);
  init_view_character(dll);
  init_view_list(dll);
}
