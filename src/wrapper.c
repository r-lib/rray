#include "wrapper.h"

#include <R_ext/Altrep.h>

#include "clone.h"

#include "decl/wrapper-decl.h"

static char* package = "rray";

// Wrap an R object in a lightweight ALTREP wrapper
//
// Useful when only modifying the attributes of an object,
// without touching the underlying data.
//
// - Read only accesses to the data are "passed through" to the wrapped object
// - Writable access forces a shallow duplication of the wrapped object if it
//   EVER looks shared, which happens if we are wrapping a basic R object
//   supplied from the R side by a user, or if we rewrap a wrapper's underlying
//   data (because the original wrapper also shares it).
r_obj* r_wrap(r_obj* x) {
  R_altrep_class_t cls = wrapper_class(r_typeof(x));

  // If `x` is a wrapper, unwrap it to gain access to the underlying data. This
  // is what we wrap and bump the reference count of. This avoids a
  // proliferation of wrappers. We manage writable access to the underlying data
  // by always cloning it before giving out a writable handle if it is ever
  // `MAYBE_SHARED()` (i.e. a ref count >1), meaning that we aren't the sole
  // owner of it at that time.
  r_obj* data = KEEP(wrapper_data_from(x));

  // Reference count on `data` is bumped when we wrap it here.
  // It now looks at least "referenced", but not necessarily "shared".
  r_obj* out = KEEP(R_new_altrep(cls, data, r_null));

  // Creates a fresh attribute pairlist!
  // Note that we always use `x` here, not `data`.
  r_attrib_clone_from(out, x);

  MARK_NOT_MUTABLE(out);

  FREE(2);
  return out;
}

// -----------------------------------------------------------------------------
// Class

static R_altrep_class_t wrapper_logical_class;
static R_altrep_class_t wrapper_integer_class;
static R_altrep_class_t wrapper_double_class;
static R_altrep_class_t wrapper_complex_class;
static R_altrep_class_t wrapper_raw_class;
static R_altrep_class_t wrapper_character_class;
static R_altrep_class_t wrapper_list_class;

static inline bool is_wrapper(r_obj* x) {
  if (!ALTREP(x)) {
    return false;
  }

  switch (r_typeof(x)) {
  case R_TYPE_logical:
    return R_altrep_inherits(x, wrapper_logical_class);
  case R_TYPE_integer:
    return R_altrep_inherits(x, wrapper_integer_class);
  case R_TYPE_double:
    return R_altrep_inherits(x, wrapper_double_class);
  case R_TYPE_complex:
    return R_altrep_inherits(x, wrapper_complex_class);
  case R_TYPE_raw:
    return R_altrep_inherits(x, wrapper_raw_class);
  case R_TYPE_character:
    return R_altrep_inherits(x, wrapper_character_class);
  case R_TYPE_list:
    return R_altrep_inherits(x, wrapper_list_class);
  default:
    return false;
  }
}

static inline R_altrep_class_t wrapper_class(enum r_type type) {
  switch (type) {
  case R_TYPE_logical:
    return wrapper_logical_class;
  case R_TYPE_integer:
    return wrapper_integer_class;
  case R_TYPE_double:
    return wrapper_double_class;
  case R_TYPE_complex:
    return wrapper_complex_class;
  case R_TYPE_raw:
    return wrapper_raw_class;
  case R_TYPE_character:
    return wrapper_character_class;
  case R_TYPE_list:
    return wrapper_list_class;
  default:
    r_abort("Can't wrap a %s.", Rf_type2char(type));
  }
}

// -----------------------------------------------------------------------------
// Data

static inline r_obj* wrapper_data(r_obj* x) {
  return R_altrep_data1(x);
}

static r_obj* wrapper_data_from(r_obj* x) {
  if (is_wrapper(x)) {
    return wrapper_data(x);
  }

  if (ALTREP(x)) {
    return r_clone_data(x);
  }

  return x;
}

// -----------------------------------------------------------------------------
// Generic

static r_obj* wrapper_serialized_state(r_obj* x) {
  // Full serialization, lose ALTREP-ness
  return NULL;
}

// Powers `Rf_duplicate()` and `Rf_shallow_duplicate()`
static r_obj* wrapper_duplicate(r_obj* x, Rboolean deep) {
  if (deep) {
    // Wrapper only guarantees shallow ownership.
    // No need to return another wrapper if we fully duplicate.
    return Rf_duplicate(wrapper_data(x));
  } else {
    return r_clone_data(wrapper_data(x));
  }
}

static Rboolean wrapper_inspect(
  r_obj* x,
  int pre,
  int deep,
  int pvec,
  void (*inspect_subtree)(r_obj*, int, int, int)
) {
  Rprintf("<wrapper>\n");
  inspect_subtree(wrapper_data(x), pre, deep, pvec);
  return TRUE;
}

static r_ssize wrapper_length(r_obj* x) {
  return r_length(wrapper_data(x));
}

// -----------------------------------------------------------------------------
// Dataptr

static void* wrapper_logical_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_lgl_cbegin(wrapper_data(x));
}

static void* wrapper_integer_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_int_cbegin(wrapper_data(x));
}

static void* wrapper_double_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_dbl_cbegin(wrapper_data(x));
}

static void* wrapper_complex_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_cpl_cbegin(wrapper_data(x));
}

static void* wrapper_raw_dataptr(r_obj* x, Rboolean writeable) {
  return (void*) r_raw_cbegin(wrapper_data(x));
}

static void* wrapper_character_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    r_abort("Can't request a writable dataptr to a character wrapper.");
  } else {
    return (void*) r_chr_cbegin(wrapper_data(x));
  }
}

static void* wrapper_list_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    r_abort("Can't request a writable dataptr to a list wrapper.");
  } else {
    return (void*) r_list_cbegin(wrapper_data(x));
  }
}

// -----------------------------------------------------------------------------
// Dataptr_or_null

static const void* wrapper_logical_dataptr_or_null(r_obj* x) {
  return LOGICAL_OR_NULL(wrapper_data(x));
}

static const void* wrapper_integer_dataptr_or_null(r_obj* x) {
  return INTEGER_OR_NULL(wrapper_data(x));
}

static const void* wrapper_double_dataptr_or_null(r_obj* x) {
  return REAL_OR_NULL(wrapper_data(x));
}

static const void* wrapper_complex_dataptr_or_null(r_obj* x) {
  return COMPLEX_OR_NULL(wrapper_data(x));
}

static const void* wrapper_raw_dataptr_or_null(r_obj* x) {
  return RAW_OR_NULL(wrapper_data(x));
}

static const void* wrapper_character_dataptr_or_null(r_obj* x) {
  // There is no `STRING_PTR_RO_OR_NULL`
  return r_chr_cbegin(wrapper_data(x));
}

static const void* wrapper_list_dataptr_or_null(r_obj* x) {
  // There is no `VECTOR_PTR_RO_OR_NULL`
  return r_list_cbegin(wrapper_data(x));
}

// -----------------------------------------------------------------------------
// Elt

static int wrapper_logical_elt(r_obj* x, r_ssize i) {
  return r_lgl_get(wrapper_data(x), i);
}

static int wrapper_integer_elt(r_obj* x, r_ssize i) {
  return r_int_get(wrapper_data(x), i);
}

static double wrapper_double_elt(r_obj* x, r_ssize i) {
  return r_dbl_get(wrapper_data(x), i);
}

static r_complex wrapper_complex_elt(r_obj* x, r_ssize i) {
  return r_cpl_get(wrapper_data(x), i);
}

static Rbyte wrapper_raw_elt(r_obj* x, r_ssize i) {
  return r_raw_get(wrapper_data(x), i);
}

static r_obj* wrapper_character_elt(r_obj* x, r_ssize i) {
  return r_chr_get(wrapper_data(x), i);
}

static r_obj* wrapper_list_elt(r_obj* x, r_ssize i) {
  return r_list_get(wrapper_data(x), i);
}

// -----------------------------------------------------------------------------
// Get_region

static r_ssize wrapper_logical_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  int* buf
) {
  return LOGICAL_GET_REGION(wrapper_data(x), i, n, buf);
}

static r_ssize wrapper_integer_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  int* buf
) {
  return INTEGER_GET_REGION(wrapper_data(x), i, n, buf);
}

static r_ssize wrapper_double_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  double* buf
) {
  return REAL_GET_REGION(wrapper_data(x), i, n, buf);
}

static r_ssize wrapper_complex_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  Rcomplex* buf
) {
  return COMPLEX_GET_REGION(wrapper_data(x), i, n, buf);
}

static r_ssize wrapper_raw_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  Rbyte* buf
) {
  return RAW_GET_REGION(wrapper_data(x), i, n, buf);
}

// -----------------------------------------------------------------------------
// Set_elt

static void wrapper_character_set_elt(r_obj* x, r_ssize i, r_obj* v) {
  r_abort("Can't set an element of a character wrapper.");
}

static void wrapper_list_set_elt(r_obj* x, r_ssize i, r_obj* v) {
  r_abort("Can't set an element of a list wrapper.");
}

// -----------------------------------------------------------------------------
// FFI test helpers

r_obj* ffi_test_wrap(r_obj* ffi_x) {
  return r_wrap(ffi_x);
}

r_obj* ffi_test_wrapper_data(r_obj* ffi_x) {
  if (!is_wrapper(ffi_x)) {
    r_abort("`x` must be a wrapper.");
  }
  return wrapper_data(ffi_x);
}

r_obj* ffi_test_is_wrapper(r_obj* ffi_x) {
  return r_lgl(is_wrapper(ffi_x));
}

r_obj* ffi_test_is_altrep(r_obj* ffi_x) {
  return r_lgl(ALTREP(ffi_x));
}

r_obj* ffi_test_wrapper_pointer_is_stable(r_obj* ffi_x) {
  if (!is_wrapper(ffi_x)) {
    r_abort("`x` must be a wrapper.");
  }

  r_obj* data = wrapper_data(ffi_x);

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

r_obj* ffi_test_wrapper_set_elt(r_obj* ffi_x) {
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

r_obj* ffi_test_wrapper_read_access(r_obj* ffi_x) {
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

static void init_wrapper_logical(DllInfo* dll) {
  R_altrep_class_t cls =
    R_make_altlogical_class("wrapper_logical", package, dll);
  wrapper_logical_class = cls;

  R_set_altrep_Serialized_state_method(cls, wrapper_serialized_state);
  R_set_altrep_Duplicate_method(cls, wrapper_duplicate);
  R_set_altrep_Inspect_method(cls, wrapper_inspect);
  R_set_altrep_Length_method(cls, wrapper_length);

  R_set_altvec_Dataptr_method(cls, wrapper_logical_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, wrapper_logical_dataptr_or_null);

  R_set_altlogical_Elt_method(cls, wrapper_logical_elt);
  R_set_altlogical_Get_region_method(cls, wrapper_logical_get_region);
}

static void init_wrapper_integer(DllInfo* dll) {
  R_altrep_class_t cls =
    R_make_altinteger_class("wrapper_integer", package, dll);
  wrapper_integer_class = cls;

  R_set_altrep_Serialized_state_method(cls, wrapper_serialized_state);
  R_set_altrep_Duplicate_method(cls, wrapper_duplicate);
  R_set_altrep_Inspect_method(cls, wrapper_inspect);
  R_set_altrep_Length_method(cls, wrapper_length);

  R_set_altvec_Dataptr_method(cls, wrapper_integer_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, wrapper_integer_dataptr_or_null);

  R_set_altinteger_Elt_method(cls, wrapper_integer_elt);
  R_set_altinteger_Get_region_method(cls, wrapper_integer_get_region);
}

static void init_wrapper_double(DllInfo* dll) {
  R_altrep_class_t cls = R_make_altreal_class("wrapper_double", package, dll);
  wrapper_double_class = cls;

  R_set_altrep_Serialized_state_method(cls, wrapper_serialized_state);
  R_set_altrep_Duplicate_method(cls, wrapper_duplicate);
  R_set_altrep_Inspect_method(cls, wrapper_inspect);
  R_set_altrep_Length_method(cls, wrapper_length);

  R_set_altvec_Dataptr_method(cls, wrapper_double_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, wrapper_double_dataptr_or_null);

  R_set_altreal_Elt_method(cls, wrapper_double_elt);
  R_set_altreal_Get_region_method(cls, wrapper_double_get_region);
}

static void init_wrapper_complex(DllInfo* dll) {
  R_altrep_class_t cls =
    R_make_altcomplex_class("wrapper_complex", package, dll);
  wrapper_complex_class = cls;

  R_set_altrep_Serialized_state_method(cls, wrapper_serialized_state);
  R_set_altrep_Duplicate_method(cls, wrapper_duplicate);
  R_set_altrep_Inspect_method(cls, wrapper_inspect);
  R_set_altrep_Length_method(cls, wrapper_length);

  R_set_altvec_Dataptr_method(cls, wrapper_complex_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, wrapper_complex_dataptr_or_null);

  R_set_altcomplex_Elt_method(cls, wrapper_complex_elt);
  R_set_altcomplex_Get_region_method(cls, wrapper_complex_get_region);
}

static void init_wrapper_raw(DllInfo* dll) {
  R_altrep_class_t cls = R_make_altraw_class("wrapper_raw", package, dll);
  wrapper_raw_class = cls;

  R_set_altrep_Serialized_state_method(cls, wrapper_serialized_state);
  R_set_altrep_Duplicate_method(cls, wrapper_duplicate);
  R_set_altrep_Inspect_method(cls, wrapper_inspect);
  R_set_altrep_Length_method(cls, wrapper_length);

  R_set_altvec_Dataptr_method(cls, wrapper_raw_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, wrapper_raw_dataptr_or_null);

  R_set_altraw_Elt_method(cls, wrapper_raw_elt);
  R_set_altraw_Get_region_method(cls, wrapper_raw_get_region);
}

static void init_wrapper_character(DllInfo* dll) {
  R_altrep_class_t cls =
    R_make_altstring_class("wrapper_character", package, dll);
  wrapper_character_class = cls;

  R_set_altrep_Serialized_state_method(cls, wrapper_serialized_state);
  R_set_altrep_Duplicate_method(cls, wrapper_duplicate);
  R_set_altrep_Inspect_method(cls, wrapper_inspect);
  R_set_altrep_Length_method(cls, wrapper_length);

  R_set_altvec_Dataptr_method(cls, wrapper_character_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, wrapper_character_dataptr_or_null);

  R_set_altstring_Elt_method(cls, wrapper_character_elt);
  R_set_altstring_Set_elt_method(cls, wrapper_character_set_elt);
}

static void init_wrapper_list(DllInfo* dll) {
  R_altrep_class_t cls = R_make_altlist_class("wrapper_list", package, dll);
  wrapper_list_class = cls;

  R_set_altrep_Serialized_state_method(cls, wrapper_serialized_state);
  R_set_altrep_Duplicate_method(cls, wrapper_duplicate);
  R_set_altrep_Inspect_method(cls, wrapper_inspect);
  R_set_altrep_Length_method(cls, wrapper_length);

  R_set_altvec_Dataptr_method(cls, wrapper_list_dataptr);
  R_set_altvec_Dataptr_or_null_method(cls, wrapper_list_dataptr_or_null);

  R_set_altlist_Elt_method(cls, wrapper_list_elt);
  R_set_altlist_Set_elt_method(cls, wrapper_list_set_elt);
}

void r_init_wrapper(DllInfo* dll) {
  init_wrapper_logical(dll);
  init_wrapper_integer(dll);
  init_wrapper_double(dll);
  init_wrapper_complex(dll);
  init_wrapper_raw(dll);
  init_wrapper_character(dll);
  init_wrapper_list(dll);
}
