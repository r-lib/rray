#include "wrapper.h"

#include <R_ext/Altrep.h>

#include "decl/wrapper-decl.h"

static char* package = "rray4";

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
  r_obj* data = is_wrapper(x) ? wrapper_readonly(x) : x;

  // Reference count on `data` is bumped when we wrap it here.
  // It now looks at least "referenced", but not necessarily "shared".
  r_obj* out = KEEP(R_new_altrep(cls, data, r_null));

  // Creates a fresh attribute pairlist!
  // Note that we always use `x` here, not `data`.
  r_attrib_clone_from(out, x);

  FREE(1);
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

static inline r_obj* wrapper_readonly(r_obj* x) {
  return R_altrep_data1(x);
}

static inline r_obj* wrapper_writable(r_obj* x) {
  r_obj* data = R_altrep_data1(x);

  // If ANYONE besides us shares the wrapped data, clone it
  // before giving out a writable handle.
  if (MAYBE_SHARED(data)) {
    data = r_clone_data(data);
    R_set_altrep_data1(x, data);
  }

  return data;
}

// Shallow duplicates only the data, not its attributes,
// since we don't ever pull them from the wrapped object
static inline r_obj* r_clone_data(r_obj* x) {
  switch (r_typeof(x)) {
    case R_TYPE_logical: {
      const int* v_x = r_lgl_cbegin(x);
      const r_ssize size = r_length(x);
      r_obj* out = KEEP(r_alloc_logical(size));
      int* v_out = r_lgl_begin(out);
      r_memcpy(v_out, v_x, sizeof(int) * size);
      FREE(1);
      return out;
    }
    case R_TYPE_integer: {
      const int* v_x = r_int_cbegin(x);
      const r_ssize size = r_length(x);
      r_obj* out = KEEP(r_alloc_integer(size));
      int* v_out = r_int_begin(out);
      r_memcpy(v_out, v_x, sizeof(int) * size);
      FREE(1);
      return out;
    }
    case R_TYPE_double: {
      const double* v_x = r_dbl_cbegin(x);
      const r_ssize size = r_length(x);
      r_obj* out = KEEP(r_alloc_double(size));
      double* v_out = r_dbl_begin(out);
      r_memcpy(v_out, v_x, sizeof(double) * size);
      FREE(1);
      return out;
    }
    case R_TYPE_complex: {
      const r_complex* v_x = r_cpl_cbegin(x);
      const r_ssize size = r_length(x);
      r_obj* out = KEEP(r_alloc_complex(size));
      r_complex* v_out = r_cpl_begin(out);
      r_memcpy(v_out, v_x, sizeof(r_complex) * size);
      FREE(1);
      return out;
    }
    case R_TYPE_raw: {
      const Rbyte* v_x = r_raw_cbegin(x);
      const r_ssize size = r_length(x);
      r_obj* out = KEEP(r_alloc_raw(size));
      Rbyte* v_out = r_raw_begin(out);
      r_memcpy(v_out, v_x, sizeof(Rbyte) * size);
      FREE(1);
      return out;
    }
    case R_TYPE_character: {
      r_obj* const* v_x = r_chr_cbegin(x);
      const r_ssize size = r_length(x);
      r_obj* out = KEEP(r_alloc_character(size));
      for (r_ssize i = 0; i < size; ++i) {
        r_chr_poke(out, i, v_x[i]);
      }
      FREE(1);
      return out;
    }
    case R_TYPE_list: {
      r_obj* const* v_x = r_list_cbegin(x);
      const r_ssize size = r_length(x);
      r_obj* out = KEEP(r_alloc_list(size));
      for (r_ssize i = 0; i < size; ++i) {
        r_list_poke(out, i, v_x[i]);
      }
      FREE(1);
      return out;
    }
    default: {
      r_stop_unimplemented_type(r_typeof(x));
    }
  }
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
    return Rf_duplicate(wrapper_readonly(x));
  } else {
    // Perform shallow duplication by rewrapping the wrapped
    // data in a new wrapper. This won't wrap the wrapper itself.
    return r_wrap(x);
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
  inspect_subtree(wrapper_readonly(x), pre, deep, pvec);
  return TRUE;
}

static r_ssize wrapper_length(r_obj* x) {
  return r_length(wrapper_readonly(x));
}

// -----------------------------------------------------------------------------
// Dataptr

static void* wrapper_logical_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    return r_lgl_begin(wrapper_writable(x));
  } else {
    return (void*) r_lgl_cbegin(wrapper_readonly(x));
  }
}

static void* wrapper_integer_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    return r_int_begin(wrapper_writable(x));
  } else {
    return (void*) r_int_cbegin(wrapper_readonly(x));
  }
}

static void* wrapper_double_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    return r_dbl_begin(wrapper_writable(x));
  } else {
    return (void*) r_dbl_cbegin(wrapper_readonly(x));
  }
}

static void* wrapper_complex_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    return r_cpl_begin(wrapper_writable(x));
  } else {
    return (void*) r_cpl_cbegin(wrapper_readonly(x));
  }
}

static void* wrapper_raw_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    return r_raw_begin(wrapper_writable(x));
  } else {
    return (void*) r_raw_cbegin(wrapper_readonly(x));
  }
}

static void* wrapper_character_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    r_abort("Can't request a writable dataptr to a character wrapper.");
  } else {
    return (void*) r_chr_cbegin(wrapper_readonly(x));
  }
}

static void* wrapper_list_dataptr(r_obj* x, Rboolean writeable) {
  if (writeable) {
    r_abort("Can't request a writable dataptr to a list wrapper.");
  } else {
    return (void*) r_list_cbegin(wrapper_readonly(x));
  }
}

// -----------------------------------------------------------------------------
// Dataptr_or_null

static const void* wrapper_logical_dataptr_or_null(r_obj* x) {
  return LOGICAL_OR_NULL(wrapper_readonly(x));
}

static const void* wrapper_integer_dataptr_or_null(r_obj* x) {
  return INTEGER_OR_NULL(wrapper_readonly(x));
}

static const void* wrapper_double_dataptr_or_null(r_obj* x) {
  return REAL_OR_NULL(wrapper_readonly(x));
}

static const void* wrapper_complex_dataptr_or_null(r_obj* x) {
  return COMPLEX_OR_NULL(wrapper_readonly(x));
}

static const void* wrapper_raw_dataptr_or_null(r_obj* x) {
  return RAW_OR_NULL(wrapper_readonly(x));
}

static const void* wrapper_character_dataptr_or_null(r_obj* x) {
  // There is no `STRING_PTR_RO_OR_NULL`
  return r_chr_cbegin(wrapper_readonly(x));
}

static const void* wrapper_list_dataptr_or_null(r_obj* x) {
  // There is no `VECTOR_PTR_RO_OR_NULL`
  return r_list_cbegin(wrapper_readonly(x));
}

// -----------------------------------------------------------------------------
// Elt

static int wrapper_logical_elt(r_obj* x, r_ssize i) {
  return r_lgl_get(wrapper_readonly(x), i);
}

static int wrapper_integer_elt(r_obj* x, r_ssize i) {
  return r_int_get(wrapper_readonly(x), i);
}

static double wrapper_double_elt(r_obj* x, r_ssize i) {
  return r_dbl_get(wrapper_readonly(x), i);
}

static r_complex wrapper_complex_elt(r_obj* x, r_ssize i) {
  return r_cpl_get(wrapper_readonly(x), i);
}

static Rbyte wrapper_raw_elt(r_obj* x, r_ssize i) {
  return r_raw_get(wrapper_readonly(x), i);
}

static r_obj* wrapper_character_elt(r_obj* x, r_ssize i) {
  return r_chr_get(wrapper_readonly(x), i);
}

static r_obj* wrapper_list_elt(r_obj* x, r_ssize i) {
  return r_list_get(wrapper_readonly(x), i);
}

// -----------------------------------------------------------------------------
// Get_region

static r_ssize wrapper_logical_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  int* buf
) {
  return LOGICAL_GET_REGION(wrapper_readonly(x), i, n, buf);
}

static r_ssize wrapper_integer_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  int* buf
) {
  return INTEGER_GET_REGION(wrapper_readonly(x), i, n, buf);
}

static r_ssize wrapper_double_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  double* buf
) {
  return REAL_GET_REGION(wrapper_readonly(x), i, n, buf);
}

static r_ssize wrapper_complex_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  Rcomplex* buf
) {
  return COMPLEX_GET_REGION(wrapper_readonly(x), i, n, buf);
}

static r_ssize wrapper_raw_get_region(
  r_obj* x,
  r_ssize i,
  r_ssize n,
  Rbyte* buf
) {
  return RAW_GET_REGION(wrapper_readonly(x), i, n, buf);
}

// -----------------------------------------------------------------------------
// Set_elt

static void wrapper_character_set_elt(r_obj* x, r_ssize i, r_obj* v) {
  r_chr_poke(wrapper_writable(x), i, v);
}

static void wrapper_list_set_elt(r_obj* x, r_ssize i, r_obj* v) {
  r_list_poke(wrapper_writable(x), i, v);
}

// -----------------------------------------------------------------------------
// FFI test helpers

r_obj* ffi_test_wrap(r_obj* x) {
  return r_wrap(x);
}

r_obj* ffi_test_wrapper_readonly(r_obj* x) {
  return wrapper_readonly(x);
}

r_obj* ffi_test_wrapper_writable(r_obj* x) {
  return wrapper_writable(x);
}

r_obj* ffi_test_is_wrapper(r_obj* x) {
  return r_lgl(is_wrapper(x));
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
