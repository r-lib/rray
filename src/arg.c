#include "arg.h"

#include <stdio.h>
#include <string.h>

#include "utils.h"

#include "decl/arg-decl.h"

#define RRAY_ARG_BUFFER_SIZE 100

r_obj* rray_arg(struct rray_arg* p_arg) {
  if (p_arg == NULL) {
    return r_chrs.empty_string;
  }

  r_ssize size = RRAY_ARG_BUFFER_SIZE;

  while (true) {
    r_obj* shelter = KEEP(r_alloc_raw(size));
    char* buf = (char*) r_raw_begin(shelter);

    if (fill_arg_buffer(p_arg, buf, 0, size) >= 0) {
      r_obj* out = r_chr(buf);
      FREE(1);
      return out;
    }

    FREE(1);
    size += size / 2;
  }
}

const char* rray_arg_format(struct rray_arg* p_arg) {
  r_obj* chr = KEEP(rray_arg(p_arg));
  const char* out = r_format_error_arg(chr);
  FREE(1);
  return out;
}

const char* rray_arg_format_input(struct rray_arg* p_arg) {
  if (rray_arg_is_empty(p_arg)) {
    return "Input";
  }

  return rray_arg_format(p_arg);
}

static r_ssize fill_arg_buffer(
  struct rray_arg* p_arg,
  char* buf,
  r_ssize cur_size,
  r_ssize tot_size
) {
  if (p_arg->p_parent != NULL) {
    cur_size = fill_arg_buffer(p_arg->p_parent, buf, cur_size, tot_size);

    if (cur_size < 0) {
      return cur_size;
    }
  }

  const r_ssize written =
    p_arg->fill(p_arg->data, buf + cur_size, tot_size - cur_size);

  if (written < 0) {
    return written;
  }

  return cur_size + written;
}

static r_ssize str_arg_fill(const char* data, char* buf, r_ssize remaining) {
  const r_ssize len = (r_ssize) strlen(data);

  if (len >= remaining) {
    return -1;
  }

  r_memcpy(buf, data, len);
  buf[len] = '\0';

  return len;
}

struct rray_arg new_wrapper_arg(struct rray_arg* p_parent, const char* arg) {
  struct rray_arg out =
    {.p_parent = p_parent, .fill = &wrapper_arg_fill, .data = (void*) arg};
  return out;
}

static r_ssize wrapper_arg_fill(void* data, char* buf, r_ssize remaining) {
  return str_arg_fill((const char*) data, buf, remaining);
}

struct rray_arg new_lazy_arg(struct r_lazy* arg) {
  return (struct rray_arg){.fill = &lazy_arg_fill, .data = arg};
}

static r_ssize lazy_arg_fill(void* data, char* buf, r_ssize remaining) {
  r_obj* arg = KEEP(r_lazy_eval(*(struct r_lazy*) data));

  const char* string = "";

  if (r_is_string(arg)) {
    string = r_chr_get_c_string(arg, 0);
  } else if (arg != r_null) {
    r_abort(
      "`arg` must be a string or `NULL`, not %s.",
      r_obj_type_friendly(arg)
    );
  }

  const r_ssize out = str_arg_fill(string, buf, remaining);

  FREE(1);
  return out;
}

struct subscript_arg_data {
  struct rray_arg self;
  r_obj* names;
  r_ssize n;
  r_ssize* p_i;
};

struct rray_arg* new_subscript_arg(
  struct rray_arg* p_parent,
  r_obj* names,
  r_ssize n,
  r_ssize* p_i
) {
  r_obj* shelter = KEEP(r_alloc_list(2));
  r_list_poke(shelter, 0, r_alloc_raw(sizeof(struct subscript_arg_data)));
  r_list_poke(shelter, 1, names);

  struct subscript_arg_data* p_data = r_raw_begin(r_list_get(shelter, 0));

  p_data->self = (struct rray_arg){
    .shelter = shelter,
    .p_parent = p_parent,
    .fill = &subscript_arg_fill,
    .data = p_data
  };
  p_data->names = names;
  p_data->n = n;
  p_data->p_i = p_i;

  FREE(1);
  return (struct rray_arg*) p_data;
}

static r_ssize subscript_arg_fill(void* data, char* buf, r_ssize remaining) {
  struct subscript_arg_data* p_data = (struct subscript_arg_data*) data;

  const r_ssize i = *p_data->p_i;
  const r_ssize n = p_data->n;
  r_obj* names = p_data->names;

  if (i >= n) {
    r_stop_internal(
      "`i` of %" R_PRI_SSIZE " can't be past the end of %" R_PRI_SSIZE ".",
      i,
      n
    );
  }

  const size_t space = (size_t) remaining;
  const bool named = r_has_name_at(names, i);
  const bool child = !rray_arg_is_empty(p_data->self.p_parent);

  int len;

  if (child) {
    if (named) {
      len = snprintf(buf, space, "$%s", r_chr_get_c_string(names, i));
    } else {
      len = snprintf(buf, space, "[[%" R_PRI_SSIZE "]]", i + 1);
    }
  } else {
    if (named) {
      len = snprintf(buf, space, "%s", r_chr_get_c_string(names, i));
    } else {
      len = snprintf(buf, space, "..%" R_PRI_SSIZE, i + 1);
    }
  }

  if (len >= remaining) {
    return -1;
  }

  return len;
}

bool rray_arg_is_empty(struct rray_arg* p_arg) {
  if (p_arg == NULL) {
    return true;
  }

  char buf[1];
  return p_arg->fill(p_arg->data, buf, 1) == 0;
}

struct rray_args rray_args;

#define INIT_ARG(ARG)                                                          \
  static struct rray_arg ARG;                                                  \
  ARG = new_wrapper_arg(NULL, #ARG);                                           \
  rray_args.ARG = &ARG

#define INIT_ARG2(ARG, STR)                                                    \
  static struct rray_arg ARG;                                                  \
  ARG = new_wrapper_arg(NULL, STR);                                            \
  rray_args.ARG = &ARG

void rray_init_args(r_obj* ns) {
  INIT_ARG2(empty, "");
  INIT_ARG(x);
  INIT_ARG(names);
  INIT_ARG(axis);
  INIT_ARG(axes);
  INIT_ARG(dimensions);
  INIT_ARG2(dot_dimensions, ".dimensions");
}
