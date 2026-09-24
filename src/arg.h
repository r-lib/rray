#ifndef RRAY_ARG_H
#define RRAY_ARG_H

#include "rlang.h"

struct rray_arg {
  r_obj* shelter;
  struct rray_arg* parent;
  r_ssize (*fill)(void* data, char* buf, r_ssize remaining);
  void* data;
};

struct rray_args {
  struct rray_arg* empty;
  struct rray_arg* x;
  struct rray_arg* y;
  struct rray_arg* i;
  struct rray_arg* names;
  struct rray_arg* axis;
  struct rray_arg* dot_axis;
  struct rray_arg* axes;
  struct rray_arg* from;
  struct rray_arg* to;
  struct rray_arg* dimensions;
  struct rray_arg* dot_dimensions;
  struct rray_arg* dimension;
  struct rray_arg* times;
};

extern struct rray_args rray_args;

r_obj* rray_arg(struct rray_arg* arg);

const char* rray_arg_format(struct rray_arg* arg);

const char* rray_arg_format_input(struct rray_arg* arg);

bool rray_arg_is_empty(struct rray_arg* arg);

struct rray_arg new_wrapper_arg(struct rray_arg* parent, const char* arg);

struct rray_arg new_lazy_arg(struct r_lazy* arg);

struct rray_arg* new_subscript_arg(
  struct rray_arg* parent,
  r_obj* names,
  r_ssize n,
  r_ssize* p_i
);

struct rray_arg new_column_arg(struct rray_arg* parent, r_ssize* p_j);

#endif
