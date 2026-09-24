#include "syms.h"

struct rray_syms rray_syms;

void rray_init_syms(r_obj* ns) {
  rray_syms.to = r_sym("to");
  rray_syms.arg = r_sym("arg");
  rray_syms.x_arg = r_sym("x_arg");
  rray_syms.y_arg = r_sym("y_arg");
  rray_syms.to_arg = r_sym("to_arg");
  rray_syms.dot_arg = r_sym(".arg");
  rray_syms.dot_to_arg = r_sym(".to_arg");
  rray_syms.dot_ptype_arg = r_sym(".ptype_arg");
  rray_syms.call = r_sym("call");
  rray_syms.dot_call = r_sym(".call");
}
