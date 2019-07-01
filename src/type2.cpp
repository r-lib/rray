#include <utils.h>
#include <type2.h>

SEXP fns_vec_ptype_inner2 = NULL;

SEXP vec__ptype_inner2(SEXP x, SEXP y) {
  SEXP env = PROTECT(r_new_environment(rray_ns_env, 2));

  Rf_defineVar(syms_x, x, env);
  Rf_defineVar(syms_y, y, env);

  SEXP call = PROTECT(Rf_lang3(fns_vec_ptype_inner2, syms_x, syms_y));

  SEXP res = PROTECT(Rf_eval(call, env));

  UNPROTECT(3);
  return res;
}

void rray_init_type2(SEXP ns) {
  fns_vec_ptype_inner2 = Rf_install("vec_ptype_inner2");
}
