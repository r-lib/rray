#include <utils.h>
#include <cast.h>

SEXP fns_vec_cast_inner = NULL;

SEXP vec__cast_inner(SEXP x, SEXP to) {
  SEXP env = PROTECT(r_new_environment(rray_ns_env, 2));

  Rf_defineVar(syms_x, x, env);
  Rf_defineVar(syms_to, to, env);

  SEXP call = PROTECT(Rf_lang3(fns_vec_cast_inner, syms_x, syms_to));

  SEXP res = PROTECT(Rf_eval(call, env));

  UNPROTECT(3);
  return res;
}

void rray_init_cast(SEXP ns) {
  fns_vec_cast_inner = Rf_install("vec_cast_inner");
}
