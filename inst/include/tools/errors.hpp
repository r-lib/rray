#ifndef rray_errors_hpp
#define rray_errors_hpp

namespace rray {

  [[ noreturn ]] inline void error_unknown_type() {
    Rcpp::stop("Incompatible SEXP encountered; only accepts doubles, integers, and logicals.");
  }

}

#endif
