#ifndef rray_hpp
#define rray_hpp

// for std::vector & std::size_t
#include <vector>

namespace rray {

  // Useful aliases
  using dim_t  = const std::vector<std::size_t>&;
  using axes_t = const std::vector<std::size_t>&;

}

// xtensor-r headers containing R <-> Rcpp <-> xtensor conversion
#include "xtensor-r/rarray.hpp"
#include "xtensor-r/rtensor.hpp"
#include "xtensor-r/rvectorize.hpp"
#include "xtensor-r/rcontainer.hpp"

#endif
