#ifndef rray_helpers_hpp
#define rray_helpers_hpp

#include <rray.h>

namespace rray {

  // Helper for switching on the string op
  constexpr unsigned int str2int(const char* str, int h = 0) {
    return !str[h] ? 5381 : (str2int(str, h+1) * 33) ^ str[h];
  }

  // Helper for going from a cpp index to an R index
  template <typename T>
  xt::rarray<T> as_r_idx(xt::rarray<T> x) {
    return x + 1;
  }

}

#endif
