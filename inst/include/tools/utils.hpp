#ifndef rray_helpers_hpp
#define rray_helpers_hpp

namespace rray {

  // Helper for switching on the string op
  constexpr unsigned int str2int(const char* str, int h = 0) {
    return !str[h] ? 5381 : (str2int(str, h+1) * 33) ^ str[h];
  }

}

#endif
