#ifndef rray_utils_h
#define rray_utils_h

#include <rray.h>

// Helper for switching on the string op
constexpr unsigned int str2int(const char* str, int h = 0) {
  return !str[h] ? 5381 : (str2int(str, h+1) * 33) ^ str[h];
}

#endif
