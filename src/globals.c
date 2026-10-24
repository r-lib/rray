#include "globals.h"

struct rray_globals rray_globals;

void rray_init_globals(r_obj* ns) {
  r_preserve_global(rray_globals.one_int = r_int(1));
  r_preserve_global(rray_globals.two_int = r_int(2));
}
