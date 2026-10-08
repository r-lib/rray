# nocov start

.onLoad <- function(libname, pkgname) {
  .Call(ffi_rray_init_library, ns_env())
}

# nocov end
