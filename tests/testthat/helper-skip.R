skip_if_not_testing_long_vectors <- function() {
  skip_if_not(
    identical(Sys.getenv("RRAY_TESTING_LONG_VECTORS"), "true"),
    "Not testing long vectors"
  )
}

skip_if_long_double <- function() {
  skip_if(
    !is.null(.Machine$longdouble.digits),
    "Base R accumulates in `long double`"
  )
}
