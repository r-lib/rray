skip_if_not_testing_long_vectors <- function() {
  skip_if_not(
    identical(Sys.getenv("RRAY_TESTING_LONG_VECTORS"), "true"),
    "Not testing long vectors"
  )
}
