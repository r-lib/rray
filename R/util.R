generate_names <- function(n, quiet = TRUE) {
  tidy_names(rep("", times = n), quiet = quiet)
}

compact <- function (x) {
  is_null <- map_lgl(x, is.null)
  x[!is_null]
}
