dim_names <- function(x) {
  UseMethod("dim_names")
}

dim_names.default <- function(x) {
  unname(dimnames(x)) %||% list()
}

dim_names.vctrs_rray <- function(x) {
  attr(x, "dim_names")
}

# numeric vector
dim_names.numeric <- function(x) {
  nm <- names(x)
  if (is.null(nm)) {
    list()
  } else {
    list(nm)
  }
}

dim_names.array  <- dim_names.default
dim_names.matrix <- dim_names.default
