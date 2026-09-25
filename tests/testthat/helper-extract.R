new_extract_subscript <- function(i, kind, size) {
  list(i = i, kind = kind, size = size)
}

extract_base <- function(x, i) {
  out <- x[i]
  names(out) <- NULL
  dim(out) <- length(out)
  out
}
