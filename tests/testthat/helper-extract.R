new_extract_subscript <- function(index, kind, size) {
  list(index = index, kind = kind, size = size)
}

extract_base <- function(x, i) {
  out <- x[i]
  names(out) <- NULL
  dim(out) <- length(out)
  out
}
