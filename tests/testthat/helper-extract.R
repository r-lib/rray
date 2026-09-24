extract_base <- function(x, i) {
  out <- x[i]
  names(out) <- NULL
  dim(out) <- length(out)
  out
}
