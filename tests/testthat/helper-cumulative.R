cumulative_reference <- function(x, axis, f) {
  dimensions <- dim(x)
  axes <- c(axis, setdiff(seq_along(dimensions), axis))

  out <- apply(x, axes[-1L], \(line) Reduce(f, line, accumulate = TRUE))
  out <- array(out, dimensions[axes])
  out <- aperm(out, order(axes))

  dimnames(out) <- dimnames(x)
  out
}
