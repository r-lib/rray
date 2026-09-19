expected_rep <- function(x, times, axis, each) {
  dimension <- dim(x)[[axis]]

  locations <- if (each) {
    rep(seq_len(dimension), times = rep_len(times, dimension))
  } else {
    rep.int(seq_len(dimension), times)
  }

  indices <- lapply(dim(x), seq_len)
  indices[[axis]] <- locations

  do.call(`[`, c(list(x), indices, list(drop = FALSE)))
}
