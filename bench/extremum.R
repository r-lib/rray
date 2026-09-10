devtools::load_all(quiet = TRUE)

size <- as.integer(Sys.getenv("RRAY_BENCH_SIZE", "1000000"))
iterations <- as.integer(Sys.getenv("RRAY_BENCH_ITERATIONS", "15"))
output <- Sys.getenv("RRAY_BENCH_OUTPUT", "")

side <- as.integer(sqrt(size))
if (side * side != size) {
  stop("`RRAY_BENCH_SIZE` must be a perfect square.", call. = FALSE)
}

dimensions <- c(side, side)

cases <- expand.grid(
  operation = c("pmax", "pmin"),
  type = c("integer", "double"),
  na_rm = c(FALSE, TRUE),
  missing = c("none", "sparse", "dense"),
  traversal = c("contiguous", "scalar", "broadcast_axis0", "broadcast_axis1"),
  stringsAsFactors = FALSE
)

make_values <- function(n, type, offset) {
  values <- (seq_len(n) * (offset * 2L + 1L)) %% 2001L - 1000L

  if (type == "integer") {
    as.integer(values)
  } else {
    as.double(values) + offset / 10
  }
}

add_missing <- function(x, type, missing, offset) {
  if (missing == "none") {
    return(x)
  }

  step <- if (missing == "sparse") 101L + offset else 2L + offset
  start <- 1L + offset

  if (start > length(x)) {
    return(x)
  }

  locations <- seq.int(start, length(x), by = step)

  if (type == "integer") {
    x[locations] <- NA_integer_
  } else {
    midpoint <- length(locations) %/% 2L
    na_locations <- locations[seq_len(midpoint)]
    nan_locations <- locations[seq.int(midpoint + 1L, length(locations))]
    x[na_locations] <- NA_real_
    x[nan_locations] <- NaN
  }

  x
}

make_inputs <- function(type, missing, traversal) {
  x <- make_values(size, type, 1L)

  y_size <- switch(
    traversal,
    contiguous = size,
    scalar = 1L,
    broadcast_axis0 = side,
    broadcast_axis1 = side
  )
  y <- make_values(y_size, type, 2L)

  x <- add_missing(x, type, missing, 0L)
  y <- add_missing(y, type, missing, 1L)

  x <- array(x, dimensions)
  y <- switch(
    traversal,
    contiguous = array(y, dimensions),
    scalar = array(y, c(1L, 1L)),
    broadcast_axis0 = array(y, c(1L, side)),
    broadcast_axis1 = array(y, c(side, 1L))
  )

  list(x = x, y = y)
}

results <- vector("list", nrow(cases))

for (i in seq_len(nrow(cases))) {
  case <- cases[i, ]
  inputs <- make_inputs(case$type, case$missing, case$traversal)
  fn <- switch(case$operation, pmax = rray_pmax, pmin = rray_pmin)

  measurement <- bench::mark(
    extremum = fn(inputs$x, inputs$y, na_rm = case$na_rm),
    iterations = iterations,
    check = FALSE,
    memory = FALSE,
    filter_gc = FALSE
  )

  results[[i]] <- cbind(
    case,
    median_ns = as.numeric(measurement$median) * 1e9,
    itr_per_sec = as.numeric(measurement$`itr/sec`)
  )
}

results <- do.call(rbind, results)
row.names(results) <- NULL

print(results, row.names = FALSE)

if (nzchar(output)) {
  saveRDS(
    list(
      results = results,
      size = size,
      iterations = iterations,
      revision = system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
    ),
    output
  )
}
