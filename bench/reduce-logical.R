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
  operation = c("all", "any"),
  na_rm = c(FALSE, TRUE),
  missing = c("none", "sparse", "dense"),
  values = c("true", "false", "mixed"),
  axes = c("axis1", "axis2", "both"),
  stringsAsFactors = FALSE
)

make_values <- function(n, values) {
  switch(
    values,
    true = rep(TRUE, n),
    false = rep(FALSE, n),
    mixed = (seq_len(n) * 7L) %% 3L != 0L
  )
}

add_missing <- function(x, missing) {
  if (missing == "none") {
    return(x)
  }

  step <- if (missing == "sparse") 101L else 3L
  x[seq.int(1L, length(x), by = step)] <- NA
  x
}

make_input <- function(values, missing) {
  x <- make_values(size, values)
  x <- add_missing(x, missing)
  array(x, dimensions)
}

results <- vector("list", nrow(cases))

for (i in seq_len(nrow(cases))) {
  case <- cases[i, ]
  x <- make_input(case$values, case$missing)
  fn <- switch(case$operation, all = rray_all_along, any = rray_any_along)
  axes <- switch(case$axes, axis1 = 1L, axis2 = 2L, both = c(1L, 2L))

  measurement <- bench::mark(
    reduce = fn(x, axes, na_rm = case$na_rm),
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
