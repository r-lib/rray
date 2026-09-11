benchmark_library <- Sys.getenv("RRAY_BENCH_LIBRARY", unset = NA_character_)

if (!is.na(benchmark_library)) {
  .libPaths(c(benchmark_library, .libPaths()))
}

devtools::load_all(quiet = TRUE)

if (!requireNamespace("broadcast", quietly = TRUE)) {
  stop("The broadcast package must be installed.", call. = FALSE)
}

iterations <- 30L

benchmark_pair <- function(group, case, rray4, comparison, other) {
  rray4()
  other()
  gc()

  result <- bench::mark(
    rray4 = rray4(),
    comparison = other(),
    iterations = iterations,
    check = TRUE,
    memory = TRUE
  )

  data.frame(
    group = group,
    case = case,
    implementation = c("rray4", comparison),
    median_ms = as.numeric(result$median) * 1000,
    mem_alloc_mb = as.numeric(result$mem_alloc) / 1024^2,
    relative_time = as.numeric(result$median) / min(as.numeric(result$median)),
    row.names = NULL
  )
}

bind_results <- function(...) {
  do.call(rbind, list(...))
}

n <- 1e6L
x <- array(seq(1, 2, length.out = n), c(1000L, 1000L))
y <- array(seq(2, 3, length.out = n), c(1000L, 1000L))
scalar <- array(2, c(1L, 1L))
row <- array(seq(2, 3, length.out = 1000L), c(1L, 1000L))
column <- array(seq(2, 3, length.out = 1000L), c(1000L, 1L))
alternating_x <- array(
  seq(1, 2, length.out = 1000L),
  c(1L, 10L, 1L, 10L, 1L, 10L)
)
alternating_y <- array(
  seq(2, 3, length.out = 1000L),
  c(10L, 1L, 10L, 1L, 10L, 1L)
)

addition <- bind_results(
  benchmark_pair(
    "add",
    "same shape",
    \() rray_add(x, y),
    "broadcast",
    \() broadcast::bc.d(x, y, "+")
  ),
  benchmark_pair(
    "add",
    "scalar",
    \() rray_add(x, scalar),
    "broadcast",
    \() broadcast::bc.d(x, scalar, "+")
  ),
  benchmark_pair(
    "add",
    "row",
    \() rray_add(x, row),
    "broadcast",
    \() broadcast::bc.d(x, row, "+")
  ),
  benchmark_pair(
    "add",
    "column",
    \() rray_add(x, column),
    "broadcast",
    \() broadcast::bc.d(x, column, "+")
  ),
  benchmark_pair(
    "add",
    "alternating 6d",
    \() rray_add(alternating_x, alternating_y),
    "broadcast",
    \() broadcast::bc.d(alternating_x, alternating_y, "+")
  )
)

broadcasting <- bind_results(
  benchmark_pair(
    "broadcast",
    "scalar to matrix",
    \() rray_broadcast(scalar, c(1000L, 1000L)),
    "broadcast",
    \() broadcast::rep_dim(scalar, c(1000L, 1000L))
  ),
  benchmark_pair(
    "broadcast",
    "row to matrix",
    \() rray_broadcast(row, c(1000L, 1000L)),
    "broadcast",
    \() broadcast::rep_dim(row, c(1000L, 1000L))
  ),
  benchmark_pair(
    "broadcast",
    "column to matrix",
    \() rray_broadcast(column, c(1000L, 1000L)),
    "broadcast",
    \() broadcast::rep_dim(column, c(1000L, 1000L))
  ),
  benchmark_pair(
    "broadcast",
    "alternating 6d",
    \() rray_broadcast(alternating_x, rep(10L, 6L)),
    "broadcast",
    \() broadcast::rep_dim(alternating_x, rep(10L, 6L))
  )
)

high_dimensional <- array(seq(1, 2, length.out = n), rep(10L, 6L))

reductions <- bind_results(
  benchmark_pair(
    "sum",
    "matrix axis 1",
    \() rray_sum_along(x, 1L),
    "base",
    \() array(colSums(x), c(1L, 1000L))
  ),
  benchmark_pair(
    "sum",
    "matrix axis 2",
    \() rray_sum_along(x, 2L),
    "base",
    \() array(rowSums(x), c(1000L, 1L))
  ),
  benchmark_pair(
    "sum",
    "matrix both axes",
    \() rray_sum_along(x, c(1L, 2L)),
    "base",
    \() array(sum(x), c(1L, 1L))
  ),
  benchmark_pair(
    "sum",
    "6d odd axes",
    \() rray_sum_along(high_dimensional, c(1L, 3L, 5L)),
    "base",
    \() {
      array(
        apply(high_dimensional, c(2L, 4L, 6L), sum),
        c(1L, 10L, 1L, 10L, 1L, 10L)
      )
    }
  ),
  benchmark_pair(
    "sum",
    "6d even axes",
    \() rray_sum_along(high_dimensional, c(2L, 4L, 6L)),
    "base",
    \() {
      array(
        apply(high_dimensional, c(1L, 3L, 5L), sum),
        c(10L, 1L, 10L, 1L, 10L, 1L)
      )
    }
  )
)

results <- bind_results(addition, broadcasting, reductions)
results$median_ms <- round(results$median_ms, 3L)
results$mem_alloc_mb <- round(results$mem_alloc_mb, 3L)
results$relative_time <- round(results$relative_time, 2L)

print(results, row.names = FALSE)
cat("\nR:", as.character(getRversion()), "\n")
cat("rray4:", as.character(utils::packageVersion("rray4")), "\n")
cat("broadcast:", as.character(utils::packageVersion("broadcast")), "\n")
cat("platform:", R.version$platform, "\n")
