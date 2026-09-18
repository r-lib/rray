devtools::load_all(quiet = TRUE)

iterations <- as.integer(Sys.getenv("RRAY_BENCH_ITERATIONS", "30"))
output <- Sys.getenv("RRAY_BENCH_OUTPUT", "")

n <- 1e6L
matrix_dimensions <- c(1000L, 1000L)
array_dimensions <- c(100L, 100L, 100L)
six_dimensions <- rep(10L, 6L)

x <- seq(1, 2, length.out = n)
y <- seq(2, 3, length.out = n)

matrix_x <- array(x, matrix_dimensions)
matrix_y <- array(y, matrix_dimensions)
scalar <- array(2, c(1L, 1L))
row <- array(seq(2, 3, length.out = 1000L), c(1L, 1000L))
column <- array(seq(2, 3, length.out = 1000L), c(1000L, 1L))

array_x <- array(x, array_dimensions)
array_inner <- array(
  seq(2, 3, length.out = 10000L),
  c(1L, 100L, 100L)
)
array_later <- array(
  seq(2, 3, length.out = 10000L),
  c(100L, 1L, 100L)
)

shared_x <- array(x, c(1L, 1000L, 1000L))
shared_inner <- array(
  seq(2, 3, length.out = 1000L),
  c(1L, 1L, 1000L)
)
shared_y <- array(y, c(1L, 1000L, 1000L))

six_x <- array(x, six_dimensions)
six_inner <- array(
  seq(2, 3, length.out = 1000L),
  c(1L, 10L, 1L, 10L, 1L, 10L)
)
six_outer <- array(
  seq(2, 3, length.out = 1000L),
  c(10L, 1L, 10L, 1L, 10L, 1L)
)

product_x <- array(seq(1, 1.000001, length.out = n), matrix_dimensions)
logical_x <- array(rep(c(TRUE, FALSE), length.out = n), matrix_dimensions)

results <- list()

cat("\nrray_add() with a scalar on the right\n")
# rray_add() with [1000, 1000] and right [1, 1]
results[["shape/scalar_right"]] <- bench::mark(
  result = rray_add(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["shape/scalar_right"]])

cat("\nrray_add() with a scalar on the left\n")
# rray_add() with left [1, 1] and [1000, 1000]
results[["shape/scalar_left"]] <- bench::mark(
  result = rray_add(scalar, matrix_x),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["shape/scalar_left"]])

cat("\nrray_add() with a row array on the right\n")
# rray_add() with [1000, 1000] and right [1, 1000]
results[["shape/row_right"]] <- bench::mark(
  result = rray_add(matrix_x, row),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["shape/row_right"]])

cat("\nrray_add() with a row array on the left\n")
# rray_add() with left [1, 1000] and [1000, 1000]
results[["shape/row_left"]] <- bench::mark(
  result = rray_add(row, matrix_x),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["shape/row_left"]])

cat("\nrray_add() with a 3D inner broadcast on the right\n")
# rray_add() with [100, 100, 100] and right [1, 100, 100]
results[["shape/3d_inner_right"]] <- bench::mark(
  result = rray_add(array_x, array_inner),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["shape/3d_inner_right"]])

cat("\nrray_add() with a 3D inner broadcast on the left\n")
# rray_add() with left [1, 100, 100] and [100, 100, 100]
results[["shape/3d_inner_left"]] <- bench::mark(
  result = rray_add(array_inner, array_x),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["shape/3d_inner_left"]])

cat("\nrray_add() after a shared leading singleton axis\n")
# rray_add() with [1, 1000, 1000] and right [1, 1, 1000]
results[["shape/shared_leading_right"]] <- bench::mark(
  result = rray_add(shared_x, shared_inner),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["shape/shared_leading_right"]])

cat("\nrray_add() with the shared singleton input on the left\n")
# rray_add() with left [1, 1, 1000] and [1, 1000, 1000]
results[["shape/shared_leading_left"]] <- bench::mark(
  result = rray_add(shared_inner, shared_x),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["shape/shared_leading_left"]])

cat("\nrray_add() with alternating inner broadcasts on the right\n")
# rray_add() with full 6D and right [1, 10, 1, 10, 1, 10]
results[["shape/alternating_inner_right"]] <- bench::mark(
  result = rray_add(six_x, six_inner),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["shape/alternating_inner_right"]])

cat("\nrray_add() with alternating inner broadcasts on the left\n")
# rray_add() with left [1, 10, 1, 10, 1, 10] and full 6D
results[["shape/alternating_inner_left"]] <- bench::mark(
  result = rray_add(six_inner, six_x),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["shape/alternating_inner_left"]])

cat("\nrray_add() with identical shapes as a control\n")
# rray_add() with two [1000, 1000] arrays
results[["control/identical"]] <- bench::mark(
  result = rray_add(matrix_x, matrix_y),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["control/identical"]])

cat("\nrray_add() with a column array as a control\n")
# rray_add() with [1000, 1000] and right [1000, 1]
results[["control/column"]] <- bench::mark(
  result = rray_add(matrix_x, column),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["control/column"]])

cat("\nrray_add() with a later-axis broadcast as a control\n")
# rray_add() with [100, 100, 100] and right [100, 1, 100]
results[["control/later_axis"]] <- bench::mark(
  result = rray_add(array_x, array_later),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["control/later_axis"]])

cat("\nrray_add() with matching leading singletons as a control\n")
# rray_add() with two [1, 1000, 1000] arrays
results[["control/matching_leading"]] <- bench::mark(
  result = rray_add(shared_x, shared_y),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["control/matching_leading"]])

cat("\nrray_add() with an alternating outer broadcast as a control\n")
# rray_add() with full 6D and right [10, 1, 10, 1, 10, 1]
results[["control/alternating_outer"]] <- bench::mark(
  result = rray_add(six_x, six_outer),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["control/alternating_outer"]])

cat("\nrray_subtract() with a scalar on the right\n")
# rray_subtract() with [1000, 1000] and right [1, 1]
results[["operation/subtract"]] <- bench::mark(
  result = rray_subtract(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/subtract"]])

cat("\nrray_multiply() with a scalar on the right\n")
# rray_multiply() with [1000, 1000] and right [1, 1]
results[["operation/multiply"]] <- bench::mark(
  result = rray_multiply(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/multiply"]])

cat("\nrray_divide() with a scalar on the right\n")
# rray_divide() with [1000, 1000] and right [1, 1]
results[["operation/divide"]] <- bench::mark(
  result = rray_divide(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/divide"]])

cat("\nrray_exponentiate() with a scalar on the right\n")
# rray_exponentiate() with [1000, 1000] and right [1, 1]
results[["operation/exponentiate"]] <- bench::mark(
  result = rray_exponentiate(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/exponentiate"]])

cat("\nrray_greater_than() with a scalar on the right\n")
# rray_greater_than() with [1000, 1000] and right [1, 1]
results[["operation/greater_than"]] <- bench::mark(
  result = rray_greater_than(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/greater_than"]])

cat("\nrray_greater_than_or_equal() with a scalar on the right\n")
# rray_greater_than_or_equal() with [1000, 1000] and right [1, 1]
results[["operation/greater_than_or_equal"]] <- bench::mark(
  result = rray_greater_than_or_equal(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/greater_than_or_equal"]])

cat("\nrray_less_than() with a scalar on the right\n")
# rray_less_than() with [1000, 1000] and right [1, 1]
results[["operation/less_than"]] <- bench::mark(
  result = rray_less_than(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/less_than"]])

cat("\nrray_less_than_or_equal() with a scalar on the right\n")
# rray_less_than_or_equal() with [1000, 1000] and right [1, 1]
results[["operation/less_than_or_equal"]] <- bench::mark(
  result = rray_less_than_or_equal(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/less_than_or_equal"]])

cat("\nrray_equal() with a scalar on the right\n")
# rray_equal() with [1000, 1000] and right [1, 1]
results[["operation/equal"]] <- bench::mark(
  result = rray_equal(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/equal"]])

cat("\nrray_not_equal() with a scalar on the right\n")
# rray_not_equal() with [1000, 1000] and right [1, 1]
results[["operation/not_equal"]] <- bench::mark(
  result = rray_not_equal(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/not_equal"]])

cat("\nrray_max() with a scalar on the right\n")
# rray_max() with [1000, 1000] and right [1, 1]
results[["operation/max"]] <- bench::mark(
  result = rray_max(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/max"]])

cat("\nrray_min() with a scalar on the right\n")
# rray_min() with [1000, 1000] and right [1, 1]
results[["operation/min"]] <- bench::mark(
  result = rray_min(matrix_x, scalar),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["operation/min"]])

cat("\nrray_broadcast() from a scalar to a matrix\n")
# rray_broadcast() from [1, 1] to [1000, 1000]
results[["iterator/broadcast_scalar"]] <- bench::mark(
  result = rray_broadcast(scalar, matrix_dimensions),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["iterator/broadcast_scalar"]])

cat("\nrray_broadcast() from a row array to a matrix\n")
# rray_broadcast() from [1, 1000] to [1000, 1000]
results[["iterator/broadcast_row"]] <- bench::mark(
  result = rray_broadcast(row, matrix_dimensions),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["iterator/broadcast_row"]])

cat("\nrray_broadcast() with an inner 3D broadcast\n")
# rray_broadcast() from [1, 100, 100] to [100, 100, 100]
results[["iterator/broadcast_3d_inner"]] <- bench::mark(
  result = rray_broadcast(array_inner, array_dimensions),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["iterator/broadcast_3d_inner"]])

cat("\nrray_broadcast() with alternating inner broadcasts\n")
# rray_broadcast() from [1, 10, 1, 10, 1, 10] to full 6D
results[["iterator/broadcast_alternating"]] <- bench::mark(
  result = rray_broadcast(six_inner, six_dimensions),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["iterator/broadcast_alternating"]])

cat("\nrray_broadcast() from a column array as a control\n")
# rray_broadcast() from [1000, 1] to [1000, 1000]
results[["control/broadcast_column"]] <- bench::mark(
  result = rray_broadcast(column, matrix_dimensions),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["control/broadcast_column"]])

cat("\nrray_sum_along() over the inner axis\n")
# rray_sum_along() over axis 1 of [1000, 1000]
results[["iterator/sum_inner"]] <- bench::mark(
  result = rray_sum_along(matrix_x, 1L),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["iterator/sum_inner"]])

cat("\nrray_product_along() over the inner axis\n")
# rray_product_along() over axis 1 of [1000, 1000]
results[["iterator/product_inner"]] <- bench::mark(
  result = rray_product_along(product_x, 1L),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["iterator/product_inner"]])

cat("\nrray_all_along() over the inner axis\n")
# rray_all_along() over axis 1 of [1000, 1000]
results[["iterator/all_inner"]] <- bench::mark(
  result = rray_all_along(logical_x, 1L),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["iterator/all_inner"]])

cat("\nrray_any_along() over the inner axis\n")
# rray_any_along() over axis 1 of [1000, 1000]
results[["iterator/any_inner"]] <- bench::mark(
  result = rray_any_along(logical_x, 1L),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["iterator/any_inner"]])

cat("\nrray_sum_along() over the outer axis as a control\n")
# rray_sum_along() over axis 2 of [1000, 1000]
results[["control/sum_outer"]] <- bench::mark(
  result = rray_sum_along(matrix_x, 2L),
  iterations = iterations,
  check = FALSE,
  memory = FALSE
)
print(results[["control/sum_outer"]])

summary <- do.call(
  rbind,
  lapply(names(results), function(name) {
    result <- results[[name]]
    data.frame(
      case = name,
      min = as.numeric(result$min),
      median = as.numeric(result$median),
      itr_per_sec = as.numeric(result$`itr/sec`)
    )
  })
)
row.names(summary) <- NULL

cat("\nSummary\n")
print(summary, row.names = FALSE)

if (nzchar(output)) {
  saveRDS(
    list(
      results = results,
      summary = summary,
      iterations = iterations,
      revision = system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
    ),
    output
  )
}
