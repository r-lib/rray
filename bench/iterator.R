devtools::load_all(quiet = TRUE)

cat("\nArithmetic traversal shapes\n")

local({
  n <- 1e6L
  x <- seq(1, 2, length.out = n)
  y <- seq(2, 3, length.out = n)

  vector_x <- array(x, n)
  vector_y <- array(y, n)

  tall_x <- array(x, c(10000L, 100L))
  tall_y <- array(y, c(10000L, 100L))
  tall_axis0_y <- array(seq(2, 3, length.out = 100L), c(1L, 100L))
  tall_axis1_y <- array(seq(2, 3, length.out = 10000L), c(10000L, 1L))

  wide_x <- array(x, c(100L, 10000L))
  wide_y <- array(y, c(100L, 10000L))
  wide_axis0_y <- array(seq(2, 3, length.out = 10000L), c(1L, 10000L))
  wide_axis1_y <- array(seq(2, 3, length.out = 100L), c(100L, 1L))

  gc()
  print(bench::mark(
    vector_identity = rray_add(vector_x, vector_y),
    vector_scalar = rray_add(vector_x, 1),
    tall_identity = rray_add(tall_x, tall_y),
    tall_broadcast_axis0 = rray_add(tall_x, tall_axis0_y),
    tall_broadcast_axis1 = rray_add(tall_x, tall_axis1_y),
    wide_identity = rray_add(wide_x, wide_y),
    wide_broadcast_axis0 = rray_add(wide_x, wide_axis0_y),
    wide_broadcast_axis1 = rray_add(wide_x, wide_axis1_y),
    iterations = 20L,
    check = FALSE,
    memory = FALSE
  ))
})

cat("\nAxis coalescing and carry depth\n")

local({
  n <- 1e6L
  x <- seq(1, 2, length.out = n)
  y <- seq(2, 3, length.out = n)

  short_first_x <- array(x, c(2L, 500000L))
  short_first_y <- array(y, c(2L, 500000L))
  leading_unit_x <- array(x, c(1L, n))
  leading_unit_y <- array(y, c(1L, n))
  internal_unit_x <- array(x, c(100L, 1L, 10000L))
  internal_unit_y <- array(y, c(100L, 1L, 10000L))
  leading_unit4_x <- array(x, c(1L, 100L, 100L, 100L))
  leading_unit6_x <- array(x, c(1L, 10L, 10L, 10L, 10L, 100L))

  crossed_x <- array(seq(1, 2, length.out = 1000L), c(1L, 1L, 1000L))
  crossed_y <- array(seq(2, 3, length.out = 1000L), c(1L, 1000L, 1L))

  gc()
  print(bench::mark(
    contiguous_short_first = rray_add(short_first_x, short_first_y),
    contiguous_with_scalar = rray_add(short_first_x, 1),
    leading_unit_matrix = rray_add(leading_unit_x, leading_unit_y),
    internal_unit_array = rray_add(internal_unit_x, internal_unit_y),
    leading_unit_array4 = rray_add(leading_unit4_x, 1),
    leading_unit_array6 = rray_add(leading_unit6_x, 1),
    crossed_broadcast = rray_add(crossed_x, crossed_y),
    iterations = 20L,
    check = FALSE,
    memory = FALSE
  ))
})

cat("\nArithmetic operations\n")

local({
  n <- 1e6L
  x <- array(seq(1, 2, length.out = n), c(1L, 1000L, 1000L))
  y <- array(seq(2, 3, length.out = n), c(1L, 1000L, 1000L))

  gc()
  print(bench::mark(
    add = rray_add(x, y),
    subtract = rray_subtract(x, y),
    multiply = rray_multiply(x, y),
    divide = rray_divide(x, y),
    exponentiate = rray_exponentiate(x, y),
    iterations = 15L,
    check = FALSE,
    memory = FALSE
  ))
})

cat("\nArithmetic storage types\n")

local({
  dimensions <- c(2L, 500000L)
  n <- prod(dimensions)

  logical_x <- array(rep(c(TRUE, FALSE), length.out = n), dimensions)
  logical_y <- array(rep(c(FALSE, TRUE), length.out = n), dimensions)
  integer_x <- array(as.integer(seq_len(n) %% 1000L), dimensions)
  integer_y <- array(as.integer((seq_len(n) + 1L) %% 1000L), dimensions)
  double_x <- array(as.double(seq_len(n) %% 1000L), dimensions)
  double_y <- array(as.double((seq_len(n) + 1L) %% 1000L), dimensions)
  complex_x <- array(
    complex(real = seq_len(n) %% 1000L, imaginary = 1),
    dimensions
  )
  complex_y <- array(
    complex(real = seq_len(n) %% 1000L, imaginary = 2),
    dimensions
  )

  gc()
  print(bench::mark(
    logical_logical = rray_add(logical_x, logical_y),
    integer_integer = rray_add(integer_x, integer_y),
    integer_double = rray_add(integer_x, double_y),
    double_double = rray_add(double_x, double_y),
    complex_complex = rray_add(complex_x, complex_y),
    iterations = 15L,
    check = FALSE,
    memory = FALSE
  ))
})

cat("\nDirect broadcasting\n")

local({
  output_dimensions <- c(1000L, 1000L)
  scalar <- array(1, c(1L, 1L))
  row <- array(seq_len(1000L), c(1L, 1000L))
  column <- array(seq_len(1000L), c(1000L, 1L))
  leading_unit <- array(seq_len(1000L), c(1L, 1L, 1000L))
  contiguous_prefix <- array(seq_len(500000L), c(2L, 250000L, 1L))
  high_dimensional <- array(
    seq_len(1000L),
    c(1L, 10L, 1L, 10L, 1L, 10L)
  )

  gc()
  print(bench::mark(
    scalar_to_matrix = rray_broadcast(scalar, output_dimensions),
    row_to_matrix = rray_broadcast(row, output_dimensions),
    column_to_matrix = rray_broadcast(column, output_dimensions),
    leading_unit = rray_broadcast(leading_unit, c(1L, 1000L, 1000L)),
    contiguous_prefix = rray_broadcast(contiguous_prefix, c(2L, 250000L, 2L)),
    alternating_axes = rray_broadcast(high_dimensional, rep(10L, 6L)),
    iterations = 15L,
    check = FALSE,
    memory = FALSE
  ))
})

cat("\nReductions\n")

local({
  matrix <- array(seq(1, 2, length.out = 1e6L), c(1000L, 1000L))
  leading_unit <- array(
    seq(1, 2, length.out = 1e6L),
    c(1L, 1000L, 1000L)
  )
  high_dimensional <- array(
    seq(1, 1.000001, length.out = 1e6L),
    rep(10L, 6L)
  )

  gc()
  print(bench::mark(
    sum_axis1 = rray_sum_along(matrix, 1L),
    sum_axis2 = rray_sum_along(matrix, 2L),
    sum_both = rray_sum_along(matrix, c(1L, 2L)),
    product_axis1 = rray_product_along(matrix, 1L),
    leading_unit_sum_axis3 = rray_sum_along(leading_unit, 3L),
    sum_odd_axes_6d = rray_sum_along(high_dimensional, c(1L, 3L, 5L)),
    sum_even_axes_6d = rray_sum_along(high_dimensional, c(2L, 4L, 6L)),
    iterations = 15L,
    check = FALSE,
    memory = FALSE
  ))
})

cat("\nSplitting\n")

local({
  x <- array(seq_len(1e6L), c(100L, 100L, 100L))
  leading_unit <- array(seq_len(1e6L), c(1L, 1000L, 1000L))
  named <- array(
    seq_len(8000L),
    c(20L, 20L, 20L),
    dimnames = list(
      paste0("r", 1:20),
      paste0("c", 1:20),
      paste0("z", 1:20)
    )
  )

  gc()
  print(bench::mark(
    unnamed_axis1 = rray_split(x, 1L),
    unnamed_axis2 = rray_split(x, 2L),
    unnamed_axis3 = rray_split(x, 3L),
    leading_unit_axis3 = rray_split(leading_unit, 3L),
    named_axes1_3 = rray_split(named, c(1L, 3L)),
    iterations = 10L,
    check = FALSE,
    memory = FALSE
  ))
})

cat("\nTiny arrays\n")

local({
  x1 <- array(seq_len(1L), 1L)
  x10 <- array(seq_len(10L), 10L)
  x100 <- array(seq_len(100L), 100L)
  x1000 <- array(seq_len(1000L), 1000L)

  gc()
  print(bench::mark(
    elements_1 = rray_add(x1, 1L),
    elements_10 = rray_add(x10, 1L),
    elements_100 = rray_add(x100, 1L),
    elements_1000 = rray_add(x1000, 1L),
    iterations = 100L,
    check = FALSE,
    memory = FALSE
  ))
})

cat("\nArray sizes\n")

local({
  x200000 <- array(seq(1, 2, length.out = 2e5L), 2e5L)
  x1000000 <- array(seq(1, 2, length.out = 1e6L), 1e6L)
  x4000000 <- array(seq(1, 2, length.out = 4e6L), 4e6L)
  x20000000 <- array(seq(1, 2, length.out = 2e7L), 2e7L)

  gc()
  print(bench::mark(
    elements_200000 = rray_add(x200000, 1),
    elements_1000000 = rray_add(x1000000, 1),
    elements_4000000 = rray_add(x4000000, 1),
    elements_20000000 = rray_add(x20000000, 1),
    iterations = 10L,
    check = FALSE,
    memory = FALSE
  ))
})
