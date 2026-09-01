test_that("can sum along axis 1", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_sum(x, 1L)
  expect_identical(as.vector(out), c(15L, 40L))
})

test_that("can sum along axis 2", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_sum(x, 2L)
  expect_identical(as.vector(out), c(7L, 9L, 11L, 13L, 15L))
})

test_that("reduced axes become size 1", {
  x <- array(1:10, c(5L, 2L))
  expect_identical(rray_dimensions(rray_sum(x, 1L)), c(1L, 2L))
  expect_identical(rray_dimensions(rray_sum(x, 2L)), c(5L, 1L))
})

test_that("can reduce over all axes", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_sum(x, c(1L, 2L))
  expect_identical(as.vector(out), 55L)
  expect_identical(rray_dimensions(out), c(1L, 1L))
})

test_that("reducing over no axes returns the input unchanged", {
  x <- array(1:10, c(5L, 2L))
  out <- rray_sum(x, integer())
  expect_identical(out, x)
})

test_that("can reduce over multiple axes of a 3D array", {
  x <- array(1, c(2L, 3L, 4L))
  out <- rray_sum(x, c(1L, 2L))
  expect_identical(as.vector(out), rep(6, 4L))
  expect_identical(rray_dimensions(out), c(1L, 1L, 4L))
})

test_that("can reduce axis 3", {
  x <- array(1:24, c(2L, 3L, 4L))
  out <- rray_sum(x, 3L)
  expect_identical(rray_dimensions(out), c(2L, 3L, 1L))
  expect_identical(as.vector(out), c(40L, 44L, 48L, 52L, 56L, 60L))
})

test_that("dimension names are kept for non-reduced axes", {
  x <- array(1:10, c(5L, 2L), dimnames = list(letters[1:5], c("c1", "c2")))
  expect_identical(dimnames(rray_sum(x, 1L)), list(NULL, c("c1", "c2")))
  expect_identical(dimnames(rray_sum(x, 2L)), list(letters[1:5], NULL))
})

test_that("dimension names are dropped when all named axes are reduced", {
  x <- array(1:10, c(5L, 2L), dimnames = list(letters[1:5], NULL))
  expect_null(dimnames(rray_sum(x, 1L)))
})

test_that("output type matches input type", {
  expect_identical(storage.mode(rray_sum(array(1L), 1L)), "integer")
  expect_identical(storage.mode(rray_sum(array(1), 1L)), "double")
  expect_identical(storage.mode(rray_sum(array(1i), 1L)), "complex")

  # Logical -> integer
  expect_identical(storage.mode(rray_sum(array(TRUE), 1L)), "integer")
})

test_that("logical TRUE is summed as 1", {
  x <- array(c(TRUE, FALSE, TRUE, TRUE), c(2L, 2L))
  out <- rray_sum(x, 1L)
  expect_identical(as.vector(out), c(1L, 2L))
})

test_that("integer NA propagates", {
  x <- array(c(1L, NA_integer_), c(2L, 1L))
  out <- rray_sum(x, 1L)
  expect_identical(as.vector(out), NA_integer_)
})

test_that("double NA / NaN propagates", {
  x <- c(1, NA_real_)
  expect_identical(as.vector(rray_sum(x, 1L)), NA_real_)

  x <- c(NA_real_, 1)
  expect_identical(as.vector(rray_sum(x, 1L)), NA_real_)

  x <- c(1, NaN)
  expect_identical(as.vector(rray_sum(x, 1L)), NaN)

  x <- c(NaN, 1)
  expect_identical(as.vector(rray_sum(x, 1L)), NaN)

  x <- c(NA, NaN)
  expect_identical(as.vector(rray_sum(x, 1L)), NA_real_)

  x <- c(NaN, NA)
  expect_identical(as.vector(rray_sum(x, 1L)), NA_real_)
})

test_that("Inf matches base R sum", {
  expect_identical(
    as.vector(rray_sum(c(Inf, 1), 1L)),
    sum(Inf, 1)
  )
  expect_identical(
    as.vector(rray_sum(c(-Inf, 1), 1L)),
    sum(-Inf, 1)
  )
  expect_identical(
    as.vector(rray_sum(c(Inf, -Inf), 1L)),
    sum(Inf, -Inf)
  )
  expect_identical(
    as.vector(rray_sum(c(Inf, NA), 1L)),
    sum(Inf, NA)
  )
  expect_identical(
    as.vector(rray_sum(c(-Inf, NA), 1L)),
    sum(-Inf, NA)
  )
  expect_identical(
    as.vector(rray_sum(c(Inf, NaN), 1L)),
    sum(Inf, NaN)
  )
  expect_identical(
    as.vector(rray_sum(c(-Inf, NaN), 1L)),
    sum(-Inf, NaN)
  )
  expect_identical(
    as.vector(rray_sum(c(Inf, -Inf, NA), 1L)),
    sum(Inf, -Inf, NA)
  )
  expect_identical(
    as.vector(rray_sum(c(Inf, -Inf, NaN), 1L)),
    sum(Inf, -Inf, NaN)
  )
})

test_that("complex sum works", {
  x <- array(c(1 + 2i, 3 + 4i, 5 + 6i, 7 + 8i), c(2L, 2L))
  expect_identical(as.vector(rray_sum(x, 1L)), c(4 + 6i, 12 + 14i))
  expect_identical(as.vector(rray_sum(x, 2L)), c(6 + 8i, 10 + 12i))
})

test_that("complex NA propagates independently per component", {
  x <- c(1 + 2i, NA_complex_)
  expect_identical(as.vector(rray_sum(x, 1L)), NA_complex_)

  x <- c(complex(real = 1, imaginary = NaN), 1 + 1i)
  out <- as.vector(rray_sum(x, 1L))
  expect_identical(Re(out), 2)
  expect_identical(Im(out), NaN)
})

test_that("logical NA propagates", {
  x <- array(c(TRUE, NA), c(2L, 1L))
  out <- rray_sum(x, 1L)
  expect_identical(as.vector(out), NA_integer_)
})

test_that("na_rm removes integer NA", {
  x <- array(c(1L, NA_integer_, 3L, 4L), c(2L, 2L))
  out <- rray_sum(x, 1L, na_rm = TRUE)
  expect_identical(as.vector(out), c(1L, 7L))
})

test_that("na_rm removes double NA and NaN", {
  x <- c(1, NA_real_, 3)
  expect_identical(as.vector(rray_sum(x, 1L, na_rm = TRUE)), 4)

  x <- c(1, NaN, 3)
  expect_identical(as.vector(rray_sum(x, 1L, na_rm = TRUE)), 4)

  x <- c(NA_real_, NaN)
  expect_identical(as.vector(rray_sum(x, 1L, na_rm = TRUE)), 0)
})

test_that("na_rm removes logical NA", {
  x <- array(c(TRUE, NA, FALSE, TRUE), c(2L, 2L))
  out <- rray_sum(x, 1L, na_rm = TRUE)
  expect_identical(as.vector(out), c(1L, 1L))
})

test_that("na_rm removes complex NA", {
  x <- c(1 + 2i, NA_complex_)
  out <- as.vector(rray_sum(x, 1L, na_rm = TRUE))
  expect_identical(out, 1 + 2i)

  x <- c(complex(real = 1, imaginary = NaN), 1 + 1i)
  out <- as.vector(rray_sum(x, 1L, na_rm = TRUE))
  expect_identical(Re(out), 2)
  expect_identical(Im(out), 1)
})

test_that("na_rm with all NA returns identity", {
  x <- c(NA_integer_, NA_integer_)
  expect_identical(as.vector(rray_sum(x, 1L, na_rm = TRUE)), 0L)

  x <- c(NA_real_, NA_real_)
  expect_identical(as.vector(rray_sum(x, 1L, na_rm = TRUE)), 0)
})

test_that("na_rm with no NA matches default", {
  x <- array(1:10, c(5L, 2L))
  expect_identical(
    rray_sum(x, 1L, na_rm = TRUE),
    rray_sum(x, 1L)
  )
})

test_that("plain vector input works", {
  out <- rray_sum(1:3, 1L)
  expect_identical(as.vector(out), 6L)
  expect_identical(rray_dimensions(out), 1L)
})

test_that("scalar reduction works", {
  out <- rray_sum(5, 1L)
  expect_identical(as.vector(out), 5)
  expect_identical(rray_dimensions(out), 1L)
})

test_that("axes are coerced to integer", {
  x <- array(1:4, c(2L, 2L))
  expect_identical(
    rray_sum(x, 1),
    rray_sum(x, 1L)
  )
})

test_that("reducing a zero-length axis gives identity value", {
  x <- matrix(numeric(), 0L, 2L)
  out <- rray_sum(x, 1L)
  expect_identical(rray_dimensions(out), c(1L, 2L))
  expect_identical(as.vector(out), c(0, 0))
})

test_that("reducing over a non-zero-length axis with a zero-length axis", {
  x <- matrix(numeric(), 0L, 2L)
  out <- rray_sum(x, 2L)
  expect_identical(rray_dimensions(out), c(0L, 1L))
  expect_identical(as.vector(out), numeric())
})

test_that("reducing all axes of a zero-length array", {
  x <- matrix(numeric(), 0L, 2L)
  out <- rray_sum(x, c(1L, 2L))
  expect_identical(rray_dimensions(out), c(1L, 1L))
  expect_identical(as.vector(out), 0)
})

test_that("errors on axes out of range", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_sum(x, 3L), error = TRUE)
})

test_that("errors on axes not in strictly increasing order", {
  x <- array(1:24, c(2L, 3L, 4L))
  expect_snapshot(rray_sum(x, c(1L, 1L)), error = TRUE)
  expect_snapshot(rray_sum(x, c(2L, 1L)), error = TRUE)
})

test_that("errors on axes less than 1", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_sum(x, 0L), error = TRUE)
})

test_that("errors on axes with NA", {
  x <- array(1:4, c(2L, 2L))
  expect_snapshot(rray_sum(x, NA_integer_), error = TRUE)
})

test_that("errors on integer overflow", {
  x <- array(c(.Machine$integer.max, 1L), c(2L, 1L))
  expect_snapshot(rray_sum(x, 1L), error = TRUE)
})

test_that("errors on integer underflow", {
  x <- array(c(-.Machine$integer.max, -1L), c(2L, 1L))
  expect_snapshot(rray_sum(x, 1L), error = TRUE)
})

test_that("errors on non-numeric input", {
  x <- array(letters[1:4], c(2L, 2L))
  expect_snapshot(rray_sum(x, 1L), error = TRUE)

  x <- array(as.raw(1:4), c(2L, 2L))
  expect_snapshot(rray_sum(x, 1L), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_sum(x, 1L), error = TRUE)
})
