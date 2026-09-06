test_that("subtracts elementwise", {
  x <- array(1:6, c(3L, 2L))
  y <- array(6:1, c(3L, 2L))
  expect_identical(
    rray_subtract(x, y),
    array(c(-5L, -3L, -1L, 1L, 3L, 5L), c(3L, 2L))
  )
})

test_that("broadcasts along an axis", {
  x <- array(1:6, c(3L, 2L))
  y <- array(c(10L, 20L), c(1L, 2L))
  expect_identical(
    rray_subtract(x, y),
    array(c(-9L, -8L, -7L, -16L, -15L, -14L), c(3L, 2L))
  )
})

test_that("broadcasts both inputs at once", {
  x <- array(1:3, c(3L, 1L))
  y <- array(c(10L, 20L), c(1L, 2L))
  expect_identical(
    rray_subtract(x, y),
    array(c(-9L, -8L, -7L, -19L, -18L, -17L), c(3L, 2L))
  )
})

test_that("dimensionality grows to the larger input", {
  x <- array(1:3, 3L)
  y <- array(1:6, c(3L, 2L))
  out <- rray_subtract(x, y)
  expect_identical(rray_dimensions(out), c(3L, 2L))
  expect_identical(as.vector(out), c(0L, 0L, 0L, -3L, -3L, -3L))
})

test_that("works with 3D arrays", {
  x <- array(1:24, c(2L, 3L, 4L))
  y <- array(1L, c(2L, 1L, 1L))
  out <- rray_subtract(x, y)
  expect_identical(rray_dimensions(out), c(2L, 3L, 4L))
  expect_identical(as.vector(out), 0:23)
})

test_that("plain vectors are normalized to 1D arrays", {
  out <- rray_subtract(1:3, 1L)
  expect_identical(rray_dimensions(out), 3L)
  expect_identical(as.vector(out), 0:2)
})

test_that("the output type of every pair of native types", {
  expect_snapshot(native_ptype_matrix(rray_subtract, c("x", "y")))
})

test_that("logical and integer combinations give an integer array", {
  expect_identical(rray_subtract(TRUE, TRUE), array(0L, 1L))
  expect_identical(rray_subtract(TRUE, 1L), array(0L, 1L))
  expect_identical(rray_subtract(1L, TRUE), array(0L, 1L))
  expect_identical(rray_subtract(1L, 1L), array(0L, 1L))
})

test_that("double wins over logical and integer, in either position", {
  expect_identical(rray_subtract(TRUE, 1.5), array(-0.5, 1L))
  expect_identical(rray_subtract(1.5, TRUE), array(0.5, 1L))
  expect_identical(rray_subtract(1L, 1.5), array(-0.5, 1L))
  expect_identical(rray_subtract(1.5, 1L), array(0.5, 1L))
  expect_identical(rray_subtract(1.5, 1.5), array(0, 1L))
})

test_that("complex wins over everything else, in either position", {
  expect_identical(rray_subtract(TRUE, 1i), array(1 - 1i, 1L))
  expect_identical(rray_subtract(1i, TRUE), array(-1 + 1i, 1L))
  expect_identical(rray_subtract(1L, 1i), array(1 - 1i, 1L))
  expect_identical(rray_subtract(1i, 1L), array(-1 + 1i, 1L))
  expect_identical(rray_subtract(1.5, 1i), array(1.5 - 1i, 1L))
  expect_identical(rray_subtract(1i, 1.5), array(-1.5 + 1i, 1L))
  expect_identical(rray_subtract(1 + 2i, 3 + 4i), array(-2 - 2i, 1L))
})

test_that("mixed type missing values match base R", {
  expect_identical(as.vector(rray_subtract(NA, 1L)), NA - 1L)
  expect_identical(as.vector(rray_subtract(NA, 1.5)), NA - 1.5)
  expect_identical(as.vector(rray_subtract(1.5, NA)), 1.5 - NA)
  expect_identical(
    as.vector(rray_subtract(NA_integer_, 1.5)),
    NA_integer_ - 1.5
  )
  expect_identical(
    as.vector(rray_subtract(1.5, NA_integer_)),
    1.5 - NA_integer_
  )
})

test_that("casting into complex zeroes the imaginary part", {
  out <- as.vector(rray_subtract(NA, 1i))
  expect_identical(Re(out), NA_real_)
  expect_identical(Im(out), -1)

  out <- as.vector(rray_subtract(NA_integer_, 1i))
  expect_identical(Re(out), NA_real_)
  expect_identical(Im(out), -1)
})

test_that("integer missing values propagate", {
  expect_identical(rray_subtract(NA_integer_, 1L), array(NA_integer_, 1L))
  expect_identical(rray_subtract(1L, NA_integer_), array(NA_integer_, 1L))
})

test_that("double missing values propagate", {
  expect_identical(rray_subtract(NA_real_, 1), array(NA_real_, 1L))
  expect_identical(rray_subtract(1, NA_real_), array(NA_real_, 1L))
  expect_identical(rray_subtract(NaN, 1), array(NaN, 1L))
  expect_identical(rray_subtract(1, NaN), array(NaN, 1L))
})

test_that("complex missing values propagate", {
  expect_identical(
    rray_subtract(NA_complex_, 1 + 1i),
    array(NA_complex_, 1L)
  )

  out <- as.vector(rray_subtract(complex(real = 1, imaginary = NaN), 1 + 1i))
  expect_identical(Re(out), 0)
  expect_identical(Im(out), NaN)
})

test_that("infinities match base R", {
  expect_identical(as.vector(rray_subtract(Inf, 1)), Inf - 1)
  expect_identical(as.vector(rray_subtract(Inf, Inf)), Inf - Inf)
  expect_identical(as.vector(rray_subtract(Inf, NA_real_)), Inf - NA_real_)
  expect_identical(as.vector(rray_subtract(Inf, NaN)), Inf - NaN)
})

test_that("missing values win over integer overflow", {
  expect_identical(
    rray_subtract(NA_integer_, -.Machine$integer.max),
    array(NA_integer_, 1L)
  )
})

test_that("names are kept for axes that aren't broadcast", {
  x <- array(1:6, c(3L, 2L), dimnames = list(c("a", "b", "c"), NULL))
  y <- array(1:2, c(1L, 2L), dimnames = list("z", c("c1", "c2")))
  expect_identical(
    rray_names(rray_subtract(x, y)),
    list(c("a", "b", "c"), c("c1", "c2"))
  )
})

test_that("`x` wins over `y` on an axis they both name", {
  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(1:3, 3L, dimnames = list(c("x", "y", "z")))
  expect_identical(rray_names(rray_subtract(x, y)), list(c("a", "b", "c")))
})

test_that("names are dropped for a broadcast axis", {
  x <- array(1L, c(1L, 2L), dimnames = list("z", NULL))
  y <- array(1:6, c(3L, 2L))
  expect_null(rray_names(rray_subtract(x, y)))
})

test_that("names travel to a new axis of a larger input", {
  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(1:6, c(3L, 2L))
  expect_identical(
    rray_names(rray_subtract(x, y)),
    list(c("a", "b", "c"), NULL)
  )
})

test_that("unnamed inputs give an unnamed output", {
  x <- array(1:6, c(3L, 2L))
  expect_null(rray_names(rray_subtract(x, x)))
})

test_that("a zero dimension broadcasts against a dimension of 1", {
  x <- array(1L, c(1L, 2L))
  y <- array(integer(), c(0L, 2L))
  out <- rray_subtract(x, y)
  expect_identical(rray_dimensions(out), c(0L, 2L))
  expect_identical(as.vector(out), integer())
})

test_that("every axis can be zero", {
  x <- array(1L, c(1L, 1L))
  y <- array(integer(), c(0L, 0L))
  expect_identical(rray_dimensions(rray_subtract(x, y)), c(0L, 0L))
})

test_that("zero size 1D arrays work", {
  out <- rray_subtract(integer(), 1L)
  expect_identical(rray_dimensions(out), 0L)
  expect_identical(as.vector(out), integer())
})

test_that("errors on incompatible dimensions", {
  x <- array(1:6, c(3L, 2L))
  expect_snapshot(rray_subtract(x, array(1L, c(2L, 2L))), error = TRUE)
  expect_snapshot(rray_subtract(x, array(integer(), c(0L, 2L))), error = TRUE)
})

test_that("errors on types `-` doesn't support", {
  expect_snapshot(rray_subtract("a", "b"), error = TRUE)
  expect_snapshot(rray_subtract(as.raw(1), as.raw(1)), error = TRUE)
  expect_snapshot(rray_subtract(list(1), list(2)), error = TRUE)
  expect_snapshot(rray_subtract(1L, "a"), error = TRUE)
  expect_snapshot(rray_subtract(as.raw(1), 1L), error = TRUE)
})

test_that("a type error beats a dimension error", {
  x <- array("a", c(2L, 2L))
  y <- array("b", c(3L, 3L))
  expect_snapshot(rray_subtract(x, y), error = TRUE)
})

test_that("errors on integer overflow", {
  expect_snapshot(rray_subtract(.Machine$integer.max, -1L), error = TRUE)
})

test_that("errors on integer underflow", {
  expect_snapshot(rray_subtract(-.Machine$integer.max, 1L), error = TRUE)
})

test_that("errors on scalar input", {
  expect_snapshot(rray_subtract(NULL, 1L), error = TRUE)
  expect_snapshot(rray_subtract(1L, NULL), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2L, 2L)), class = "foo")
  expect_snapshot(rray_subtract(x, 1L), error = TRUE)
  expect_snapshot(rray_subtract(1L, x), error = TRUE)
})
