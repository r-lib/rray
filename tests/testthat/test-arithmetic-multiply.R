test_that("multiplies elementwise", {
  x <- array(1:6, c(3L, 2L))
  y <- array(6:1, c(3L, 2L))
  expect_identical(
    rray_multiply(x, y),
    array(c(6L, 10L, 12L, 12L, 10L, 6L), c(3L, 2L))
  )
})

test_that("broadcasts along an axis", {
  x <- array(1:6, c(3L, 2L))
  y <- array(c(10L, 20L), c(1L, 2L))
  expect_identical(
    rray_multiply(x, y),
    array(c(10L, 20L, 30L, 80L, 100L, 120L), c(3L, 2L))
  )
})

test_that("broadcasts both inputs at once", {
  x <- array(1:3, c(3L, 1L))
  y <- array(c(10L, 20L), c(1L, 2L))
  expect_identical(
    rray_multiply(x, y),
    array(c(10L, 20L, 30L, 20L, 40L, 60L), c(3L, 2L))
  )
})

test_that("dimensionality grows to the larger input", {
  x <- array(1:3, 3L)
  y <- array(1:6, c(3L, 2L))
  out <- rray_multiply(x, y)
  expect_identical(rray_dimensions(out), c(3L, 2L))
  expect_identical(as.vector(out), c(1L, 4L, 9L, 4L, 10L, 18L))
})

test_that("works with 3D arrays", {
  x <- array(1:24, c(2L, 3L, 4L))
  y <- array(2L, c(2L, 1L, 1L))
  out <- rray_multiply(x, y)
  expect_identical(rray_dimensions(out), c(2L, 3L, 4L))
  expect_identical(as.vector(out), 1:24 * 2L)
})

test_that("plain vectors are normalized to 1D arrays", {
  out <- rray_multiply(1:3, 2L)
  expect_identical(rray_dimensions(out), 3L)
  expect_identical(as.vector(out), c(2L, 4L, 6L))
})

test_that("the output type of every pair of native types", {
  expect_snapshot(native_ptype_matrix(rray_multiply, c("x", "y")))
})

test_that("logical and integer combinations give an integer array", {
  expect_identical(rray_multiply(TRUE, TRUE), array(1L, 1L))
  expect_identical(rray_multiply(TRUE, 2L), array(2L, 1L))
  expect_identical(rray_multiply(2L, TRUE), array(2L, 1L))
  expect_identical(rray_multiply(2L, 3L), array(6L, 1L))
})

test_that("double wins over logical and integer, in either position", {
  expect_identical(rray_multiply(TRUE, 1.5), array(1.5, 1L))
  expect_identical(rray_multiply(1.5, TRUE), array(1.5, 1L))
  expect_identical(rray_multiply(2L, 1.5), array(3, 1L))
  expect_identical(rray_multiply(1.5, 2L), array(3, 1L))
  expect_identical(rray_multiply(1.5, 1.5), array(2.25, 1L))
})

test_that("complex wins over everything else, in either position", {
  expect_identical(rray_multiply(TRUE, 1i), array(1i, 1L))
  expect_identical(rray_multiply(1i, TRUE), array(1i, 1L))
  expect_identical(rray_multiply(2L, 1i), array(2i, 1L))
  expect_identical(rray_multiply(1i, 2L), array(2i, 1L))
  expect_identical(rray_multiply(1.5, 1i), array(1.5i, 1L))
  expect_identical(rray_multiply(1i, 1.5), array(1.5i, 1L))
  expect_identical(rray_multiply(1 + 2i, 3 + 4i), array(-5 + 10i, 1L))
})

test_that("mixed type missing values match base R", {
  expect_identical(as.vector(rray_multiply(NA, 2L)), NA * 2L)
  expect_identical(as.vector(rray_multiply(NA, 1.5)), NA * 1.5)
  expect_identical(as.vector(rray_multiply(1.5, NA)), 1.5 * NA)
  expect_identical(
    as.vector(rray_multiply(NA_integer_, 1.5)),
    NA_integer_ * 1.5
  )
  expect_identical(
    as.vector(rray_multiply(1.5, NA_integer_)),
    1.5 * NA_integer_
  )
})

test_that("casting into complex zeroes the imaginary part", {
  out <- as.vector(rray_multiply(2L, 1i))
  expect_identical(Re(out), 0)
  expect_identical(Im(out), 2)
})

test_that("a missing value cast into complex reaches both parts", {
  out <- as.vector(rray_multiply(NA, 1i))
  expect_identical(Re(out), NA_real_)
  expect_identical(Im(out), NA_real_)

  out <- as.vector(rray_multiply(NA_integer_, 1i))
  expect_identical(Re(out), NA_real_)
  expect_identical(Im(out), NA_real_)
})

test_that("integer missing values propagate", {
  expect_identical(rray_multiply(NA_integer_, 2L), array(NA_integer_, 1L))
  expect_identical(rray_multiply(2L, NA_integer_), array(NA_integer_, 1L))
})

test_that("double missing values propagate", {
  expect_identical(rray_multiply(NA_real_, 2), array(NA_real_, 1L))
  expect_identical(rray_multiply(2, NA_real_), array(NA_real_, 1L))
  expect_identical(rray_multiply(NaN, 2), array(NaN, 1L))
  expect_identical(rray_multiply(2, NaN), array(NaN, 1L))
})

test_that("a missing value times zero is still missing", {
  expect_identical(rray_multiply(NA_integer_, 0L), array(NA_integer_, 1L))
  expect_identical(rray_multiply(NA_real_, 0), array(NA_real_, 1L))
})

test_that("complex missing values propagate", {
  expect_identical(rray_multiply(NA_complex_, 1 + 1i), array(NA_complex_, 1L))

  out <- as.vector(rray_multiply(complex(real = 1, imaginary = NaN), 1 + 1i))
  expect_identical(Re(out), NaN)
  expect_identical(Im(out), NaN)
})

test_that("infinities match base R", {
  expect_identical(as.vector(rray_multiply(Inf, 2)), Inf * 2)
  expect_identical(as.vector(rray_multiply(Inf, -Inf)), Inf * -Inf)
  expect_identical(as.vector(rray_multiply(Inf, 0)), Inf * 0)
  expect_identical(as.vector(rray_multiply(Inf, NA_real_)), Inf * NA_real_)
  expect_identical(as.vector(rray_multiply(Inf, NaN)), Inf * NaN)
})

test_that("complex infinities are recovered like base R", {
  x <- complex(real = Inf, imaginary = Inf)
  out <- as.vector(rray_multiply(x, 1 + 0i))
  expect_identical(Re(out), Inf)
  expect_identical(Im(out), Inf)

  out <- as.vector(rray_multiply(complex(real = Inf, imaginary = 0), 2 + 0i))
  expect_identical(Re(out), Inf)
  expect_identical(Im(out), NaN)
})

test_that("the largest representable products are allowed", {
  max <- .Machine$integer.max
  expect_identical(rray_multiply(max, 1L), array(max, 1L))
  expect_identical(rray_multiply(max, -1L), array(-max, 1L))
})

test_that("missing values win over integer overflow", {
  expect_identical(
    rray_multiply(NA_integer_, .Machine$integer.max),
    array(NA_integer_, 1L)
  )
})

test_that("names are kept for axes that aren't broadcast", {
  x <- array(1:6, c(3L, 2L), dimnames = list(c("a", "b", "c"), NULL))
  y <- array(1:2, c(1L, 2L), dimnames = list("z", c("c1", "c2")))
  expect_identical(
    rray_names(rray_multiply(x, y)),
    list(c("a", "b", "c"), c("c1", "c2"))
  )
})

test_that("`x` wins over `y` on an axis they both name", {
  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(1:3, 3L, dimnames = list(c("x", "y", "z")))
  expect_identical(rray_names(rray_multiply(x, y)), list(c("a", "b", "c")))
})

test_that("names are dropped for a broadcast axis", {
  x <- array(1L, c(1L, 2L), dimnames = list("z", NULL))
  y <- array(1:6, c(3L, 2L))
  expect_null(rray_names(rray_multiply(x, y)))
})

test_that("names travel to a new axis of a larger input", {
  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(1:6, c(3L, 2L))
  expect_identical(
    rray_names(rray_multiply(x, y)),
    list(c("a", "b", "c"), NULL)
  )
})

test_that("unnamed inputs give an unnamed output", {
  x <- array(1:6, c(3L, 2L))
  expect_null(rray_names(rray_multiply(x, x)))
})

test_that("a zero dimension broadcasts against a dimension of 1", {
  x <- array(1L, c(1L, 2L))
  y <- array(integer(), c(0L, 2L))
  out <- rray_multiply(x, y)
  expect_identical(rray_dimensions(out), c(0L, 2L))
  expect_identical(as.vector(out), integer())
})

test_that("every axis can be zero", {
  x <- array(1L, c(1L, 1L))
  y <- array(integer(), c(0L, 0L))
  expect_identical(rray_dimensions(rray_multiply(x, y)), c(0L, 0L))
})

test_that("zero size 1D arrays work", {
  out <- rray_multiply(integer(), 2L)
  expect_identical(rray_dimensions(out), 0L)
  expect_identical(as.vector(out), integer())
})

test_that("errors on incompatible dimensions", {
  x <- array(1:6, c(3L, 2L))
  expect_snapshot(rray_multiply(x, array(1L, c(2L, 2L))), error = TRUE)
  expect_snapshot(rray_multiply(x, array(integer(), c(0L, 2L))), error = TRUE)
})

test_that("errors on types `*` doesn't support", {
  expect_snapshot(rray_multiply("a", "b"), error = TRUE)
  expect_snapshot(rray_multiply(as.raw(1), as.raw(1)), error = TRUE)
  expect_snapshot(rray_multiply(list(1), list(2)), error = TRUE)
  expect_snapshot(rray_multiply(1L, "a"), error = TRUE)
  expect_snapshot(rray_multiply(as.raw(1), 1L), error = TRUE)
})

test_that("a type error beats a dimension error", {
  x <- array("a", c(2L, 2L))
  y <- array("b", c(3L, 3L))
  expect_snapshot(rray_multiply(x, y), error = TRUE)
})

test_that("errors on integer overflow", {
  expect_snapshot(rray_multiply(.Machine$integer.max, 2L), error = TRUE)
})

test_that("errors on integer underflow", {
  expect_snapshot(rray_multiply(-.Machine$integer.max, 2L), error = TRUE)
})

test_that("errors when the product is exactly the missing value", {
  expect_snapshot(rray_multiply(-1073741824L, 2L), error = TRUE)
})

test_that("errors on scalar input", {
  expect_snapshot(rray_multiply(NULL, 1L), error = TRUE)
  expect_snapshot(rray_multiply(1L, NULL), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2L, 2L)), class = "foo")
  expect_snapshot(rray_multiply(x, 1L), error = TRUE)
  expect_snapshot(rray_multiply(1L, x), error = TRUE)
})
