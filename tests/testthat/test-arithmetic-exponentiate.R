test_that("exponentiates elementwise", {
  x <- array(c(2L, 3L, 4L), c(3L, 1L))
  y <- array(c(2L, 2L, 2L), c(3L, 1L))
  expect_identical(rray_exponentiate(x, y), array(c(4, 9, 16), c(3L, 1L)))
})

test_that("broadcasts along an axis", {
  x <- array(c(1L, 2L, 3L, 1L, 2L, 3L), c(3L, 2L))
  y <- array(c(2L, 3L), c(1L, 2L))
  expect_identical(
    rray_exponentiate(x, y),
    array(c(1, 4, 9, 1, 8, 27), c(3L, 2L))
  )
})

test_that("broadcasts both inputs at once", {
  x <- array(c(1L, 2L, 3L), c(3L, 1L))
  y <- array(c(2L, 3L), c(1L, 2L))
  expect_identical(
    rray_exponentiate(x, y),
    array(c(1, 4, 9, 1, 8, 27), c(3L, 2L))
  )
})

test_that("dimensionality grows to the larger input", {
  x <- array(c(1L, 2L, 3L), 3L)
  y <- array(1:6, c(3L, 2L))
  out <- rray_exponentiate(x, y)
  expect_identical(rray_dimensions(out), c(3L, 2L))
  expect_identical(as.vector(out), c(1, 4, 27, 1, 32, 729))
})

test_that("works with 3D arrays", {
  x <- array(1:24, c(2L, 3L, 4L))
  y <- array(2L, c(2L, 1L, 1L))
  out <- rray_exponentiate(x, y)
  expect_identical(rray_dimensions(out), c(2L, 3L, 4L))
  expect_identical(as.vector(out), as.double(1:24)^2)
})

test_that("plain vectors are normalized to 1D arrays", {
  out <- rray_exponentiate(1:3, 2L)
  expect_identical(rray_dimensions(out), 3L)
  expect_identical(as.vector(out), c(1, 4, 9))
})

test_that("the output type of every pair of native types", {
  expect_snapshot(native_ptype_matrix(rray_exponentiate, c("x", "y")))
})

test_that("logical and integer combinations give a double array", {
  expect_identical(rray_exponentiate(TRUE, TRUE), array(1, 1L))
  expect_identical(rray_exponentiate(TRUE, 2L), array(1, 1L))
  expect_identical(rray_exponentiate(2L, TRUE), array(2, 1L))
  expect_identical(rray_exponentiate(3L, 2L), array(9, 1L))
})

test_that("integer exponentiation always gives a double, matching base R", {
  expect_identical(rray_exponentiate(2L, 3L), array(2L^3L, 1L))
})

test_that("double wins over logical and integer, in either position", {
  expect_identical(rray_exponentiate(TRUE, 2), array(1, 1L))
  expect_identical(rray_exponentiate(2, TRUE), array(2, 1L))
  expect_identical(rray_exponentiate(2L, 0.5), array(2^0.5, 1L))
  expect_identical(rray_exponentiate(2, 3L), array(8, 1L))
  expect_identical(rray_exponentiate(2, 0.5), array(2^0.5, 1L))
})

test_that("mixed type missing values match base R", {
  expect_identical(as.vector(rray_exponentiate(NA, 2L)), NA^2L)
  expect_identical(as.vector(rray_exponentiate(NA, 1.5)), NA^1.5)
  expect_identical(as.vector(rray_exponentiate(1.5, NA)), 1.5^NA)
  expect_identical(
    as.vector(rray_exponentiate(NA_integer_, 1.5)),
    NA_integer_^1.5
  )
  expect_identical(
    as.vector(rray_exponentiate(1.5, NA_integer_)),
    1.5^NA_integer_
  )
})

test_that("a missing value to the power of zero is 1, matching base R", {
  expect_identical(as.vector(rray_exponentiate(NA, 0)), NA^0)
  expect_identical(
    as.vector(rray_exponentiate(NA_integer_, 0L)),
    NA_integer_^0L
  )
})

test_that("integer missing values propagate", {
  expect_identical(rray_exponentiate(NA_integer_, 2L), array(NA_real_, 1L))
  expect_identical(rray_exponentiate(2L, NA_integer_), array(NA_real_, 1L))
})

test_that("double missing values propagate", {
  expect_identical(rray_exponentiate(NA_real_, 2), array(NA_real_, 1L))
  expect_identical(rray_exponentiate(2, NA_real_), array(NA_real_, 1L))
  expect_identical(rray_exponentiate(NaN, 2), array(NaN, 1L))
  expect_identical(rray_exponentiate(2, NaN), array(NaN, 1L))
})

test_that("zero to a negative power matches base R", {
  expect_identical(as.vector(rray_exponentiate(0, -1)), 0^-1)
  expect_identical(as.vector(rray_exponentiate(0, 2)), 0^2)
})

test_that("infinities match base R", {
  expect_identical(as.vector(rray_exponentiate(Inf, 2)), Inf^2)
  expect_identical(as.vector(rray_exponentiate(2, Inf)), 2^Inf)
  expect_identical(as.vector(rray_exponentiate(Inf, 0)), Inf^0)
  expect_identical(as.vector(rray_exponentiate(Inf, NA_real_)), Inf^NA_real_)
  expect_identical(as.vector(rray_exponentiate(Inf, NaN)), Inf^NaN)
  expect_identical(as.vector(rray_exponentiate(-1, Inf)), (-1)^Inf)
})

test_that("names are kept for axes that aren't broadcast", {
  x <- array(1:6, c(3L, 2L), dimnames = list(c("a", "b", "c"), NULL))
  y <- array(1:2, c(1L, 2L), dimnames = list("z", c("c1", "c2")))
  expect_identical(
    rray_names(rray_exponentiate(x, y)),
    list(c("a", "b", "c"), c("c1", "c2"))
  )
})

test_that("`x` wins over `y` on an axis they both name", {
  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(1:3, 3L, dimnames = list(c("x", "y", "z")))
  expect_identical(rray_names(rray_exponentiate(x, y)), list(c("a", "b", "c")))
})

test_that("names are dropped for a broadcast axis", {
  x <- array(1L, c(1L, 2L), dimnames = list("z", NULL))
  y <- array(1:6, c(3L, 2L))
  expect_null(rray_names(rray_exponentiate(x, y)))
})

test_that("names travel to a new axis of a larger input", {
  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(1:6, c(3L, 2L))
  expect_identical(
    rray_names(rray_exponentiate(x, y)),
    list(c("a", "b", "c"), NULL)
  )
})

test_that("unnamed inputs give an unnamed output", {
  x <- array(1:6, c(3L, 2L))
  expect_null(rray_names(rray_exponentiate(x, x)))
})

test_that("a zero dimension broadcasts against a dimension of 1", {
  x <- array(1L, c(1L, 2L))
  y <- array(integer(), c(0L, 2L))
  out <- rray_exponentiate(x, y)
  expect_identical(rray_dimensions(out), c(0L, 2L))
  expect_identical(as.vector(out), double())
})

test_that("every axis can be zero", {
  x <- array(1L, c(1L, 1L))
  y <- array(integer(), c(0L, 0L))
  expect_identical(rray_dimensions(rray_exponentiate(x, y)), c(0L, 0L))
})

test_that("zero size 1D arrays work", {
  out <- rray_exponentiate(integer(), 2L)
  expect_identical(rray_dimensions(out), 0L)
  expect_identical(as.vector(out), double())
})

test_that("errors on incompatible dimensions", {
  x <- array(1:6, c(3L, 2L))
  expect_snapshot(rray_exponentiate(x, array(1L, c(2L, 2L))), error = TRUE)
  expect_snapshot(
    rray_exponentiate(x, array(integer(), c(0L, 2L))),
    error = TRUE
  )
})

test_that("errors on types `^` doesn't support", {
  expect_snapshot(rray_exponentiate("a", "b"), error = TRUE)
  expect_snapshot(rray_exponentiate(as.raw(1), as.raw(1)), error = TRUE)
  expect_snapshot(rray_exponentiate(list(1), list(2)), error = TRUE)
  expect_snapshot(rray_exponentiate(1L, "a"), error = TRUE)
  expect_snapshot(rray_exponentiate(as.raw(1), 1L), error = TRUE)
})

test_that("errors on complex input", {
  expect_snapshot(rray_exponentiate(1i, 1i), error = TRUE)
  expect_snapshot(rray_exponentiate(1L, 1i), error = TRUE)
  expect_snapshot(rray_exponentiate(1i, 1L), error = TRUE)
  expect_snapshot(rray_exponentiate(1, 1i), error = TRUE)
  expect_snapshot(rray_exponentiate(TRUE, 1i), error = TRUE)
})

test_that("a type error beats a dimension error", {
  x <- array("a", c(2L, 2L))
  y <- array("b", c(3L, 3L))
  expect_snapshot(rray_exponentiate(x, y), error = TRUE)
})

test_that("no integer overflow is possible, since the result is a double", {
  max <- .Machine$integer.max
  expect_identical(rray_exponentiate(max, 2L), array(max^2, 1L))
})

test_that("errors on scalar input", {
  expect_snapshot(rray_exponentiate(NULL, 1L), error = TRUE)
  expect_snapshot(rray_exponentiate(1L, NULL), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2L, 2L)), class = "foo")
  expect_snapshot(rray_exponentiate(x, 1L), error = TRUE)
  expect_snapshot(rray_exponentiate(1L, x), error = TRUE)
})
