# ------------------------------------------------------------------------------
# rray_add()

test_that("adds elementwise", {
  x <- array(1:6, c(3L, 2L))
  y <- array(6:1, c(3L, 2L))
  expect_identical(rray_add(x, y), array(rep(7L, 6L), c(3L, 2L)))
})

test_that("broadcasts along an axis", {
  x <- array(1:6, c(3L, 2L))
  y <- array(c(10L, 20L), c(1L, 2L))
  expect_identical(
    rray_add(x, y),
    array(c(11L, 12L, 13L, 24L, 25L, 26L), c(3L, 2L))
  )
})

test_that("broadcasts both inputs at once", {
  x <- array(1:3, c(3L, 1L))
  y <- array(c(10L, 20L), c(1L, 2L))
  expect_identical(
    rray_add(x, y),
    array(c(11L, 12L, 13L, 21L, 22L, 23L), c(3L, 2L))
  )
})

test_that("dimensionality grows to the larger input", {
  x <- array(1:3, 3L)
  y <- array(1:6, c(3L, 2L))
  out <- rray_add(x, y)
  expect_identical(rray_dimensions(out), c(3L, 2L))
  expect_identical(as.vector(out), c(2L, 4L, 6L, 5L, 7L, 9L))
})

test_that("works with 3D arrays", {
  x <- array(1:24, c(2L, 3L, 4L))
  y <- array(1L, c(2L, 1L, 1L))
  out <- rray_add(x, y)
  expect_identical(rray_dimensions(out), c(2L, 3L, 4L))
  expect_identical(as.vector(out), 2:25)
})

test_that("plain vectors are normalized to 1D arrays", {
  out <- rray_add(1:3, 1L)
  expect_identical(rray_dimensions(out), 3L)
  expect_identical(as.vector(out), 2:4)
})

test_that("logical and integer promote to integer", {
  expect_identical(rray_add(TRUE, TRUE), array(2L, 1L))
  expect_identical(rray_add(TRUE, 1L), array(2L, 1L))
  expect_identical(rray_add(1L, 1L), array(2L, 1L))
})

test_that("double wins over logical and integer", {
  expect_identical(rray_add(TRUE, 1.5), array(2.5, 1L))
  expect_identical(rray_add(1L, 1.5), array(2.5, 1L))
  expect_identical(rray_add(1.5, 1.5), array(3, 1L))
})

test_that("complex wins over everything else", {
  expect_identical(rray_add(TRUE, 1i), array(1 + 1i, 1L))
  expect_identical(rray_add(1L, 1i), array(1 + 1i, 1L))
  expect_identical(rray_add(1.5, 1i), array(1.5 + 1i, 1L))
  expect_identical(rray_add(1 + 2i, 3 + 4i), array(4 + 6i, 1L))
})

test_that("integer missing values propagate", {
  expect_identical(rray_add(NA_integer_, 1L), array(NA_integer_, 1L))
  expect_identical(rray_add(1L, NA_integer_), array(NA_integer_, 1L))
})

test_that("double missing values propagate", {
  expect_identical(rray_add(NA_real_, 1), array(NA_real_, 1L))
  expect_identical(rray_add(1, NA_real_), array(NA_real_, 1L))
  expect_identical(rray_add(NaN, 1), array(NaN, 1L))
  expect_identical(rray_add(1, NaN), array(NaN, 1L))
})

test_that("complex missing values propagate", {
  expect_identical(rray_add(NA_complex_, 1 + 1i), array(NA_complex_, 1L))

  out <- as.vector(rray_add(complex(real = 1, imaginary = NaN), 1 + 1i))
  expect_identical(Re(out), 2)
  expect_identical(Im(out), NaN)
})

test_that("infinities match base R", {
  expect_identical(as.vector(rray_add(Inf, 1)), Inf + 1)
  expect_identical(as.vector(rray_add(Inf, -Inf)), Inf + -Inf)
  expect_identical(as.vector(rray_add(Inf, NA_real_)), Inf + NA_real_)
  expect_identical(as.vector(rray_add(Inf, NaN)), Inf + NaN)
})

test_that("missing values win over integer overflow", {
  expect_identical(
    rray_add(NA_integer_, .Machine$integer.max),
    array(NA_integer_, 1L)
  )
})

test_that("names are kept for axes that aren't broadcast", {
  x <- array(1:6, c(3L, 2L), dimnames = list(c("a", "b", "c"), NULL))
  y <- array(1:2, c(1L, 2L), dimnames = list("z", c("c1", "c2")))
  expect_identical(
    rray_names(rray_add(x, y)),
    list(c("a", "b", "c"), c("c1", "c2"))
  )
})

test_that("`x` wins over `y` on an axis they both name", {
  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(1:3, 3L, dimnames = list(c("x", "y", "z")))
  expect_identical(rray_names(rray_add(x, y)), list(c("a", "b", "c")))
})

test_that("names are dropped for a broadcast axis", {
  x <- array(1L, c(1L, 2L), dimnames = list("z", NULL))
  y <- array(1:6, c(3L, 2L))
  expect_null(rray_names(rray_add(x, y)))
})

test_that("names travel to a new axis of a larger input", {
  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(1:6, c(3L, 2L))
  expect_identical(rray_names(rray_add(x, y)), list(c("a", "b", "c"), NULL))
})

test_that("unnamed inputs give an unnamed output", {
  x <- array(1:6, c(3L, 2L))
  expect_null(rray_names(rray_add(x, x)))
})

test_that("a zero dimension broadcasts against a dimension of 1", {
  x <- array(1L, c(1L, 2L))
  y <- array(integer(), c(0L, 2L))
  out <- rray_add(x, y)
  expect_identical(rray_dimensions(out), c(0L, 2L))
  expect_identical(as.vector(out), integer())
})

test_that("every axis can be zero", {
  x <- array(1L, c(1L, 1L))
  y <- array(integer(), c(0L, 0L))
  expect_identical(rray_dimensions(rray_add(x, y)), c(0L, 0L))
})

test_that("zero size 1D arrays work", {
  out <- rray_add(integer(), 1L)
  expect_identical(rray_dimensions(out), 0L)
  expect_identical(as.vector(out), integer())
})

test_that("errors on incompatible dimensions", {
  x <- array(1:6, c(3L, 2L))
  expect_snapshot(rray_add(x, array(1L, c(2L, 2L))), error = TRUE)
  expect_snapshot(rray_add(x, array(integer(), c(0L, 2L))), error = TRUE)
})

test_that("errors on incompatible types", {
  expect_snapshot(rray_add(1L, "a"), error = TRUE)
  expect_snapshot(rray_add(as.raw(1), 1L), error = TRUE)
})

test_that("errors on types `+` doesn't support", {
  expect_snapshot(rray_add("a", "b"), error = TRUE)
  expect_snapshot(rray_add(as.raw(1), as.raw(1)), error = TRUE)
  expect_snapshot(rray_add(list(1), list(2)), error = TRUE)
})

test_that("errors on integer overflow", {
  expect_snapshot(rray_add(.Machine$integer.max, 1L), error = TRUE)
})

test_that("errors on integer underflow", {
  expect_snapshot(rray_add(-.Machine$integer.max, -1L), error = TRUE)
})

test_that("errors on scalar input", {
  expect_snapshot(rray_add(NULL, 1L), error = TRUE)
  expect_snapshot(rray_add(1L, NULL), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2L, 2L)), class = "foo")
  expect_snapshot(rray_add(x, 1L), error = TRUE)
  expect_snapshot(rray_add(1L, x), error = TRUE)
})
