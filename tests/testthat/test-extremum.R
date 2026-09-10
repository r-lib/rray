test_that("computes parallel maxima and minima", {
  x <- array(1:6, c(3L, 2L))
  y <- array(6:1, c(3L, 2L))

  expect_identical(rray_pmax(x, y), array(c(6L, 5L, 4L, 4L, 5L, 6L), dim(x)))
  expect_identical(rray_pmin(x, y), array(c(1L, 2L, 3L, 3L, 2L, 1L), dim(x)))
})

test_that("broadcasts both inputs to common dimensions", {
  x <- array(1:3, c(3L, 1L))
  y <- array(c(2L, 4L), c(1L, 2L))

  expect_identical(
    rray_pmax(x, y),
    array(c(2L, 2L, 3L, 4L, 4L, 4L), c(3L, 2L))
  )
  expect_identical(
    rray_pmin(x, y),
    array(c(1L, 2L, 2L, 1L, 2L, 3L), c(3L, 2L))
  )
})

test_that("plain vectors are normalized to 1D arrays", {
  expect_identical(rray_pmax(1:3, 2L), array(c(2L, 2L, 3L), 3L))
  expect_identical(rray_pmin(1:3, 2L), array(c(1L, 2L, 2L), 3L))
})

test_that("the output type of every pair of native types", {
  expect_snapshot(native_ptype_matrix(rray_pmax, c("x", "y")))
  expect_snapshot(native_ptype_matrix(rray_pmin, c("x", "y")))
})

test_that("logical inputs stay logical", {
  expect_identical(rray_pmax(TRUE, FALSE), array(TRUE, 1L))
  expect_identical(rray_pmin(TRUE, FALSE), array(FALSE, 1L))
})

test_that("integer wins over logical, in either position", {
  expect_identical(rray_pmax(TRUE, 2L), array(2L, 1L))
  expect_identical(rray_pmax(2L, TRUE), array(2L, 1L))
  expect_identical(rray_pmin(TRUE, 2L), array(1L, 1L))
  expect_identical(rray_pmin(2L, TRUE), array(1L, 1L))
})

test_that("double wins over logical and integer, in either position", {
  expect_identical(rray_pmax(TRUE, 2.5), array(2.5, 1L))
  expect_identical(rray_pmax(2.5, TRUE), array(2.5, 1L))
  expect_identical(rray_pmin(1L, 2.5), array(1, 1L))
  expect_identical(rray_pmin(2.5, 1L), array(1, 1L))
})

test_that("missing values match base R", {
  x <- c(1, NA_real_, NaN)
  y <- c(2, 3, 4)

  expect_identical(as.vector(rray_pmax(x, y)), pmax(x, y))
  expect_identical(as.vector(rray_pmin(x, y)), pmin(x, y))
  expect_identical(
    as.vector(rray_pmax(x, y, na_rm = TRUE)),
    pmax(x, y, na.rm = TRUE)
  )
  expect_identical(
    as.vector(rray_pmin(x, y, na_rm = TRUE)),
    pmin(x, y, na.rm = TRUE)
  )
})

test_that("the second double missing value wins when both are missing", {
  x <- c(NA_real_, NaN)
  y <- c(NaN, NA_real_)

  expect_identical(as.vector(rray_pmax(x, y)), pmax(x, y))
  expect_identical(as.vector(rray_pmin(x, y)), pmin(x, y))
  expect_identical(
    as.vector(rray_pmax(x, y, na_rm = TRUE)),
    pmax(x, y, na.rm = TRUE)
  )
  expect_identical(
    as.vector(rray_pmin(x, y, na_rm = TRUE)),
    pmin(x, y, na.rm = TRUE)
  )
})

test_that("logical and integer missing values match base R", {
  x <- c(TRUE, NA, FALSE, NA)
  y <- c(NA, FALSE, NA, NA)

  expect_identical(as.vector(rray_pmax(x, y)), as.logical(pmax(x, y)))
  expect_identical(as.vector(rray_pmin(x, y)), as.logical(pmin(x, y)))
  expect_identical(
    as.vector(rray_pmax(x, y, na_rm = TRUE)),
    as.logical(pmax(x, y, na.rm = TRUE))
  )
  expect_identical(
    as.vector(rray_pmin(x, y, na_rm = TRUE)),
    as.logical(pmin(x, y, na.rm = TRUE))
  )

  x <- c(1L, NA_integer_)
  y <- c(NA_integer_, 2L)
  expect_identical(as.vector(rray_pmax(x, y, na_rm = TRUE)), c(1L, 2L))
  expect_identical(as.vector(rray_pmin(x, y, na_rm = TRUE)), c(1L, 2L))
})

test_that("infinities and signed zero match base R", {
  x <- c(Inf, -Inf, -0, 0)
  y <- c(-Inf, Inf, 0, -0)

  expect_identical(as.vector(rray_pmax(x, y)), pmax(x, y))
  expect_identical(as.vector(rray_pmin(x, y)), pmin(x, y))
})

test_that("names are coalesced across inputs", {
  x <- array(1:3, c(3L, 1L), dimnames = list(c("r1", "r2", "r3"), NULL))
  y <- array(1:2, c(1L, 2L), dimnames = list(NULL, c("c1", "c2")))

  expect_identical(
    rray_names(rray_pmax(x, y)),
    list(c("r1", "r2", "r3"), c("c1", "c2"))
  )
  expect_identical(
    rray_names(rray_pmin(x, y)),
    list(c("r1", "r2", "r3"), c("c1", "c2"))
  )
})

test_that("zero dimensions broadcast against dimensions of 1", {
  x <- array(1L, c(1L, 2L))
  y <- array(integer(), c(0L, 2L))

  expect_identical(rray_pmax(x, y), y)
  expect_identical(rray_pmin(x, y), y)
})

test_that("errors on incompatible dimensions", {
  x <- array(1:6, c(3L, 2L))
  y <- array(1L, c(2L, 2L))

  expect_snapshot(rray_pmax(x, y), error = TRUE)
  expect_snapshot(rray_pmin(x, y), error = TRUE)
})

test_that("errors on unsupported types", {
  expect_snapshot(rray_pmax(1i, 2i), error = TRUE)
  expect_snapshot(rray_pmin(1i, 2i), error = TRUE)
  expect_snapshot(rray_pmax("a", "b"), error = TRUE)
  expect_snapshot(rray_pmin(as.raw(1), as.raw(2)), error = TRUE)
  expect_snapshot(rray_pmax(list(1), list(2)), error = TRUE)
})

test_that("`na_rm` must be `TRUE` or `FALSE`", {
  expect_snapshot(rray_pmax(1L, 2L, na_rm = NA), error = TRUE)
  expect_snapshot(rray_pmin(1L, 2L, na_rm = 1), error = TRUE)
})

test_that("dots must be empty", {
  expect_snapshot(rray_pmax(1L, 2L, 3L), error = TRUE)
  expect_snapshot(rray_pmin(1L, 2L, 3L), error = TRUE)
})

test_that("errors on scalar and classed input", {
  expect_snapshot(rray_pmax(NULL, 1L), error = TRUE)
  expect_snapshot(rray_pmin(1L, NULL), error = TRUE)

  x <- structure(1L, class = "foo")
  expect_snapshot(rray_pmax(x, 1L), error = TRUE)
  expect_snapshot(rray_pmin(1L, x), error = TRUE)
})
