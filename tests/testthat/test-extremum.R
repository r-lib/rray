test_that("computes parallel maxima and minima", {
  x <- array(1:6, c(3L, 2L))
  y <- array(6:1, c(3L, 2L))

  expect_identical(rray_max(x, y), array(c(6L, 5L, 4L, 4L, 5L, 6L), dim(x)))
  expect_identical(rray_min(x, y), array(c(1L, 2L, 3L, 3L, 2L, 1L), dim(x)))
})

test_that("broadcasts both inputs to common dimensions", {
  x <- array(1:3, c(3L, 1L))
  y <- array(c(2L, 4L), c(1L, 2L))

  expect_identical(
    rray_max(x, y),
    array(c(2L, 2L, 3L, 4L, 4L, 4L), c(3L, 2L))
  )
  expect_identical(
    rray_min(x, y),
    array(c(1L, 2L, 2L, 1L, 2L, 3L), c(3L, 2L))
  )
})

test_that("plain vectors are normalized to 1D arrays", {
  expect_identical(rray_max(1:3, 2L), array(c(2L, 2L, 3L), 3L))
  expect_identical(rray_min(1:3, 2L), array(c(1L, 2L, 2L), 3L))
})

test_that("the output type of every pair of native types", {
  expect_snapshot(native_ptype_matrix(rray_max, c("x", "y")))
  expect_snapshot(native_ptype_matrix(rray_min, c("x", "y")))
})

test_that("logical inputs stay logical", {
  expect_identical(rray_max(TRUE, FALSE), array(TRUE, 1L))
  expect_identical(rray_min(TRUE, FALSE), array(FALSE, 1L))
})

test_that("integer wins over logical, in either position", {
  expect_identical(rray_max(TRUE, 2L), array(2L, 1L))
  expect_identical(rray_max(2L, TRUE), array(2L, 1L))
  expect_identical(rray_min(TRUE, 2L), array(1L, 1L))
  expect_identical(rray_min(2L, TRUE), array(1L, 1L))
})

test_that("double wins over logical and integer, in either position", {
  expect_identical(rray_max(TRUE, 2.5), array(2.5, 1L))
  expect_identical(rray_max(2.5, TRUE), array(2.5, 1L))
  expect_identical(rray_min(1L, 2.5), array(1, 1L))
  expect_identical(rray_min(2.5, 1L), array(1, 1L))
})

test_that("native extrema match base R across special values", {
  integer_values <- c(NA_integer_, -1L, 0L, 1L)
  double_values <- c(NA_real_, NaN, -Inf, -1, -0, 0, 1, Inf)

  make_case <- function(x_values, y_values = x_values, cast = identity) {
    list(
      x = rep(x_values, each = length(y_values)),
      y = rep(y_values, times = length(x_values)),
      cast = cast
    )
  }

  cases <- list(
    logical = make_case(c(NA, FALSE, TRUE), cast = as.logical),
    integer = make_case(integer_values),
    double = make_case(double_values),
    integer_double = make_case(integer_values, double_values),
    double_integer = make_case(double_values, integer_values)
  )

  for (case in cases) {
    for (na_rm in c(FALSE, TRUE)) {
      expect_identical(
        as.vector(rray_max(case$x, case$y, na_rm = na_rm)),
        case$cast(pmax(case$x, case$y, na.rm = na_rm))
      )
      expect_identical(
        as.vector(rray_min(case$x, case$y, na_rm = na_rm)),
        case$cast(pmin(case$x, case$y, na.rm = na_rm))
      )
    }
  }
})

test_that("names are coalesced across inputs", {
  x <- array(1:3, c(3L, 1L), dimnames = list(c("r1", "r2", "r3"), NULL))
  y <- array(1:2, c(1L, 2L), dimnames = list(NULL, c("c1", "c2")))

  expect_identical(
    rray_names(rray_max(x, y)),
    list(c("r1", "r2", "r3"), c("c1", "c2"))
  )
  expect_identical(
    rray_names(rray_min(x, y)),
    list(c("r1", "r2", "r3"), c("c1", "c2"))
  )
})

test_that("zero dimensions broadcast against dimensions of 1", {
  x <- array(1L, c(1L, 2L))
  y <- array(integer(), c(0L, 2L))

  expect_identical(rray_max(x, y), y)
  expect_identical(rray_min(x, y), y)
})

test_that("errors on incompatible dimensions", {
  x <- array(1:6, c(3L, 2L))
  y <- array(1L, c(2L, 2L))

  expect_snapshot(rray_max(x, y), error = TRUE)
  expect_snapshot(rray_min(x, y), error = TRUE)
})

test_that("errors on unsupported types", {
  expect_snapshot(rray_max(1i, 2i), error = TRUE)
  expect_snapshot(rray_min(1i, 2i), error = TRUE)
  expect_snapshot(rray_max("a", "b"), error = TRUE)
  expect_snapshot(rray_min(as.raw(1), as.raw(2)), error = TRUE)
  expect_snapshot(rray_max(list(1), list(2)), error = TRUE)
})

test_that("`na_rm` must be `TRUE` or `FALSE`", {
  expect_snapshot(rray_max(1L, 2L, na_rm = NA), error = TRUE)
  expect_snapshot(rray_min(1L, 2L, na_rm = 1), error = TRUE)
})

test_that("dots must be empty", {
  expect_snapshot(rray_max(1L, 2L, 3L), error = TRUE)
  expect_snapshot(rray_min(1L, 2L, 3L), error = TRUE)
})

test_that("errors on scalar and classed input", {
  expect_snapshot(rray_max(NULL, 1L), error = TRUE)
  expect_snapshot(rray_min(1L, NULL), error = TRUE)

  x <- structure(1L, class = "foo")
  expect_snapshot(rray_max(x, 1L), error = TRUE)
  expect_snapshot(rray_min(1L, x), error = TRUE)
})
