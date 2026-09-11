test_that("compares elementwise", {
  x <- array(1:6, c(3L, 2L))
  y <- array(6:1, c(3L, 2L))

  expect_identical(rray_greater_than(x, y), x > y)
  expect_identical(rray_greater_than_or_equal(x, y), x >= y)
  expect_identical(rray_less_than(x, y), x < y)
  expect_identical(rray_less_than_or_equal(x, y), x <= y)
})

test_that("broadcasts both inputs to common dimensions", {
  x <- array(1:3, c(3L, 1L))
  y <- array(c(2L, 4L), c(1L, 2L))

  expected_x <- x[, rep(1L, 2L), drop = FALSE]
  expected_y <- y[rep(1L, 3L), , drop = FALSE]

  expect_identical(rray_greater_than(x, y), expected_x > expected_y)
  expect_identical(
    rray_greater_than_or_equal(x, y),
    expected_x >= expected_y
  )
  expect_identical(rray_less_than(x, y), expected_x < expected_y)
  expect_identical(
    rray_less_than_or_equal(x, y),
    expected_x <= expected_y
  )
})

test_that("works with 1D and 3D arrays", {
  expect_identical(
    rray_greater_than(1:3, 2L),
    array(c(FALSE, FALSE, TRUE), 3L)
  )

  x <- array(1:24, c(2L, 3L, 4L))
  y <- array(c(1L, 2L), c(2L, 1L, 1L))
  expected_y <- y[, rep(1L, 3L), rep(1L, 4L), drop = FALSE]

  expect_identical(rray_less_than_or_equal(x, y), x <= expected_y)
})

test_that("returns logical output for every supported type pair", {
  expect_snapshot(native_ptype_matrix(rray_greater_than, c("x", "y")))
})

test_that("branchless missing-value loops match base R", {
  operations <- list(
    greater_than = list(rray_greater_than, `>`),
    greater_than_or_equal = list(rray_greater_than_or_equal, `>=`),
    less_than = list(rray_less_than, `<`),
    less_than_or_equal = list(rray_less_than_or_equal, `<=`)
  )

  integer_values <- c(NA_integer_, -1L, 0L, 1L)
  double_values <- c(NA_real_, NaN, -Inf, -1, -0, 0, 1, Inf)

  make_case <- function(x_values, y_values = x_values) {
    list(
      x = rep(x_values, each = length(y_values)),
      y = rep(y_values, times = length(x_values))
    )
  }

  cases <- list(
    logical = make_case(c(NA, FALSE, TRUE)),
    integer = make_case(integer_values),
    double = make_case(double_values),
    logical_integer = make_case(c(NA, FALSE, TRUE), integer_values),
    integer_logical = make_case(integer_values, c(NA, FALSE, TRUE)),
    logical_double = make_case(c(NA, FALSE, TRUE), double_values),
    double_logical = make_case(double_values, c(NA, FALSE, TRUE)),
    integer_double = make_case(integer_values, double_values),
    double_integer = make_case(double_values, integer_values)
  )

  for (operation in operations) {
    for (case in cases) {
      expect_identical(
        as.vector(operation[[1]](case$x, case$y)),
        operation[[2]](case$x, case$y)
      )
    }
  }
})

test_that("coalesces names across inputs", {
  x <- array(
    1:3,
    c(3L, 1L),
    dimnames = list(c("r1", "r2", "r3"), "x")
  )
  y <- array(
    1:2,
    c(1L, 2L),
    dimnames = list("y", c("c1", "c2"))
  )

  expect_identical(
    rray_names(rray_greater_than(x, y)),
    list(c("r1", "r2", "r3"), c("c1", "c2"))
  )

  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  y <- array(1:3, 3L, dimnames = list(c("x", "y", "z")))
  expect_identical(
    rray_names(rray_less_than(x, y)),
    list(c("a", "b", "c"))
  )
})

test_that("zero dimensions broadcast against dimensions of 1", {
  x <- array(1L, c(1L, 2L))
  y <- array(integer(), c(0L, 2L))
  expect_identical(
    rray_greater_than(x, y),
    array(logical(), c(0L, 2L))
  )

  x <- array(1L, c(1L, 1L))
  y <- array(integer(), c(0L, 0L))
  expect_identical(rray_less_than(x, y), array(logical(), c(0L, 0L)))

  expect_identical(rray_greater_than(integer(), 1L), array(logical(), 0L))
})

test_that("errors on incompatible dimensions", {
  x <- array(1:6, c(3L, 2L))
  y <- array(1L, c(2L, 2L))
  expect_snapshot(rray_greater_than(x, y), error = TRUE)
})

test_that("errors on unsupported types", {
  expect_snapshot(rray_greater_than(as.raw(1), as.raw(2)), error = TRUE)
  expect_snapshot(rray_greater_than_or_equal(list(1), list(1)), error = TRUE)
  expect_snapshot(rray_less_than(1i, 1i), error = TRUE)
  expect_snapshot(rray_less_than_or_equal("a", "b"), error = TRUE)
})

test_that("a type error beats a dimension error", {
  x <- array("a", c(2L, 2L))
  y <- array("b", c(3L, 3L))
  expect_snapshot(rray_greater_than(x, y), error = TRUE)
})

test_that("errors on scalar and classed input", {
  expect_snapshot(rray_greater_than(NULL, 1L), error = TRUE)
  expect_snapshot(rray_less_than(1L, NULL), error = TRUE)

  x <- structure(1L, class = "foo")
  expect_snapshot(rray_greater_than(x, 1L), error = TRUE)
  expect_snapshot(rray_less_than(1L, x), error = TRUE)
})
