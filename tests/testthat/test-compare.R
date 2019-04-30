context("test-gt")

test_that("gt with rrays", {
  expect_is(rray_greater(rray(1), 1), "vctrs_rray_lgl")
  expect_is(rray_greater(1, rray(1)), "vctrs_rray_lgl")
  expect_equal(rray(1) > 1, rray(FALSE))
})

test_that("gt with base R", {
  expect_equal(rray_greater(1, 2), new_array(FALSE))

  expect_equal(
    rray_greater(matrix(1:5), matrix(6:10)),
    new_matrix(FALSE, c(5, 1))
  )
})

test_that("ensure gt requiring a reshape view is correct", {
  x <- array(1:8, c(2, 2, 2))
  expect_equal(
    rray_greater(x, matrix(1:2)),
    new_array(c(rep(FALSE, 2), rep(TRUE, 6)), dim = c(2, 2, 2))
  )
})

test_that("expect that we can't compare with classed base r", {
  expect_warning(factor("x") > rray(1), "Incompatible methods")
})

test_that("incompatible types are caught", {
  expect_error(rray_greater(rray(1), "hi"), "No common")
})

# TODO - what should this return? Get feedback from this
# https://github.com/r-lib/vctrs/issues/308
test_that("comparison with NULL", {
  expect_equal(rray_greater(1, NULL), NULL)
})

# ------------------------------------------------------------------------------

context("test-gte")

test_that("gte with rrays", {
  expect_is(rray_greater_equal(rray(1), 1), "vctrs_rray_lgl")
  expect_is(rray_greater_equal(1, rray(1)), "vctrs_rray_lgl")
  expect_equal(rray(1) >= 1, rray(TRUE))
})

test_that("gte with base R", {
  expect_equal(rray_greater_equal(1, 1), new_array(TRUE))

  expect_equal(
    rray_greater_equal(matrix(1:5), matrix(6:10)),
    new_matrix(FALSE, c(5, 1))
  )
})

test_that("ensure gte requiring a reshape view is correct", {
  x <- array(1:8, c(2, 2, 2))
  expect_equal(
    rray_greater_equal(x, matrix(1:2)),
    new_array(rep(TRUE, 8), dim = c(2, 2, 2))
  )
})

test_that("expect that we can't compare with classed base r", {
  expect_warning(factor("x") >= rray(1), "Incompatible methods")
})

test_that("incompatible types are caught", {
  expect_error(rray_greater_equal(rray(1), "hi"), "No common")
})

# TODO - what should this return? Get feedback from this
# https://github.com/r-lib/vctrs/issues/308
test_that("comparison with NULL", {
  expect_equal(rray_greater_equal(1, NULL), NULL)
})

