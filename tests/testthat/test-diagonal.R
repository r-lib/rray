context("test-diag")

test_that("can create a diagonal matrix", {
  x <- rray_diag(1L)
  expect_equal(rray_dim(x), c(1, 1))
  expect_equal(storage.mode(x), "integer")

  x <- rray_diag(TRUE)
  expect_equal(storage.mode(x), "logical")

  x <- rray_diag(c(1, 2, 3))
  expect_equal(x, diag(c(1, 2, 3)))
})

test_that("rray class is kept", {
  expect_is(rray_diag(rray(1:5), 1), "vctrs_rray_int")
})

test_that("can offset the diagonal", {
  expect_equal(rray_diag(1, 1), array(c(0, 0, 1, 0), c(2, 2)))
  expect_equal(rray_diag(1, -1), array(c(0, 1, 0, 0), c(2, 2)))
})

test_that("can create empty matrices with 0 length input", {
  expect_equal(rray_diag(logical()), matrix(logical(), 0, 0))
  expect_equal(rray_diag(logical(), offset = 1), matrix(FALSE))
  expect_equal(rray_diag(integer(), offset = 3), matrix(0, 3, 3))
})

test_that("`NULL` passes through", {
  expect_equal(rray_diag(NULL), NULL)
})

test_that("fails if `x` is not an allowed type", {
  expect_error(rray_diag("hi"), "Incompatible")
})

test_that("fails if `x` is not 1D", {
  expect_error(rray_diag(matrix(1)), "`x` must be 1D, not 2D")
})

test_that("fails with bad `offset`", {
  expect_error(rray_diag(1, offset = c(1, 1)), "1, not 2")
})
