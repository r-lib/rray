context("test-diff")

# Using `vctrs:::diff.vctrs_vctr` rather than implementing our own method as
# it works just fine!

test_that("can compute a diff on 1D", {
  x <- rray(c(1, 3, 4, 5))
  expect_equal(diff(x), rray(c(2, 1, 1)))
  expect_equal(diff(x, lag = 2), rray(c(3, 2)))
  expect_equal(diff(x, differences = 2), rray(c(-1, 0)))
})

test_that("can compute a diff on 2D", {
  x <- rray(c(1, 3, 4, 5, 6, 8), c(3, 2))
  expect_equal(diff(x), rray(c(2, 1, 1, 2), c(2, 2)))
})

test_that("can compute a diff on 3D", {
  x <- rray(c(1, 3, 4, 5, 6, 8, 9, 1), c(2, 2, 2))
  expect_equal(diff(x), rray(c(2, 1, 2, -8), c(1, 2, 2)))
})

test_that("diff on logical returns integers", {
  expect_equal(diff(rray(c(TRUE, FALSE))), rray(-1L))
})

test_that("diff with too many lags/differences keeps shape", {
  x <- rray(1:6, c(3, 2))
  expect_equal(diff(x, lag = 3), vec_slice(x, 0L))
  expect_equal(diff(x, differences = 3), vec_slice(x, 0L))
})

test_that("diff keeps names", {
  x <- rray(1:6, c(3, 2), dim_names = list(letters[1:3], letters[4:5]))
  expect_equal(rray_dim_names(diff(x)), list(letters[2:3], letters[4:5]))
  expect_equal(rray_dim_names(diff(x, lag = 2)), list(letters[3], letters[4:5]))
  expect_equal(rray_dim_names(diff(x, differences = 2)), list(letters[3], letters[4:5]))
})
