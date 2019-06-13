test_that("can use a extract assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_extract(x, 1, 1, 1) <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(as.vector(x), c(NA, 2:8))
})

test_that("value is broadcast in integer extract assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_extract(x, 1) <- NA
  expect_equal(as.vector(x), c(NA, 2, NA, 4, NA, 6, NA, 8))
})

test_that("assigning to 0 does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_extract(x, 0) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))

  rray_extract(x, 0, 0) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("assigning to NULL does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_extract(x, NULL) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("broadcast can fail gracefully in extract assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_extract(x, 1) <- c(1, 2), "due to dimension 1")
})

test_that("can extract assign with base R objects", {
  x <- matrix(1:8, nrow = 2)
  rray_extract(x, 1) <- 4:1
  expect_equal(as.vector(rray_subset(x, 1)), 4:1)
})

test_that("extract assigning a non-vector is an error", {
  x <- array(1:5)
  expect_error(rray_extract(x, 1) <- NULL, class = "vctrs_error_scalar_type")
  expect_error(rray_yank(x, 1) <- environment(), class = "vctrs_error_scalar_type")
})

