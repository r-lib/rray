test_that("can use a subset assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_subset(x, 1, 1, 1) <- NA
  expect_is(x, "vctrs_rray")
  expect_equal(storage.mode(x), "integer")
  expect_equal(as.vector(x), c(NA, 2:8))
})

test_that("value is broadcast in subset assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_subset(x, 1:2, 1) <- NA
  expect_equal(as.vector(x), c(NA, NA, 3, 4, NA, NA, 7, 8))
})

test_that("assigning to 0 does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_subset(x, 0) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))

  x <- rray(1:8, dim = c(2, 2, 2))
  rray_subset(x, 0, 1) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("assigning to NULL does nothing", {
  x <- rray(1:8, dim = c(2, 2, 2))
  rray_subset(x, NULL) <- 1
  expect_equal(x, rray(1:8, dim = c(2, 2, 2)))
})

test_that("broadcast can fail gracefully in subset assign", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_subset(x, 1, 1) <- c(1, 2), "due to dimension 1")
})

test_that("cannot assign down in dimensionality", {
  x <- 1
  expect_error(rray_subset(x, 1) <- matrix(1), "from 2 to 1")
})

test_that("can subset assign with shaped input", {
  x <- rray(1:8, dim = c(2, 2, 2))
  expect_error(rray_subset(x, , 1) <- matrix(1:2), NA)
  expect_equal(
    as.vector(x),
    c(1:4, 1:2, 7:8)
  )
})

test_that("can subset assign with base R objects", {
  x <- matrix(1:8, nrow = 2)
  rray_subset(x, 1) <- matrix(4:1, nrow = 1)
  expect_equal(as.vector(x), c(4, 2, 3, 4, 2, 6, 1, 8))
})

test_that("subset assign keeps names", {
  nms <- list(r = "r1", c = "c1")
  x <- array(1, c(1, 1), dimnames = nms)
  rray_subset(x, 1) <- 2
  expect_equal(rray_dim_names(x), nms)
})

test_that("subset assigning a non-vector is an error", {
  x <- array(1:5)
  expect_error(rray_subset(x, 1) <- NULL, class = "vctrs_error_scalar_type")
  expect_error(rray_subset(x, 1) <- environment(), class = "vctrs_error_scalar_type")
})
