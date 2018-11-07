context("test-rray-subset")

test_that("subset doesn't drop dimensions", {
  x <- as_rray(array(1:24, dim = c(3, 4, 2)))

  # 1st col of every dimension
  expect_equal(dim(x[1]), c(3, 1, 2))
  expect_equal(x[1], x[,1])

  # first row
  expect_equal(dim(x[1,]), c(1, 4, 2))

  # first 3rd dim
  expect_equal(dim(x[,,1]), c(3, 4, 1))

  # multiple dimension subset
  expect_equal(dim(x[1,1]), c(1, 1, 2))
  expect_equal(dim(x[1,1,1]), c(1, 1, 1))
})

test_that("subset works on 4D", {
  x <- as_rray(array(1:48, dim = c(3, 4, 2, 2)))

  expect_equal(dim(x[1, 1, 1, 1]), c(1, 1, 1, 1))
  expect_equal(dim(x[, , , 1]), c(3, 4, 2, 1))
})

test_that("extract can pull elements n-dim elements", {
  x <- as_rray(array(1:24, dim = c(3, 4, 2)))

  expect_equal(x[[1]], 1)
  expect_equal(x[[2]], 2) # col major

  expect_equal(x[[1, 1, 1]], 1)
  expect_equal(x[[2, 1, 1]], 2)

  # not enough dims
  expect_error(x[[1, 1]])
})

test_that("extract works on 4D", {
  x <- as_rray(array(1:48, dim = c(3, 4, 2, 2)))

  expect_equal(x[[1]], 1)
  expect_equal(x[[2]], 2) # col major

  expect_equal(x[[1, 1, 1, 1]], 1)
  expect_equal(x[[2, 1, 1, 1]], 2)
  expect_equal(x[[1, 1, 1, 2]], 25)

  # not enough dims
  expect_error(x[[1, 1]])
})
