context("test-mtrx-subset")

test_that("subset doesn't drop dimensions", {
  x <- as_mtrx(matrix(1:24, 3, 8))

  # 1st col of every dimension
  expect_equal(dim(x[1]), c(3, 1))
  expect_equal(x[1], x[,1])

  # first row
  expect_equal(dim(x[1,]), c(1, 8))

  # first of the 2nd dim
  expect_equal(dim(x[,1]), c(3, 1))

  # multiple dimension subset
  expect_equal(dim(x[1,1]), c(1, 1))

  expect_warning(x[,,FALSE])
})

test_that("extract can pull elements", {
  x <- as_mtrx(matrix(1:24, 3, 8))

  expect_equal(x[[1]], 1)
  expect_equal(x[[2]], 2) # col major

  expect_equal(x[[1, 1]], 1)
  expect_equal(x[[2, 1]], 2)

  # not enough dims
  expect_error(x[[,2]])
})

test_that("0D slicing", {
  x <- new_mtrx()
  x_dim <- vec_dim(x)

  expect_is(x[0], "vctrs_mtrx")
  expect_equal(vec_dim(x[0]), x_dim)
})
