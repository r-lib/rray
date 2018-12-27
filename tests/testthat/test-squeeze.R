context("test-squeeze")

test_that("various squeeze variations work", {

  x <- rray(1:10, c(10, 1))
  expect_equal(vec_dim(rray_squeeze(x)), 10)

  # Nothing
  y <- rray_reshape(x, c(5, 2))
  expect_equal(vec_dim(rray_squeeze(y)), c(5, 2))

  z <- rray_reshape(x, c(1, 10))
  expect_equal(vec_dim(rray_squeeze(z)), 10)

  # 3D
  w <- rray_reshape(x, c(5, 2, 1))
  expect_equal(vec_dim(rray_squeeze(w)), c(5, 2))

  # Multi dimension drop
  a <- rray_reshape(x, c(10, 1, 1))
  expect_equal(vec_dim(rray_squeeze(a)), 10)

  # (10, 1, 2) -> (10, 2)
  # middle dimension collapse
  b <- rray_broadcast(x, c(10, 1, 2))
  expect_equal(vec_dim(rray_squeeze(b)), c(10, 2))

  c <- rray_broadcast(z, c(1, 10, 2))
  expect_equal(vec_dim(rray_squeeze(c)), c(10, 2))

})

test_that("dimension names behavior is consistent", {

  x <- rray(1:10, c(1, 10))
  x <- set_col_names(x, letters[1:10])
  y <- t(x)

  # when it makes sense, names are kept
  expect_equal(
    n_dim_names(rray_squeeze(y), 1),
    letters[1:10]
  )

  # this doesnt make sense because the corresponding dim size changes
  expect_equal(
    n_dim_names(rray_squeeze(x), 1),
    character()
  )

})

test_that("explicit dimensions can be specified", {

  x <- rray(1:10, c(1, 10, 1))

  # (1, 10, 1) -> drop 1 -> (10, 1)
  expect_equal(vec_dim(rray_squeeze(x, 1)), c(10, 1))

  # (1, 10, 1) -> drop 3 -> (1, 10)
  expect_equal(vec_dim(rray_squeeze(x, 3)), c(1, 10))
})
