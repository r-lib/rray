context("test-squeeze")

test_that("various squeeze variations work", {

  x <- rray(1:10, c(10, 1))
  expect_equal(rray_dim(rray_squeeze(x)), 10)

  # Nothing
  y <- rray_reshape(x, c(5, 2))
  expect_equal(rray_dim(rray_squeeze(y)), c(5, 2))

  z <- rray_reshape(x, c(1, 10))
  expect_equal(rray_dim(rray_squeeze(z)), 10)

  # 3D
  w <- rray_reshape(x, c(5, 2, 1))
  expect_equal(rray_dim(rray_squeeze(w)), c(5, 2))

  # Multi dimension drop
  a <- rray_reshape(x, c(10, 1, 1))
  expect_equal(rray_dim(rray_squeeze(a)), 10)

  # (10, 1, 2) -> (10, 2)
  # middle dimension collapse
  b <- rray_broadcast(x, c(10, 1, 2))
  expect_equal(rray_dim(rray_squeeze(b)), c(10, 2))

  c <- rray_broadcast(z, c(1, 10, 2))
  expect_equal(rray_dim(rray_squeeze(c)), c(10, 2))

})

test_that("dimension names are dropped from the squeezed axis", {

  x <- rray(1:10, c(1, 10))
  x <- rray_set_col_names(x, letters[1:10])
  y <- t(x)

  # (10, 1) -> (10)
  # drop 2nd dimension names
  expect_equal(
    rray_axis_names(rray_squeeze(y), 1),
    letters[1:10]
  )

  # (1, 10) -> (10)
  # drop first dimension names
  expect_equal(
    rray_axis_names(rray_squeeze(x), 1),
    letters[1:10]
  )

})

test_that("explicit dimensions can be specified", {

  x <- rray(1:10, c(1, 10, 1))

  # (1, 10, 1) -> drop 1 -> (10, 1)
  expect_equal(rray_dim(rray_squeeze(x, 1)), c(10, 1))

  # (1, 10, 1) -> drop 3 -> (1, 10)
  expect_equal(rray_dim(rray_squeeze(x, 3)), c(1, 10))
})

test_that("can squeeze base objects", {

  x_arr <- array(
    c(1,2,3,4),
    dim = c(2,1,2),
    dimnames = list(c("r1", "r2"), c("c1"), c("d1", "d2"))
  )

  x_arr2 <- array(1:8, c(8, 1, 1))

  x_base <- drop(x_arr)

  expect_equal(
    rray_squeeze(x_arr, 2),
    x_base
  )

  expect_is(
    rray_squeeze(x_arr2),
    "array"
  )

  expect_equal(
    rray_squeeze(x_arr2),
    array(1:8, dimnames = list(NULL))
  )

})

test_that("nothing happens when all dimensions have size >1", {

  x <- rray(1:4, c(2, 2, 1))
  x <- rray_broadcast(x, c(2, 2, 2))
  x <- rray_set_row_names(x, c("r1", "r2"))
  x <- rray_set_col_names(x, c("c1", "c2"))
  x <- rray_set_axis_names(x, 3, c("d1", "d2"))

  # (2, 2, 2) -> (2, 2, 2)
  expect_equal(rray_squeeze(x), x)

  y <- rray(1:2, dim_names = list(c("r1", "r2")))

  # (2) -> (2)
  expect_equal(rray_squeeze(y), y)
})

test_that("squeezing all dimensions keeps names from the first names found", {

  x <- array(1, dimnames = list("r1"))

  # (1) -> (1)
  expect_equal(names(rray_squeeze(x)), names(x))

  x <- array(1, c(1, 1), dimnames = list(NULL, "c1"))

  # (1, 1) -> (1) names in the columns
  expect_equal(names(rray_squeeze(x)), rray_col_names(x))
})

test_that("squeeze fails with bad input", {
  expect_error(rray_squeeze(1, "i"))
  expect_error(rray_squeeze(1, 2), "is 1")
})
