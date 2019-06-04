context("test-min-pos")

test_that("can compute min positions", {

  x <- rray(1:5)

  expect_equal(
    rray_min_pos(x),
    rray(1L)
  )

  xx <- rray_reshape(x, c(5, 1))

  expect_equal(
    rray_min_pos(xx, NULL),
    rray(1L, c(1, 1))
  )

  expect_equal(
    rray_min_pos(xx, 1),
    rray(1L, c(1, 1))
  )

  xxx <- rray(cbind(xx * 2, xx))

  expect_equal(
    rray_min_pos(xxx, 2),
    rray(rep(2L, times = 5), c(5, 1))
  )

  y <- rray_flip(xx, 1)

  # TODO - https://github.com/QuantStack/xtensor/issues/1487
  expect_equal(
    rray_min_pos(y, 1),
    rray(5L, c(1, 1))
  )

})

test_that("dimension names are kept", {

  x <- rray(1:5, c(5, 1))
  x <- rray_set_col_names(x, "c1")
  x <- rray_set_row_names(x, letters[1:5])

  expect_equal(
    rray_dim_names(rray_min_pos(x, 1)),
    c(rray_empty_dim_names(1), rray_col_names(x))
  )

  expect_equal(
    rray_dim_names(rray_min_pos(x, 2)),
    rray_dim_names(x)
  )

  xx <- rray_broadcast(x, c(5, 2))
  xx <- rray_set_col_names(xx, c("c1", "c2"))

  expect_equal(
    rray_dim_names(rray_min_pos(xx, 2)),
    c(list(rray_row_names(xx)), rray_empty_dim_names(1))
  )
})

# Confirming that the row major + column major iteration
# combination results in correct behavior
test_that("3D objects are iterated over correctly", {

  x <- rray(c(1, 5, 3, 6, 10, 2, 5, 12, 8, 21, 1, 4, 20, 5, 7, 18, 3, 9), c(3, 3, 2))

  # fully column major iteration over all axes
  expect_equal(rray_min_pos(x), rray(1L, c(1, 1, 1)))

  # argmax() is row major iteration, then the
  # keep dims reshape is column major
  expect_equal(
    rray_min_pos(x, 1),
    rray(c(1L, 3L, 1L, 2L, 2L, 2L), c(1, 3, 2))
  )

  expect_equal(
    rray_min_pos(x, 2),
    rray(c(1L, 1L, 2L, 3L, 1L, 1L), c(3, 1, 2))
  )

})
