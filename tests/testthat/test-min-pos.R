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
  x <- set_col_names(x, "c1")
  x <- set_row_names(x, letters[1:5])

  expect_equal(
    dim_names(rray_min_pos(x, 1)),
    c(new_empty_dim_names(1), col_names(x))
  )

  expect_equal(
    dim_names(rray_min_pos(x, 2)),
    dim_names(x)
  )

  xx <- rray_broadcast(x, c(5, 2))
  xx <- set_col_names(xx, c("c1", "c2"))

  expect_equal(
    dim_names(rray_min_pos(xx, 2)),
    c(list(row_names(xx)), new_empty_dim_names(1))
  )
})

