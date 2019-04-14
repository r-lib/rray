context("test-max-pos")

test_that("can compute max positions", {

  x <- rray(1:5)

  expect_equal(
    rray_max_pos(x),
    rray(5L)
  )

  xx <- rray_reshape(x, c(5, 1))

  expect_equal(
    rray_max_pos(xx, NULL),
    rray(5L, c(1, 1))
  )

  # https://github.com/QuantStack/xtensor/issues/1487
  expect_equal(
    rray_max_pos(xx, 1),
    rray(5L, c(1, 1))
  )

  expect_equal(
    rray_max_pos(xx, 2),
    rray(rep(1L, times = 5), c(5, 1))
  )

})

test_that("dimension names are kept", {

  x <- rray(1:5, c(5, 1))
  x <- set_col_names(x, "c1")
  x <- set_row_names(x, letters[1:5])

  expect_equal(
    dim_names(rray_max_pos(x, 1)),
    c(new_empty_dim_names(1), col_names(x))
  )

  expect_equal(
    dim_names(rray_max_pos(x, 2)),
    dim_names(x)
  )

  xx <- rray_broadcast(x, c(5, 2))
  xx <- set_col_names(xx, c("c1", "c2"))

  expect_equal(
    dim_names(rray_max_pos(xx, 2)),
    c(list(row_names(xx)), new_empty_dim_names(1))
  )
})

