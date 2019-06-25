test_that("basic", {
  expect_equal(rray_hypot(2, 3), new_array(sqrt(2^2 + 3^2)))
  expect_equal(rray_hypot(TRUE, TRUE), new_array(sqrt(1^2 + 1^2)))
})

test_that("broadcasting is performed", {
  expect_equal(
    rray_hypot(matrix(1:2), matrix(1:2, 1)),

    new_matrix(
      c(
        sqrt(1 ^ 2 + 1 ^ 2),
        sqrt(1 ^ 2 + 2 ^ 2),
        sqrt(2 ^ 2 + 1 ^ 2),
        sqrt(2 ^ 2 + 2 ^ 2)
      ),
      c(2, 2)
    )
  )
})

test_that("dimension names are kept", {
  x <- rray(1, dim = c(1, 1), dim_names = list("r1", NULL))
  y <- rray(2, dim = c(1, 1), dim_names = list("rr1", "cc1"))
  expect_equal(rray_dim_names(rray_hypot(x, y)), list("r1", "cc1"))
  expect_equal(rray_dim_names(rray_hypot(y, x)), rray_dim_names(y))
})

test_that("corner cases", {
  expect_equal(rray_hypot(Inf, 1), new_array(Inf))
  expect_equal(rray_hypot(-Inf, 1), new_array(Inf))

  expect_equal(rray_hypot(0, 0), new_array(0))

  expect_equal(rray_hypot(NaN, NaN), new_array(NaN))
})
