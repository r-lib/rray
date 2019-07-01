test_that("rray_multiply_add() basics", {

  expect_equal(rray_multiply_add(1, 2, 1), new_array(3))
  expect_equal(rray_multiply_add(1L, 2L, 1L), new_array(3L))

  # broadcasting
  x <- matrix(1:5)
  expect_equal(rray_dim(rray_multiply_add(x, t(x), x)), c(5, 5))

  # with names
  x <- rray(c(0, 0), c(2, 1), list(c("r1", "r2"), "c1"))
  expect_equal(
    rray_multiply_add(x, 1, 1),
    rray(c(1, 1), c(2, 1), list(c("r1", "r2"), "c1"))
  )

})

test_that("rray_multiply_add() corner cases", {

  # Logicals
  expect_equal(rray_multiply_add(TRUE, TRUE, TRUE), new_array(2L))
  expect_equal(rray_multiply_add(FALSE, FALSE, FALSE), new_array(0L))

  # NaN
  expect_equal(rray_multiply_add(NaN, NaN, NaN), as_array(NaN))

  # Inf
  expect_equal(rray_multiply_add(-Inf, -1, 1), as_array(Inf))
})
