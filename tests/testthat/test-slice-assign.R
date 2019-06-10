test_that("can get identical results with slice assign and subset assign", {
  x <- array(1:8, c(2, 2, 2))
  expect <- x

  rray_slice(x, 1, 2) <- 1
  rray_subset(expect, , 1) <- 1

  expect_equal(x, expect)

  rray_slice(x, 2, 3) <- matrix(1:2, nrow = 1)
  rray_subset(expect, , , 2) <- matrix(1:2, nrow = 1)

  expect_equal(x, expect)
})
