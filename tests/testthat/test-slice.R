test_that("can get identical results with slice and subset", {
  x <- array(1:8, c(2, 2, 2))

  expect_equal(rray_slice(x, 1, 2), rray_subset(x, , 1))
  expect_equal(rray_slice(x, 1, 1), rray_subset(x, 1))
  expect_equal(rray_slice(x, 2, 3), rray_subset(x, , , 2))
})

test_that("slice can use logical and character indices", {
  x <- array(1:8, c(2, 2, 2), dimnames = list(c("r1", "r2")))

  expect_equal(rray_slice(x, TRUE, 1), rray_subset(x, TRUE))
  expect_equal(rray_slice(x, c(TRUE, FALSE), 3), rray_subset(x, , , c(TRUE, FALSE)))
  expect_equal(rray_slice(x, "r1", 1), rray_subset(x, "r1"))
})
