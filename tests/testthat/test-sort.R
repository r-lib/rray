context("test-sort")

test_that("can sort vectors", {
  x <- rray(10:1)
  expect_equal(rray_sort(x), rray(1:10))
  expect_equal(rray_sort(x), rray_sort(x, axis = 1))
})

test_that("can sort matrices", {
  x <- rray(c(5:1, 6:10), c(5, 2))

  # global sort
  expect_equal(rray_sort(x), rray(1:10, c(5, 2)))

  # along rows
  expect_equal(rray_sort(x, axis = 1), rray(1:10, c(5, 2)))

  # along cols
  x[[1]] <- 7
  expect_equal(rray_sort(x, axis = 2), rray(c(6L, 4:1, 7L, 7:10), c(5, 2)))
})

test_that("can sort 3D arrays", {
  x <- rray(12:1, c(2, 3, 2))

  expect_equal(rray_sort(x), rray(1:12, c(2, 3, 2)))

  expect_equal(rray_sort(x, axis = 1), rray(c(11:12, 9:10, 7:8, 5:6, 3:4, 1:2), c(2, 3, 2)))

  expect_equal(rray_sort(x, axis = 2), rray(c(8:7, 10:9, 12:11, 2:1, 4:3, 6:5), c(2, 3, 2)))

  expect_equal(rray_sort(x, axis = 3), rray_bind(x[,,2], x[,,1], .axis = 3))
})

test_that("error is thrown when trying to sort on unknown axis", {
  x <- 1:10
  expect_error(rray_sort(x, 2))
})

test_that("sorting drops the names along the axis", {

  x <- rray(
    c(2, 1, 1, 2),
    dim = c(2, 2),
    dim_names = list(
      r = c("r1", "r2"),
      c = c("c1", "c2")
    )
  )

  expect_equal(
    rray_dim_names(rray_sort(x, axis = 1L)),
    list(r = NULL, c = c("c1", "c2"))
  )

  expect_equal(
    rray_dim_names(rray_sort(x, axis = 2L)),
    list(r = c("r1", "r2"), c = NULL)
  )

  # dropped along all axes
  expect_equal(
    rray_dim_names(rray_sort(x)),
    list(r = NULL, c = NULL)
  )
})
