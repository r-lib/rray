# ------------------------------------------------------------------------------
# rray_split_names()

test_that("rray_split_names() subsets the names of a split axis", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  expect_identical(
    rray_split_names(x, c(1L, 3L)),
    list(
      list(c("r1", "r2"), "c1"),
      list(c("r1", "r2"), "c2"),
      list(c("r1", "r2"), "c3")
    )
  )
})

test_that("rray_split_names() carries the names of an unsplit axis over whole", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  expect_identical(
    rray_split_names(x, c(2L, 1L)),
    list(
      list("r1", c("c1", "c2", "c3")),
      list("r2", c("c1", "c2", "c3"))
    )
  )
})

test_that("rray_split_names() splits every axis", {
  x <- array(1:4, c(2L, 2L), dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_identical(
    rray_split_names(x, c(2L, 2L)),
    list(
      list("r1", "c1"),
      list("r2", "c1"),
      list("r1", "c2"),
      list("r2", "c2")
    )
  )
})

test_that("rray_split_names() keeps everything when nothing is split", {
  x <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))
  )
  expect_identical(
    rray_split_names(x, c(1L, 1L)),
    list(list(c("r1", "r2"), c("c1", "c2", "c3")))
  )
})

test_that("rray_split_names() returns NULL when `x` has no names", {
  x <- array(1:6, c(2L, 3L))
  expect_null(rray_split_names(x, c(1L, 3L)))
})

test_that("rray_split_names() returns NULL when every axis is unnamed", {
  x <- rray_set_names(array(1:6, c(2L, 3L)), list(NULL, NULL))
  expect_null(rray_split_names(x, c(1L, 3L)))
})

test_that("rray_split_names() reuses the names of an axis across elements", {
  x <- array(1:4, c(2L, 2L), dimnames = list(c("r1", "r2"), c("c1", "c2")))
  out <- rray_split_names(x, c(2L, 2L))
  expect_identical(out[[1]][[1]], out[[3]][[1]])
  expect_identical(out[[1]][[2]], out[[2]][[2]])
})

test_that("rray_split_names() leaves unnamed axes alone", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("r1", "r2"), NULL))
  expect_identical(
    rray_split_names(x, c(1L, 3L)),
    list(
      list(c("r1", "r2"), NULL),
      list(c("r1", "r2"), NULL),
      list(c("r1", "r2"), NULL)
    )
  )
})

test_that("rray_split_names() carries the single name of a split size 1 axis", {
  x <- array(1:2, c(2L, 1L), dimnames = list(c("r1", "r2"), "z"))
  expect_identical(
    rray_split_names(x, c(1L, 1L)),
    list(list(c("r1", "r2"), "z"))
  )
})

test_that("rray_split_names() works with 1 dimensional arrays", {
  x <- array(1:3, 3L, dimnames = list(c("a", "b", "c")))
  expect_identical(
    rray_split_names(x, 3L),
    list(list("a"), list("b"), list("c"))
  )
  expect_identical(rray_split_names(x, 1L), list(list(c("a", "b", "c"))))
})

test_that("rray_split_names() works with 3+ dimensional arrays", {
  x <- array(
    1:24,
    c(2L, 3L, 4L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"), letters[1:4])
  )
  expect_identical(
    rray_split_names(x, c(1L, 1L, 4L)),
    list(
      list(c("r1", "r2"), c("c1", "c2", "c3"), "a"),
      list(c("r1", "r2"), c("c1", "c2", "c3"), "b"),
      list(c("r1", "r2"), c("c1", "c2", "c3"), "c"),
      list(c("r1", "r2"), c("c1", "c2", "c3"), "d")
    )
  )
})

test_that("rray_split_names() works with zero-size axes", {
  x <- array(integer(), c(0L, 3L), dimnames = list(NULL, c("c1", "c2", "c3")))
  expect_identical(
    rray_split_names(x, c(1L, 3L)),
    list(list(NULL, "c1"), list(NULL, "c2"), list(NULL, "c3"))
  )
  expect_identical(rray_split_names(x, c(0L, 1L)), list())
})

test_that("rray_split_names() agrees with rray_split()", {
  x <- array(
    1:24,
    c(2L, 3L, 4L),
    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"), letters[1:4])
  )
  expect_identical(
    lapply(rray_split(x, c(1, 3)), dimnames),
    rray_split_names(x, c(2L, 1L, 4L))
  )
})
