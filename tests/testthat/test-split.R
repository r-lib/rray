test_that("can split along a single axis", {
  x <- array(1:24, c(4, 3, 2))

  out <- rray_split(x, 3)
  expect_length(out, 2)
  expect_equal(dim(out[[1]]), c(4L, 3L, 1L))
  expect_equal(dim(out[[2]]), c(4L, 3L, 1L))
  expect_equal(out[[1]], array(1:12, c(4, 3, 1)))
  expect_equal(out[[2]], array(13:24, c(4, 3, 1)))

  out <- rray_split(x, 1)
  expect_length(out, 4)
  expect_equal(dim(out[[1]]), c(1L, 3L, 2L))
  expect_equal(out[[1]], array(x[1, , ], c(1, 3, 2)))

  out <- rray_split(x, 2)
  expect_length(out, 3)
  expect_equal(dim(out[[1]]), c(4L, 1L, 2L))
  expect_equal(out[[1]], array(x[, 1, ], c(4, 1, 2)))
})

test_that("can split along multiple axes", {
  x <- array(1:24, c(4, 3, 2))

  out <- rray_split(x, c(1, 2))
  expect_length(out, 12)
  expect_equal(dim(out[[1]]), c(1L, 1L, 2L))

  out <- rray_split(x, c(1, 2, 3))
  expect_length(out, 24)
  expect_equal(dim(out[[1]]), c(1L, 1L, 1L))
})

test_that("splitting carries through singleton axes for atomic and list arrays", {
  for (input in list(1:12, letters[1:12], as.list(1:12))) {
    x <- array(input, c(2L, 1L, 3L, 2L))
    expect_identical(
      rray_split(x, c(1L, 4L)),
      list(
        x[1L, , , 1L, drop = FALSE],
        x[2L, , , 1L, drop = FALSE],
        x[1L, , , 2L, drop = FALSE],
        x[2L, , , 2L, drop = FALSE]
      )
    )
  }
})

test_that("coalesces split axes", {
  expected_split <- function(x, axes) {
    dimensions <- dim(x)
    points <- arrayInd(seq_len(prod(dimensions[axes])), dimensions[axes])

    lapply(seq_len(nrow(points)), function(i) {
      indices <- lapply(seq_along(dimensions), function(axis) {
        split_axis <- match(axis, axes)

        if (is.na(split_axis)) {
          seq_len(dimensions[[axis]])
        } else {
          points[i, split_axis]
        }
      })

      do.call(`[`, c(list(x), indices, list(drop = FALSE)))
    })
  }

  axes <- list(1L, 2L, 3L, 4L, c(1L, 3L), c(2L, 4L))

  x <- array(1:24, c(1L, 3L, 2L, 4L))
  for (axis in axes) {
    expect_identical(rray_split(x, axis), expected_split(x, axis))
  }

  dimnames(x) <- list("a", letters[1:3], LETTERS[1:2], paste0("x", 1:4))
  for (axis in axes) {
    expect_identical(rray_split(x, axis), expected_split(x, axis))
  }
})

test_that("splitting with integer(0) axes returns list(x)", {
  x <- array(1:6, c(2, 3))
  out <- rray_split(x, integer())
  expect_equal(out, list(x))
})

test_that("dimension names on non-split axes are preserved", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))

  out <- rray_split(x, 1)
  expect_equal(dimnames(out[[1]]), list("a", c("x", "y", "z")))
  expect_equal(dimnames(out[[2]]), list("b", c("x", "y", "z")))

  out <- rray_split(x, 2)
  expect_equal(dimnames(out[[1]]), list(c("a", "b"), "x"))
  expect_equal(dimnames(out[[2]]), list(c("a", "b"), "y"))
  expect_equal(dimnames(out[[3]]), list(c("a", "b"), "z"))
})

test_that("dimension names on split axes are subset", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), c("x", "y", "z")))

  out <- rray_split(x, c(1, 2))
  expect_equal(dimnames(out[[1]]), list("a", "x"))
  expect_equal(dimnames(out[[2]]), list("b", "x"))
  expect_equal(dimnames(out[[3]]), list("a", "y"))
  expect_equal(dimnames(out[[4]]), list("b", "y"))
  expect_equal(dimnames(out[[5]]), list("a", "z"))
  expect_equal(dimnames(out[[6]]), list("b", "z"))
})

test_that("NULL dimension names are handled", {
  x <- array(1:6, c(2, 3))

  out <- rray_split(x, 1)
  expect_null(dimnames(out[[1]]))
})

test_that("dimension names that are all `NULL` are dropped", {
  x <- array(1:6, c(2, 3), dimnames = list(NULL, NULL))

  out <- rray_split(x, 1)
  expect_null(dimnames(out[[1]]))
})

test_that("partial dimension names are handled", {
  x <- array(1:6, c(2, 3), dimnames = list(c("a", "b"), NULL))

  out <- rray_split(x, 1)
  expect_equal(dimnames(out[[1]]), list("a", NULL))
  expect_equal(dimnames(out[[2]]), list("b", NULL))

  out <- rray_split(x, 2)
  expect_equal(dimnames(out[[1]]), list(c("a", "b"), NULL))
})

test_that("works with 1D arrays", {
  x <- array(1:5)
  out <- rray_split(x, 1)
  expect_length(out, 5)
  expect_equal(out[[1]], array(1L))
  expect_equal(out[[5]], array(5L))
})

test_that("works with zero size arrays", {
  x <- array(integer(), c(0, 2))
  expect_identical(rray_split(x, 1), list())
  expect_identical(
    rray_split(x, 2),
    list(array(integer(), c(0, 1)), array(integer(), c(0, 1)))
  )

  x <- array(integer(), c(0, 0))
  expect_identical(rray_split(x, 1), list())
  expect_identical(rray_split(x, 2), list())
})

test_that("works with every type", {
  expect_identical(
    rray_split(array(c(TRUE, NA, FALSE, TRUE), c(2, 2)), 2),
    list(array(c(TRUE, NA), c(2, 1)), array(c(FALSE, TRUE), c(2, 1)))
  )
  expect_identical(
    rray_split(array(c(1L, NA, 3L, 4L), c(2, 2)), 2),
    list(array(c(1L, NA), c(2, 1)), array(c(3L, 4L), c(2, 1)))
  )
  expect_identical(
    rray_split(array(c(1.5, NA, 3.5, 4.5), c(2, 2)), 2),
    list(array(c(1.5, NA), c(2, 1)), array(c(3.5, 4.5), c(2, 1)))
  )
  expect_identical(
    rray_split(array(c(1 + 1i, NA, 3 + 3i, 4 + 4i), c(2, 2)), 2),
    list(array(c(1 + 1i, NA), c(2, 1)), array(c(3 + 3i, 4 + 4i), c(2, 1)))
  )
  expect_identical(
    rray_split(array(as.raw(1:4), c(2, 2)), 2),
    list(array(as.raw(1:2), c(2, 1)), array(as.raw(3:4), c(2, 1)))
  )
  expect_identical(
    rray_split(array(c("a", NA, "c", "d"), c(2, 2)), 2),
    list(array(c("a", NA), c(2, 1)), array(c("c", "d"), c(2, 1)))
  )
  expect_identical(
    rray_split(array(list(1, "a", NULL, TRUE), c(2, 2)), 2),
    list(array(list(1, "a"), c(2, 1)), array(list(NULL, TRUE), c(2, 1)))
  )
})

test_that("axes are validated", {
  x <- array(1:6, c(2, 3))
  expect_snapshot(rray_split(x, 3), error = TRUE)
  expect_snapshot(rray_split(x, 0), error = TRUE)
  expect_snapshot(rray_split(x, c(1, 1)), error = TRUE)
  expect_snapshot(rray_split(x, c(2, 1)), error = TRUE)
})

test_that("errors on classed input", {
  x <- structure(array(1:4, c(2, 2)), class = "foo")
  expect_snapshot(rray_split(x, 1L), error = TRUE)
})
