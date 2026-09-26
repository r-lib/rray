test_that("selects the Cartesian product of the subscripts", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_slice(x, c(2L, 1L), c(3L, 1L), 2L),
    array(c(12L, 11L, 8L, 7L), c(2L, 2L, 1L))
  )
})

test_that("`TRUE` selects a whole axis", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(rray_slice(x, 1L, TRUE, TRUE), slice_base(x, 1L, , ))
  expect_identical(rray_slice(x, TRUE, 1L, TRUE), slice_base(x, , 1L, ))
  expect_identical(rray_slice(x, TRUE, TRUE, TRUE), x)
})

test_that("scalar `TRUE` works on each axis with other subscripts", {
  x <- array(
    1:24,
    c(2L, 3L, 4L),
    dimnames = list(letters[1:2], letters[3:5], letters[6:9])
  )
  whole <- TRUE

  expect_identical(
    rray_slice(x, whole, c(3L, 1L), whole),
    slice_base(x, , c(3L, 1L), )
  )
  expected <- slice_base(x, c(NA_integer_, 2L), , c(4L, 1L))
  dimnames(expected)[[1]] <- c("", "b")
  expect_identical(
    rray_slice(x, c(NA_integer_, 2L), whole, c(4L, 1L)),
    expected
  )
  expect_identical(
    rray_slice(x, array(TRUE, 1L), whole, 2L),
    slice_base(x, , , 2L)
  )
})

test_that("matches base R for every kind of subscript", {
  x <- array(1:60, c(3L, 4L, 5L))
  subscripts <- list(
    list(c(3L, 1L, 3L), 2:4, c(5L, 1L)),
    list(c(3, 1), c(4, 0, 2), 5:1),
    list(-2L, c(-1, -4), -(2:5)),
    list(c(TRUE, FALSE, TRUE), TRUE, c(FALSE, TRUE, TRUE, FALSE, TRUE)),
    list(c(NA, 2L), c(1, NA, 4), NA),
    list(seq(1L, 3L, by = 2L), 4:1, seq(5, 1, by = -2)),
    list(integer(), 1L, TRUE),
    list(FALSE, 0L, 1L)
  )

  for (i in subscripts) {
    expect_identical(
      rray_slice(x, !!!i),
      slice_base(x, i[[1]], i[[2]], i[[3]])
    )
  }
})

test_that("returns every native storage type", {
  xs <- list(
    c(TRUE, FALSE, NA, TRUE, FALSE, TRUE),
    1:6,
    c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5),
    c(1i, 2i, 3i, 4i, 5i, 6i),
    letters[1:6],
    as.raw(1:6),
    list("a", 2L, NULL, 4, "e", 6i)
  )

  for (x in xs) {
    x <- array(x, c(2L, 3L))
    expect_identical(
      rray_slice(x, c(2L, 1L), c(3L, 1L)),
      slice_base(x, 2:1, c(3L, 1L))
    )
    expect_identical(
      rray_slice(x, c(2L, NA), c(NA, 1L)),
      slice_base(x, c(2L, NA), c(NA, 1L))
    )
  }
})

test_that("a missing location on any axis gives a missing value", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_slice(x, TRUE, c(1L, NA), c(NA, 4L)),
    slice_base(x, , c(1L, NA), c(NA, 4L))
  )
  expect_identical(
    rray_slice(x, NA, TRUE, 1L),
    array(NA_integer_, c(2L, 3L, 1L))
  )
})

test_that("integer locations work with converted subscripts and missing axes", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_identical(
    rray_slice(x, c(NA_integer_, 2L), c(3, NA_real_), c(TRUE, FALSE, NA, TRUE)),
    slice_base(x, c(NA_integer_, 2L), c(3, NA_real_), c(TRUE, FALSE, NA, TRUE))
  )
})

test_that("works with a bare vector", {
  expect_identical(rray_slice(1:5, c(5L, 1L)), array(c(5L, 1L), 2L))
  expect_identical(
    rray_slice(c(a = 1L, b = 2L), c(2L, 1L)),
    array(c(2L, 1L), 2L, dimnames = list(c("b", "a")))
  )
})

test_that("works with one dimension", {
  x <- array(1:3)

  expect_identical(rray_slice(x, c(3L, 1L)), array(c(3L, 1L), 2L))
  expect_identical(rray_slice(x, TRUE), x)
})

test_that("works with zero dimensions", {
  x <- array(integer(), c(2L, 0L, 3L))

  expect_identical(
    rray_slice(x, 2L, TRUE, c(3L, 1L)),
    array(integer(), c(1L, 0L, 2L))
  )
  expect_identical(
    rray_slice(x, TRUE, NULL, TRUE),
    array(integer(), c(2L, 0L, 3L))
  )
})

test_that("empty selections give a dimension of zero", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(rray_slice(x, NULL, TRUE), array(integer(), c(0L, 3L)))
  expect_identical(rray_slice(x, 1L, FALSE), array(integer(), c(1L, 0L)))
  expect_identical(rray_slice(x, 0L, 0L), array(integer(), c(0L, 0L)))
  expect_identical(rray_slice(x, numeric(), TRUE), array(integer(), c(0L, 3L)))
})

test_that("works with the maximum dimensionality", {
  x <- array(1:4, c(rep(1L, 62L), 2L, 2L))
  indices <- c(rep(list(TRUE), 62L), list(2L, c(2L, 1L)))

  expect_identical(
    rray_slice(x, !!!indices),
    array(c(4L, 2L), c(rep(1L, 62L), 1L, 2L))
  )
})

test_that("matches `rray_index()` over an open mesh of locations", {
  x <- array(1:60, c(3L, 4L, 5L))
  i <- c(3L, NA, 1L)
  j <- c(2L, 2L)
  k <- c(5L, 1L, 4L, 2L)

  expect_identical(
    rray_slice(x, i, j, k),
    rray_index(
      x,
      array(i, c(3L, 1L, 1L)),
      array(j, c(1L, 2L, 1L)),
      array(k, c(1L, 1L, 4L))
    )
  )
})

test_that("selects names along with the values", {
  x <- array(
    1:24,
    c(2L, 3L, 4L),
    dimnames = list(c("a", "b"), c("c", "d", "e"), NULL)
  )

  expect_identical(
    rray_slice(x, c(2L, 1L, 2L), c("e", "c"), 1L),
    slice_base(x, c(2L, 1L, 2L), c("e", "c"), 1L)
  )
})

test_that("a missing location has an empty name", {
  x <- array(
    1:24,
    c(2L, 3L, 4L),
    dimnames = list(c("a", "b"), c("c", "d", "e"), NULL)
  )

  expected <- slice_base(x, c(1L, NA), -2L, )
  dimnames(expected)[[1]] <- c("a", "")
  expect_identical(rray_slice(x, c(1L, NA), -2L, TRUE), expected)

  expect_identical(
    rray_slice(x, c(TRUE, FALSE), c(NA, "d"), 4L),
    array(
      c(NA, 21L),
      c(1L, 2L, 1L),
      dimnames = list("a", c("", "d"), NULL)
    )
  )
})

test_that("keeps names of a whole axis", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("c", "d", "e")))

  expect_identical(rray_slice(x, TRUE, TRUE), x)
  expect_identical(rray_slice(x, 1:2, 3L), slice_base(x, , 3L))
})

test_that("an empty selection has no names", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("c", "d", "e")))

  expect_identical(
    rray_slice(x, NULL, 2L),
    array(integer(), c(0L, 1L), dimnames = list(NULL, "d"))
  )
  expect_identical(rray_slice(x, numeric(), TRUE), slice_base(x, numeric(), ))
})

test_that("subscripts can be spliced into `...`", {
  x <- array(1:24, c(2L, 3L, 4L))
  indices <- list(TRUE, 2L, c(4L, 1L))

  expect_identical(rray_slice(x, !!!indices), slice_base(x, , 2L, c(4L, 1L)))
})

test_that("ignores a trailing empty argument", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(rray_slice(x, 1L, TRUE, ), rray_slice(x, 1L, TRUE))
})

test_that("requires one subscript per axis", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_snapshot(error = TRUE, {
    rray_slice(x)
    rray_slice(x, 1L, 1L)
    rray_slice(x, 1L, 1L, 1L, 1L)
    rray_slice(1:3)
  })
})

test_that("requires unnamed subscripts", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_slice(x, i = 1L, 1L)
  })
})

test_that("reports subscript errors with their position", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), NULL))

  expect_snapshot(error = TRUE, {
    rray_slice(x, 1L, 4L)
    rray_slice(x, "z", 1L)
    rray_slice(x, 1L, "c")
    rray_slice(x, c(TRUE, FALSE, TRUE), 1L)
    rray_slice(x, matrix(1L), 1L)
  })
})

test_that("errors on unsupported `x` inputs", {
  expect_snapshot(error = TRUE, {
    rray_slice(NULL, 1L)
    rray_slice(mean, 1L)
    rray_slice(structure(1:2, class = "foo"), 1L)
  })
})
