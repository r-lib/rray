# ------------------------------------------------------------------------------
# rray_slice()

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
  expect_identical(
    rray_slice(x, c(NA_integer_, 2L)),
    array(c(NA_integer_, 2L), 2L)
  )
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

test_that("slices four dimensions and falls back for higher dimensions", {
  x4 <- array(seq_len(120L), c(2L, 3L, 4L, 5L))

  expect_identical(
    rray_slice(x4, TRUE, c(3L, 1L), c(4L, 2L), c(5L, 1L)),
    x4[, c(3L, 1L), c(4L, 2L), c(5L, 1L), drop = FALSE]
  )
  expect_identical(
    rray_slice(x4, TRUE, c(NA_integer_, 1L), c(4L, 2L), c(5L, 1L)),
    x4[, c(NA_integer_, 1L), c(4L, 2L), c(5L, 1L), drop = FALSE]
  )

  x5 <- array(seq_len(240L), c(2L, 3L, 4L, 5L, 2L))

  expect_identical(
    rray_slice(x5, TRUE, c(3L, 1L), c(4L, 2L), c(5L, 1L), c(2L, 1L)),
    x5[, c(3L, 1L), c(4L, 2L), c(5L, 1L), c(2L, 1L), drop = FALSE]
  )
  expect_identical(
    rray_slice(x5, TRUE, c(NA_integer_, 1L), c(4L, 2L), c(5L, 1L), c(2L, 1L)),
    x5[,
      c(NA_integer_, 1L),
      c(4L, 2L),
      c(5L, 1L),
      c(2L, 1L),
      drop = FALSE
    ]
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

# ------------------------------------------------------------------------------
# rray_slice_assign()

test_that("assigns the Cartesian product of the subscripts", {
  x <- array(1:24, c(2L, 3L, 4L))
  value <- array(101:104, c(2L, 2L, 1L))
  expect <- x
  expect[2:1, c(3L, 1L), 2L] <- value

  expect_identical(
    rray_slice_assign(x, 2:1, c(3L, 1L), 2L, value = value),
    expect
  )
})

test_that("`TRUE` assigns a whole axis", {
  x <- array(1:24, c(2L, 3L, 4L))
  expect <- x
  expect[1L, , ] <- 0L

  expect_identical(rray_slice_assign(x, 1L, TRUE, TRUE, value = 0L), expect)
  expect_identical(
    rray_slice_assign(x, TRUE, TRUE, TRUE, value = 0L),
    array(0L, c(2L, 3L, 4L))
  )
})

test_that("assignment matches base R for every kind of subscript", {
  x <- array(1:60, c(3L, 4L, 5L))
  subscripts <- list(
    list(c(3L, 1L, 3L), 2:4, c(5L, 1L)),
    list(c(3, 1), c(4, 0, 2), 5:1),
    list(-2L, c(-1, -4), -(2:5)),
    list(c(TRUE, FALSE, TRUE), TRUE, c(FALSE, TRUE, TRUE, FALSE, TRUE)),
    list(seq(1L, 3L, by = 2L), 4:1, seq(5, 1, by = -2)),
    list(integer(), 1L, TRUE),
    list(FALSE, 0L, 1L)
  )

  for (i in subscripts) {
    dimensions <- dim(rray_slice(x, !!!i))
    value <- array(-seq_len(prod(dimensions)), dimensions)
    expect <- x
    expect[i[[1]], i[[2]], i[[3]]] <- value

    expect_identical(rray_slice_assign(x, !!!i, value = value), expect)
  }
})

test_that("agrees with a read of the same locations", {
  x <- array(1:60, c(3L, 4L, 5L))
  subscripts <- list(
    list(c(3L, 1L), 2:4, c(5L, 1L)),
    list(-2L, TRUE, c(TRUE, FALSE, TRUE, FALSE, TRUE)),
    list(TRUE, TRUE, 3L)
  )

  for (i in subscripts) {
    dimensions <- dim(rray_slice(x, !!!i))
    value <- array(-seq_len(prod(dimensions)), dimensions)
    out <- rray_slice_assign(x, !!!i, value = value)

    expect_identical(rray_slice(out, !!!i), value)
  }
})

test_that("missing locations use a value slot without assigning", {
  x <- array(1:6, c(2L, 3L))
  value <- array(c(10L, 20L, 30L, 40L), c(2L, 2L))

  expect_identical(
    rray_slice_assign(x, c(NA, 2L), c(3L, 1L), value = value),
    array(c(1L, 40L, 3L, 4L, 5L, 20L), c(2L, 3L))
  )
  expect_identical(
    rray_slice_assign(x, c(NA, 2), c(3, 1), value = value),
    array(c(1L, 40L, 3L, 4L, 5L, 20L), c(2L, 3L))
  )
  expect_identical(
    rray_slice_assign(x, c(2L, 1L), c(NA, 3L), value = value),
    array(c(1:4, 40L, 30L), c(2L, 3L))
  )
  expect_identical(
    rray_slice_assign(x, c(NA, TRUE), c(TRUE, FALSE, TRUE), value = value),
    array(c(1L, 20L, 3:5, 40L), c(2L, 3L))
  )
  expect_identical(
    rray_slice_assign(
      x,
      c(NA, 2L),
      c(3L, 1L),
      value = array(c(10L, 30L), c(1L, 2L))
    ),
    array(c(1L, 30L, 3L, 4L, 5L, 10L), c(2L, 3L))
  )
  expect_identical(rray_slice_assign(x, NA, TRUE, value = 0L), x)
  expect_identical(
    rray_slice_assign(x, TRUE, NA, value = array(-(1:6), c(2L, 3L))),
    x
  )
})

test_that("the last assignment to a repeated location wins", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(
    rray_slice_assign(
      x,
      c(1L, 1L),
      c(2L, 2L),
      value = array(c(10L, 20L, 30L, 40L), c(2L, 2L))
    ),
    array(c(1:2, 40L, 4:6), c(2L, 3L))
  )
})

test_that("broadcasts `value` to the dimensions of the selection", {
  x <- array(1:24, c(2L, 3L, 4L))
  values <- list(
    0L,
    array(c(10L, 20L), 2L),
    array(c(10L, 20L), c(1L, 2L)),
    array(c(10L, 20L), c(1L, 1L, 2L)),
    array(-(1:4), c(2L, 2L)),
    array(-(1:4), c(2L, 1L, 2L)),
    array(-(1:4), c(1L, 2L, 2L))
  )

  for (value in values) {
    expect <- x
    expect[, c(1L, 3L), 2:3] <- rray_broadcast(value, c(2L, 2L, 2L))

    expect_identical(
      rray_slice_assign(x, TRUE, c(1L, 3L), 2:3, value = value),
      expect
    )
    expect_identical(
      rray_slice_assign(x, 1:2, c(1L, 3L), 2:3, value = value),
      expect
    )
  }
})

test_that("errors when `value` can't broadcast to the selection", {
  x <- array(1:24, c(2L, 3L, 4L))

  expect_snapshot(error = TRUE, {
    rray_slice_assign(x, TRUE, 1:2, 1L, value = 1:3)
    rray_slice_assign(x, 1L, 1:2, 1L, value = 1:2)
    rray_slice_assign(x, TRUE, TRUE, TRUE, value = 1:4)
    rray_slice_assign(x, c(1L, NA), 1L, 1L, value = 1:3)
    rray_slice_assign(x, 1L, 1L, 1L, value = array(0L, c(1L, 1L, 1L, 1L)))
  })
})

test_that("assigns every native storage type", {
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
    value <- slice_base(x, 2:1, 3:2)
    expect <- x
    expect[1:2, c(1L, 3L)] <- value

    expect_identical(
      rray_slice_assign(x, 1:2, c(1L, 3L), value = value),
      expect
    )
    expect_identical(
      rray_slice_assign(x, TRUE, c(1L, 3L), value = value),
      expect
    )
    expect_identical(
      rray_slice_assign(
        x,
        c(1L, NA, 2L),
        c(1L, 3L),
        value = slice_base(x, c(2L, 1L, 1L), 3:2)
      ),
      expect
    )
  }
})

test_that("assigns with every dimensionality", {
  for (dimensionality in 1:5) {
    dimensions <- seq_len(dimensionality) + 1L
    x <- array(seq_len(prod(dimensions)), dimensions)
    value <- array(-seq_len(2L^dimensionality), rep(2L, dimensionality))

    i <- rep(list(c(2L, 1L)), dimensionality)
    expect <- do.call(`[<-`, c(list(x), i, list(value = value)))
    expect_identical(rray_slice_assign(x, !!!i, value = value), expect)

    i[[1]] <- TRUE
    expect <- do.call(`[<-`, c(list(x), i, list(value = value)))
    expect_identical(rray_slice_assign(x, !!!i, value = value), expect)

    i[[dimensionality]] <- 1L
    expect <- do.call(`[<-`, c(list(x), i, list(value = 0L)))
    i[[dimensionality]] <- c(NA, 1L)
    expect_identical(rray_slice_assign(x, !!!i, value = 0L), expect)
  }
})

test_that("assigns with the maximum dimensionality", {
  x <- array(1:4, c(rep(1L, 62L), 2L, 2L))
  indices <- c(rep(list(TRUE), 62L), list(2L, c(2L, 1L)))

  expect_identical(
    rray_slice_assign(x, !!!indices, value = 0L),
    array(c(1L, 0L, 3L, 0L), dim(x))
  )
  expect_identical(
    rray_slice_assign(
      x,
      !!!indices,
      value = array(c(10L, 20L), c(rep(1L, 63L), 2L))
    ),
    array(c(1L, 20L, 3L, 10L), dim(x))
  )
})

test_that("empty targets accept a value that broadcasts to them", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(rray_slice_assign(x, NULL, TRUE, value = 0L), x)
  expect_identical(
    rray_slice_assign(x, NULL, TRUE, value = array(integer(), c(0L, 3L))),
    x
  )
  expect_identical(rray_slice_assign(x, 1L, FALSE, value = 0L), x)

  x <- array(integer(), c(2L, 0L, 3L))
  expect_identical(rray_slice_assign(x, TRUE, TRUE, TRUE, value = 0L), x)
  expect_identical(
    rray_slice_assign(
      x,
      2L,
      TRUE,
      c(3L, 1L),
      value = array(integer(), c(1L, 0L, 2L))
    ),
    x
  )
})

test_that("keeps the type, dimensions, and names of `x`", {
  x <- array(
    c(1, 2, 3, 4),
    c(2L, 2L),
    dimnames = list(c("r1", "r2"), c("c1", "c2"))
  )
  value <- array(c(TRUE, FALSE), c(1L, 2L), dimnames = list("a", c("b", "c")))

  expect_identical(
    rray_slice_assign(x, "r2", c("c2", "c1"), value = value),
    array(
      c(1, 0, 3, 1),
      c(2L, 2L),
      dimnames = list(c("r1", "r2"), c("c1", "c2"))
    )
  )
  expect_identical(
    rray_slice_assign(x, c(NA, "r2"), "c1", value = c(5, 6)),
    array(
      c(1, 6, 3, 4),
      c(2L, 2L),
      dimnames = list(c("r1", "r2"), c("c1", "c2"))
    )
  )
})

test_that("drops every other attribute of `x`", {
  x <- array(1:4, c(2L, 2L), dimnames = list(c("r1", "r2"), NULL))
  attr(x, "foo") <- "bar"

  expect_identical(
    rray_slice_assign(x, 1L, 1L, value = 0L),
    array(c(0L, 2:4), c(2L, 2L), dimnames = list(c("r1", "r2"), NULL))
  )

  x <- structure(1:2, foo = "bar")
  expect_identical(rray_slice_assign(x, 1L, value = 0L), array(c(0L, 2L)))
})

test_that("a bare vector `x` becomes a 1D array", {
  expect_identical(
    rray_slice_assign(1:3, 2L, value = 0L),
    array(c(1L, 0L, 3L))
  )
  expect_identical(
    rray_slice_assign(c(a = 1L, b = 2L), "b", value = 0L),
    array(c(1L, 0L), dimnames = list(c("a", "b")))
  )
})

test_that("does not modify `x` or `value`", {
  xs <- list(
    array(1:6, c(2L, 3L)),
    array(c(1, 2, 3, 4, 5, 6), c(2L, 3L)),
    array(letters[1:6], c(2L, 3L)),
    array(as.list(1:6), c(2L, 3L))
  )

  for (x in xs) {
    original <- rlang::duplicate(x)
    value <- slice_base(x, 2:1, 3:1)
    original_value <- rlang::duplicate(value)

    rray_slice_assign(x, TRUE, TRUE, value = value)
    rray_slice_assign(x, c(2L, NA), TRUE, value = value)

    expect_identical(x, original)
    expect_identical(value, original_value)
  }
})

test_that("`value` can be `x`", {
  x <- array(1:6, c(2L, 3L))

  expect_identical(rray_slice_assign(x, TRUE, TRUE, value = x), x)
  expect_identical(
    rray_slice_assign(x, 2:1, TRUE, value = x),
    slice_base(x, 2:1, )
  )
})

test_that("casts `value` to the type of `x`", {
  expect_identical(
    rray_slice_assign(array(c(1.5, 2.5, 3.5)), 2L, value = TRUE),
    array(c(1.5, 1, 3.5))
  )
  expect_identical(
    rray_slice_assign(array(1:3), 2L, value = 5),
    array(c(1L, 5L, 3L))
  )

  expect_snapshot(error = TRUE, {
    rray_slice_assign(array(1:3), 2L, value = 1.5)
    rray_slice_assign(array(1:3), 2L, value = "a")
    rray_slice_assign(array(1:3), 2L, value = NULL)
  })
})

test_that("assignment requires one unnamed subscript per axis", {
  x <- array(1:6, c(2L, 3L))

  expect_snapshot(error = TRUE, {
    rray_slice_assign(x, 1L, value = 0L)
    rray_slice_assign(x, 1L, 1L, 1L, value = 0L)
    rray_slice_assign(x, i = 1L, 1L, value = 0L)
  })
})

test_that("reports subscript errors from `rray_slice_assign()`", {
  x <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), NULL))

  expect_snapshot(error = TRUE, {
    rray_slice_assign(x, 1L, 4L, value = 0L)
    rray_slice_assign(x, "z", 1L, value = 0L)
    rray_slice_assign(x, c(-1L, NA), 1L, value = 0L)
  })
})

test_that("errors on unsupported `x` inputs to `rray_slice_assign()`", {
  expect_snapshot(error = TRUE, {
    rray_slice_assign(NULL, 1L, value = 0L)
    rray_slice_assign(structure(1:2, class = "foo"), 1L, value = 0L)
  })
})
