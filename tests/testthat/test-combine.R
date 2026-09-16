test_that("combines arrays along every axis", {
  expect_identical(
    rray_combine(1:2, 3:5, .axis = 1L),
    array(1:5, 5L)
  )

  x <- array(1:6, c(2L, 3L))
  y <- array(7:12, c(2L, 3L))

  out <- rray_combine(x, y, .axis = 1L)
  expect_identical(dim(out), c(4L, 3L))
  expect_identical(out[1:2, ], x)
  expect_identical(out[3:4, ], y)

  out <- rray_combine(x, y, .axis = 2L)
  expect_identical(dim(out), c(2L, 6L))
  expect_identical(out[, 1:3], x)
  expect_identical(out[, 4:6], y)

  z <- array(1:24, c(2L, 3L, 4L))
  for (axis in seq_along(dim(z))) {
    out <- rray_combine(z, z + 24L, .axis = axis)
    index <- lapply(dim(out), seq_len)
    index[[axis]] <- seq_len(dim(z)[[axis]])
    expect_identical(do.call(`[`, c(list(out), index, list(drop = FALSE))), z)
    index[[axis]] <- dim(z)[[axis]] + seq_len(dim(z)[[axis]])
    expect_identical(
      do.call(`[`, c(list(out), index, list(drop = FALSE))),
      z + 24L
    )
  }
})

test_that("combines one or more than two inputs", {
  x <- array(1:4, c(2L, 2L))
  expect_identical(rray_combine(x, .axis = 1L), x)

  out <- rray_combine(x, x + 4L, x + 8L, .axis = 2L)
  expect_identical(dim(out), c(2L, 6L))
  expect_identical(out[, 1:2], x)
  expect_identical(out[, 3:4], x + 4L)
  expect_identical(out[, 5:6], x + 8L)

  xs <- list(x, x + 4L, x + 8L)
  expect_identical(
    rlang::inject(rray_combine(!!!xs, .axis = 2L)),
    out
  )
})

test_that("uses the common type", {
  expect_type(rray_combine(TRUE, 2L, .axis = 1L), "integer")
  expect_type(rray_combine(TRUE, 2, .axis = 1L), "double")
  expect_type(rray_combine(1L, 2i, .axis = 1L), "complex")

  expect_identical(
    rray_combine("a", "b", .axis = 1L),
    array(c("a", "b"), 2L)
  )
  expect_identical(
    rray_combine(as.raw(1), as.raw(2), .axis = 1L),
    array(as.raw(1:2), 2L)
  )
  expect_identical(
    rray_combine(list(1L), list("x"), .axis = 1L),
    array(list(1L, "x"), 2L)
  )
})

test_that("broadcasts every non-combine axis", {
  x <- array(1:2, c(2L, 1L, 1L))
  y <- array(3:8, c(3L, 2L))
  out <- rray_combine(x, y, .axis = 1L)

  expect_identical(dim(out), c(5L, 2L, 1L))
  expect_identical(out[1:2, , 1L], x[, rep(1L, 2L), 1L])
  expect_identical(out[3:5, , 1L], y)

  x <- array(1:6, c(2L, 1L, 3L))
  y <- array(7:12, c(3L, 2L))
  out <- rray_combine(x, y, .axis = 1L)

  expect_identical(dim(out), c(5L, 2L, 3L))
  for (i in 1:3) {
    expect_identical(out[1:2, , i], x[, rep(1L, 2L), i])
    expect_identical(out[3:5, , i], y)
  }

  x <- array(1:2, c(1L, 2L, 1L))
  y <- array(3:26, c(3L, 4L, 2L))
  out <- rray_combine(x, y, .axis = 2L)
  expect_identical(dim(out), c(3L, 6L, 2L))
  expect_identical(out[, 1:2, ], rray_broadcast(x, c(3L, 2L, 2L)))
  expect_identical(out[, 3:6, ], y)
})

test_that("combines on an implicit trailing axis", {
  x <- 1:2
  y <- array(3:8, c(2L, 3L))
  out <- rray_combine(x, y, .axis = 2L)

  expect_identical(dim(out), c(2L, 4L))
  expect_identical(out[, 1L], x)
  expect_identical(out[, 2:4], y)
})

test_that("handles zero dimensions", {
  x <- array(integer(), c(0L, 2L))
  y <- array(1:6, c(3L, 2L))
  expect_identical(rray_combine(x, y, .axis = 1L), y)

  x <- array(integer(), c(2L, 0L))
  y <- array(1:2, c(2L, 1L))
  expect_identical(
    rray_combine(x, y, .axis = 1L),
    array(integer(), c(4L, 0L))
  )
})

test_that("combines axis names and broadcasts other names", {
  x <- array(
    1:4,
    c(2L, 2L),
    dimnames = list(c("x1", "x2"), c("a", "b"))
  )
  y <- array(
    5:6,
    c(1L, 2L),
    dimnames = list(NULL, c("c", "d"))
  )

  out <- rray_combine(first = x, second = y, .axis = 1L)
  expect_identical(
    dimnames(out),
    list(c("x1", "x2", ""), c("a", "b"))
  )

  x <- array(1:2, c(2L, 1L), dimnames = list(c("r1", "r2"), "x"))
  y <- array(3:8, c(2L, 3L), dimnames = list(NULL, letters[1:3]))
  out <- rray_combine(x, y, .axis = 1L)
  expect_identical(dimnames(out), list(c("r1", "r2", "", ""), letters[1:3]))

  x <- array(1:2, 2L, dimnames = list(c("a", NA_character_)))
  out <- rray_combine(named = x, unnamed = 3L, .axis = 1L)
  expect_identical(dimnames(out), list(c("a", NA_character_, "")))
})

test_that("does not modify inputs", {
  x <- array(1:2, c(2L, 1L), dimnames = list(c("a", "b"), "x"))
  y <- array(3:8, c(2L, 3L))
  expected_x <- x
  expected_y <- y

  rray_combine(x, y, .axis = 1L)

  expect_identical(x, expected_x)
  expect_identical(y, expected_y)
})

test_that("validates inputs and axis", {
  x <- array(1:4, c(2L, 2L))

  expect_snapshot(error = TRUE, {
    rray_combine(.axis = 1L)
    rray_combine(x)
    rray_combine(x, .axis = integer())
    rray_combine(x, .axis = c(1L, 2L))
    rray_combine(x, .axis = NA_integer_)
    rray_combine(x, .axis = structure(1L, foo = "bar"))
    rray_combine(x, .axis = 1.5)
    rray_combine(x, .axis = 0L)
    rray_combine(x, .axis = 3L)
  })
})

test_that("rejects incompatible inputs", {
  expect_snapshot(error = TRUE, {
    rray_combine(array(1:4, c(2L, 2L)), array(1:6, c(3L, 2L)), .axis = 2L)
    rray_combine(x = 1L, y = "x", .axis = 1L)
    rray_combine(x = NULL, y = 1L, .axis = 1L)
    rray_combine(x = mean, y = 1L, .axis = 1L)
    rray_combine(
      x = structure(array(1L), class = "foo"),
      y = 1L,
      .axis = 1L
    )
  })
})
