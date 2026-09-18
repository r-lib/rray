test_that("stacks vectors along every axis", {
  x <- 1:2
  y <- 3:4

  expect_identical(
    rray_stack(x, y, .axis = 1L),
    array(c(1L, 3L, 2L, 4L), c(2L, 2L))
  )
  expect_identical(
    rray_stack(x, y, .axis = 2L),
    array(1:4, c(2L, 2L))
  )
})

test_that("stacks matrices along every axis", {
  x <- array(1:6, c(2L, 3L))
  y <- array(7:12, c(2L, 3L))

  for (axis in 1:3) {
    out <- rray_stack(x, y, .axis = axis)
    expect_identical(dim(out), append(c(2L, 3L), 2L, after = axis - 1L))
    expect_identical(stack_slice(out, axis, 1L), x)
    expect_identical(stack_slice(out, axis, 2L), y)
  }
})

test_that("stacks 3D arrays along every axis", {
  x <- array(1:24, c(2L, 3L, 4L))
  y <- x + 24L

  for (axis in 1:4) {
    out <- rray_stack(x, y, .axis = axis)
    expect_identical(dim(out), append(c(2L, 3L, 4L), 2L, after = axis - 1L))
    expect_identical(stack_slice(out, axis, 1L), x)
    expect_identical(stack_slice(out, axis, 2L), y)
  }
})

test_that("stacks one or more than two inputs", {
  x <- array(1:4, c(2L, 2L))

  expect_identical(rray_stack(x, .axis = 1L), array(1:4, c(1L, 2L, 2L)))
  expect_identical(rray_stack(x, .axis = 3L), array(1:4, c(2L, 2L, 1L)))

  out <- rray_stack(x, x + 4L, x + 8L, .axis = 3L)
  expect_identical(dim(out), c(2L, 2L, 3L))
  expect_identical(out[,, 1L], x)
  expect_identical(out[,, 2L], x + 4L)
  expect_identical(out[,, 3L], x + 8L)

  xs <- list(x, x + 4L, x + 8L)
  expect_identical(rray_stack(!!!xs, .axis = 3L), out)
})

test_that("works with every native type", {
  xs <- list(
    lgl = c(TRUE, FALSE),
    int = 1:2,
    dbl = c(1, 2),
    cpl = c(1i, 2i),
    chr = c("a", "b"),
    raw = as.raw(1:2),
    list = list(1, "x")
  )

  for (x in xs) {
    out <- rray_stack(x, x, .axis = 2L)
    expect_identical(dim(out), c(2L, 2L))
    expect_identical(out[, 1L], x)
    expect_identical(out[, 2L], x)
  }
})

test_that("uses the common type", {
  expect_type(rray_stack(TRUE, 2L, .axis = 1L), "integer")
  expect_type(rray_stack(TRUE, 2, .axis = 1L), "double")
  expect_type(rray_stack(1L, 2i, .axis = 1L), "complex")

  expect_identical(
    rray_stack("a", "b", .axis = 1L),
    array(c("a", "b"), c(2L, 1L))
  )
  expect_identical(
    rray_stack(list(1L), list("x"), .axis = 1L),
    array(list(1L, "x"), c(2L, 1L))
  )
})

test_that("broadcasts existing axes", {
  x <- array(1:2, c(2L, 1L))
  y <- array(1:4, c(2L, 2L))

  out <- rray_stack(x, y, .axis = 1L)
  expect_identical(dim(out), c(2L, 2L, 2L))
  expect_identical(stack_slice(out, 1L, 1L), rray_broadcast(x, c(2L, 2L)))
  expect_identical(stack_slice(out, 1L, 2L), y)

  x <- array(1:6, c(2L, 1L, 3L))
  y <- array(1:12, c(1L, 4L, 3L))

  out <- rray_stack(x, y, .axis = 2L)
  expect_identical(dim(out), c(2L, 2L, 4L, 3L))
  expect_identical(
    stack_slice(out, 2L, 1L),
    rray_broadcast(x, c(2L, 4L, 3L))
  )
  expect_identical(
    stack_slice(out, 2L, 2L),
    rray_broadcast(y, c(2L, 4L, 3L))
  )
})

test_that("broadcasts inputs of differing dimensionality", {
  x <- 1:2
  y <- array(1:6, c(2L, 3L))

  out <- rray_stack(x, y, .axis = 1L)
  expect_identical(dim(out), c(2L, 2L, 3L))
  expect_identical(stack_slice(out, 1L, 1L), rray_broadcast(x, c(2L, 3L)))
  expect_identical(stack_slice(out, 1L, 2L), y)

  out <- rray_stack(x, y, .axis = 3L)
  expect_identical(dim(out), c(2L, 3L, 2L))
  expect_identical(stack_slice(out, 3L, 1L), rray_broadcast(x, c(2L, 3L)))
  expect_identical(stack_slice(out, 3L, 2L), y)
})

test_that("expands trailing axes only as far as the new axis requires", {
  x <- 1:2
  y <- array(1:24, c(2L, 3L, 4L))

  out <- rray_stack(x, y, .axis = 4L)
  expect_identical(dim(out), c(2L, 3L, 4L, 2L))
  expect_identical(
    stack_slice(out, 4L, 1L),
    rray_broadcast(x, c(2L, 3L, 4L))
  )
  expect_identical(stack_slice(out, 4L, 2L), y)
})

test_that("expansion keeps existing names and leaves new axes unnamed", {
  x <- array(1:2, 2L, dimnames = list(c("a", "b")))
  y <- array(
    1:6,
    c(2L, 3L),
    dimnames = list(c("a", "b"), c("c", "d", "e"))
  )

  out <- rray_stack(x, y, .axis = 3L)
  expect_identical(dimnames(out), list(c("a", "b"), c("c", "d", "e"), NULL))
})

test_that("handles zero dimensions", {
  x <- array(integer(), c(0L, 2L))

  out <- rray_stack(x, x, .axis = 1L)
  expect_identical(out, array(integer(), c(2L, 0L, 2L)))

  out <- rray_stack(x, x, .axis = 3L)
  expect_identical(out, array(integer(), c(0L, 2L, 2L)))
})

test_that("names of `...` name the new axis", {
  out <- rray_stack(first = 1:2, second = 3:4, .axis = 2L)
  expect_identical(dimnames(out), list(NULL, c("first", "second")))

  x <- array(1:2, 2L, dimnames = list(c("a", "b")))
  out <- rray_stack(p = x, q = x, .axis = 2L)
  expect_identical(dimnames(out), list(c("a", "b"), c("p", "q")))
})

test_that("partially named inputs use empty strings", {
  out <- rray_stack(first = 1:2, 3:4, .axis = 1L)
  expect_identical(dimnames(out), list(c("first", ""), NULL))
})

test_that("the new axis is unnamed when `...` is unnamed", {
  expect_null(dimnames(rray_stack(1:2, 3:4, .axis = 1L)))
})

test_that("keeps existing axis names under broadcasting", {
  x <- array(1:2, c(2L, 1L), dimnames = list(c("r1", "r2"), "x"))
  y <- array(1:6, c(2L, 3L), dimnames = list(NULL, c("a", "b", "c")))

  out <- rray_stack(x, y, .axis = 1L)
  expect_identical(dim(out), c(2L, 2L, 3L))
  expect_identical(
    dimnames(out),
    list(NULL, c("r1", "r2"), c("a", "b", "c"))
  )
})

test_that("does not modify inputs", {
  x <- array(1:2, c(2L, 1L), dimnames = list(c("a", "b"), "x"))
  y <- array(3:8, c(2L, 3L))
  expected_x <- x
  expected_y <- y

  rray_stack(x, y, .axis = 1L)

  expect_identical(x, expected_x)
  expect_identical(y, expected_y)
})

test_that("validates inputs and axis", {
  x <- array(1:4, c(2L, 2L))

  expect_snapshot(error = TRUE, {
    rray_stack(.axis = 1L)
    rray_stack(x)
    rray_stack(x, .axis = integer())
    rray_stack(x, .axis = c(1L, 2L))
    rray_stack(x, .axis = NA_integer_)
    rray_stack(x, .axis = structure(1L, foo = "bar"))
    rray_stack(x, .axis = 1.5)
    rray_stack(x, .axis = 0L)
    rray_stack(x, .axis = 4L)
  })
})

test_that("rejects a dimensionality above the maximum", {
  x <- array(1, rep(1L, 64L))
  expect_snapshot(rray_stack(x, x, .axis = 1L), error = TRUE)
})

test_that("rejects incompatible inputs", {
  expect_snapshot(error = TRUE, {
    rray_stack(array(1:4, c(2L, 2L)), array(1:6, c(3L, 2L)), .axis = 1L)
    rray_stack(x = array(1, c(2L, 2L)), y = array(1, c(3L, 2L)), .axis = 3L)
    rray_stack(x = 1L, y = "x", .axis = 1L)
    rray_stack(x = NULL, y = 1L, .axis = 1L)
    rray_stack(x = mean, y = 1L, .axis = 1L)
    rray_stack(x = structure(array(1L), class = "foo"), y = 1L, .axis = 1L)
  })
})
