test_that("logical wrappers are never mutated", {
  expect_no_mutation("logical")
})

test_that("integer wrappers are never mutated", {
  expect_no_mutation("integer")
})

test_that("double wrappers are never mutated", {
  expect_no_mutation("double")
})

test_that("complex wrappers are never mutated", {
  expect_no_mutation("complex")
})

test_that("raw wrappers are never mutated", {
  expect_no_mutation("raw")
})

test_that("character wrappers are never mutated", {
  expect_no_mutation("character")
})

test_that("list wrappers are never mutated", {
  expect_no_mutation("list")
})
