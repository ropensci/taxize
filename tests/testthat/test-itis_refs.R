# tests for itis_refs fxn in taxize
context("itis_refs")

one <- itis_refs(202385)
two <- itis_refs(c(202385,70340))

test_that("itis_refs returns the correct class", {
  expect_that(one, is_a("list"))
  expect_that(one[[1]], is_a("data.frame"))
  expect_that(two, is_a("list"))
  expect_that(two[[1]], is_a("data.frame"))
})

test_that("itis_refs returns correct error when given nonsense", {
  expect_message(itis_refs(202385, verbose=TRUE))
})