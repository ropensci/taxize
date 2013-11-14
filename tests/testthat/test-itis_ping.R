# tests for itis_ping fxn in taxize
context("itis_ping")

one <- itis_ping()

test_that("itis_ping returns the correct class", {
  expect_that(one, is_a("character"))
})

test_that("itis_ping returns correct error when given nonsense", {
  expect_error(itis_ping("stuff"))
})