# tests for itis_ping fxn in taxize
context("itis_ping")

one <- itis_ping()

test_that("itis_ping returns the correct class", {
  expect_is(one, "logical")
})

test_that("itis_ping returns correct things", {
  expect_false(itis_ping(503))
  expect_true(itis_ping(200))
})
