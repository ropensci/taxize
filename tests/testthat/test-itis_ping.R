# tests for itis_ping fxn in taxize
context("itis_ping")


test_that("itis_ping returns the correct class", {
  skip_on_cran()

  one <- itis_ping()

  expect_is(one, "logical")
})

test_that("itis_ping returns correct things", {
  skip_on_cran()

  expect_false(itis_ping(503))
  expect_true(itis_ping(200))
})
