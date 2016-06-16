# tests for tnrs_sources fxn in taxize
context("tnrs_sources")


test_that("tnrs_sources returns the correct value", {
  skip_on_cran()

  out <- tnrs_sources()

  expect_that(length(out), equals(3))

  expect_that(out, is_a("character"))
})
