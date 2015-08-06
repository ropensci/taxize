# tests for tnrs_sources fxn in taxize
context("tnrs_sources")

out <- tnrs_sources()

test_that("tnrs_sources returns the correct value", {
  expect_that(length(out), equals(3))
})

test_that("tnrs_sources returns the correct class", {
  expect_that(out, is_a("character"))
})
