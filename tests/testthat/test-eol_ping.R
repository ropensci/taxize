# tests for eol_ping fxn in taxize
context("eol_ping")

test_that("eol_ping returns the correct value", {
  skip_on_cran()

  Sys.sleep(0.5)
  expect_true(eol_ping())
  expect_false(eol_ping(503))
  Sys.sleep(0.5)
  expect_true(eol_ping("content"))
})
