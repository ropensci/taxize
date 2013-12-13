# tests for gnr_datasources fxn in taxize
context("gnr_datasources")

tmp <- gnr_datasources()

test_that("gnr_datasources returns the correct class", {
	expect_that(tmp, is_a("data.frame"))
})

test_that("gnr_resolve returns the correct value", {
  expect_that(ncol(tmp), equals(2))
  expect_equal(tmp$title[12], 'EOL')
})