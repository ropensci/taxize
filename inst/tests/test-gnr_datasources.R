# tests for gnr_datasources fxn in taxize
context("gnr_datasources")

test_that("gnr_datasources returns the correct value", {
	expect_that(gnr_datasources()[[1]][[1]], equals("2012-07-06T11:36:36Z"))
})

test_that("gnr_datasources returns the correct class", {
	expect_that(gnr_datasources(), is_a("list"))
})
