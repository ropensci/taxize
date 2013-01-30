# tests for gnr_datasources fxn in taxize
context("gnr_datasources")

test_that("gnr_datasources returns the correct class", {
	expect_that(gnr_datasources(), is_a("list"))
})
