# tests for tp_namedistributions fxn in taxize
context("tp_namedistributions")

test_that("tp_namedistributions returns the correct class", {
	expect_that(tp_namedistributions(id = 25509881), is_a("data.frame"))
})
