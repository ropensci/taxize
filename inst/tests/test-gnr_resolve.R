# tests for gnr_resolve fxn in taxize
context("gnr_resolve")

tmp <- gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"))

test_that("gnr_resolve returns the correct value", {
	expect_that(ncol(tmp), equals(4))
})

test_that("gnr_resolve returns the correct class", {
	expect_that(tmp, is_a("data.frame"))
})
