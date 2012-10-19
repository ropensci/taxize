# tests for gnr_resolve fxn in taxize
context("gnr_resolve")

test_that("gnr_resolve returns the correct value", {
	expect_that(ncol(gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"), returndf = TRUE)), 
							equals(5))
})

test_that("gnr_resolve returns the correct class", {
	expect_that(gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"), returndf = TRUE), is_a("data.frame"))
})
