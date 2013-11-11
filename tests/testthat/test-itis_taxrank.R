# tests for itis_taxrank fxn in taxize
context("itis_taxrank")

temp <- itis_taxrank(query=202385, verbose=FALSE)

test_that("itis_taxrank returns the correct value", {
	expect_that(as.character(temp), matches("Subspecies"))
})

test_that("itis_taxrank returns the correct class", {
	expect_that(temp, is_a("factor"))
})
