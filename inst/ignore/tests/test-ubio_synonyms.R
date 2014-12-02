# tests for ubio_synonyms fxn in taxize
context("ubio_synonyms")

out <- ubio_synonyms(hierarchiesID = 4091702)

test_that("ubio_synonyms returns the correct value", {
	expect_that(dim(out), equals(c(1,9)))
})

test_that("ubio_synonyms returns the correct class", {
	expect_that(out, is_a("data.frame"))	
})