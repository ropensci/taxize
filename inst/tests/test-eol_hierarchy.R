# tests for eol_hierarchy fxn in taxize
context("eol_hierarchy")

test_that("eol_hierarchy returns the correct value", {
	expect_that(eol_hierarchy(taxonConceptID='39153621', returntype="data.frame")[1,1], 
							matches("11660866"))
})

test_that("eol_hierarchy returns the correct class", {
	expect_that(eol_hierarchy(taxonConceptID='39153621', returntype="data.frame"), 
							is_a("data.frame"))
	expect_that(eol_hierarchy(taxonConceptID='39153621', returntype="list"), 
							is_a("list"))
})
