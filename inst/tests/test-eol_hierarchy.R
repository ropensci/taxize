# tests for eol_hierarchy fxn in taxize
context("eol_hierarchy")

test_that("eol_hierarchy returns the correct value", {
	expect_that(eol_hierarchy('34345893', json = 'TRUE', returntype="data.frame")[1,1], 
							matches("Unknown identifier 34345893"))
})

test_that("eol_hierarchy returns the correct class", {
	expect_that(eol_hierarchy('34345893', json = 'TRUE', returntype="data.frame"), is_a("data.frame"))
})
