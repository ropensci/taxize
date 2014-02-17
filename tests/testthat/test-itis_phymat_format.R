# tests for phylomatic_format fxn in taxize
context("phylomatic_format")

test_that("phylomatic_format returns the correct value", {
	expect_that(phylomatic_format("Poa annua", "rsubmit"), matches("poaceae%2Fpoa%2Fpoa_annua"))
})

test_that("phylomatic_format returns the correct class", {
	expect_that(phylomatic_format("Helianthus annuus", "rsubmit"), is_a("character"))
})
