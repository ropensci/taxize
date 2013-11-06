# tests for itis_phymat_format fxn in taxize
context("itis_phymat_format")

test_that("itis_phymat_format returns the correct value", {
	expect_that(itis_phymat_format("Poa annua", "rsubmit"), matches("poaceae%2Fpoa%2Fpoa_annua"))
})

test_that("itis_phymat_format returns the correct class", {
	expect_that(itis_phymat_format("Helianthus annuus", "rsubmit"), is_a("character"))
})
