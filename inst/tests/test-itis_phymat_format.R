# tests for itis_phymat_format fxn in taxize
context("itis_phymat_format")

test_that("itis_phymat_format returns the correct value", {
	expect_that(itis_phymat_format(180541, "rsubmit"), 
							matches("ursidae%2Fursus%2Fursus_americanus"))
})

test_that("itis_phymat_format returns the correct class", {
	expect_that(itis_phymat_format(180541, "rsubmit"), is_a("character"))
})
