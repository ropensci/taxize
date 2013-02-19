# tests for iucn_summary fxn in taxize
context("tax_name")

tmp_itis <- tax_name(query="Baetis", get="family", db="itis")
tmp_ncbi  <- tax_name(query="Baetis", get="family", db="ncbi")

test_that("tax_name returns the correct class", {
	expect_that(tmp_itis, is_a("character"))
	expect_that(tmp_ncbi, is_a("character"))
})

test_that("tax_name returns the correct value", {
	expect_equal(tmp_ncbi, "Baetidae")
})