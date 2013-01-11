# tests for iucn_summary fxn in taxize
context("tax_name")

tmp_itis <- tax_name(query="Baetis", get="family", db="itis")
tmp_ncbi  <- tax_name(query="Baetis", get="family", db="ncbi")

test_that("tax_name returns the correct value", {
  expect_equal(tmp_itis, "Baetidae")
})

test_that("tax_name returns the correct value", {
  expect_equal(tmp_ncbi, "Baetidae")
})