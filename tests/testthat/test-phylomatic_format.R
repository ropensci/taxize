# tests for phylomatic_format fxn in taxize
context("phylomatic_format")

data("apg_families", package = "taxize")

test_that("phylomatic_format returns the correct value", {
  res <- suppressMessages(phylomatic_format(taxa = "Poa annua", "rsubmit"))
	expect_match(res, "poaceae%2Fpoa%2Fpoa_annua")
})

test_that("phylomatic_format returns the correct class", {
  res <- suppressMessages(phylomatic_format("Helianthus annuus", "rsubmit"))
	expect_is(res, "character")
})
