# tests for itis_taxrank fxn in taxize
context("itis_taxrank")


test_that("itis_taxrank returns the correct value", {
  skip_on_cran()

  temp <- itis_taxrank(query=202385, verbose=FALSE)

  expect_match(as.character(temp), "Subspecies")

	expect_is(temp, "character")
})
