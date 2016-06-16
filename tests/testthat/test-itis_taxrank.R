# tests for itis_taxrank fxn in taxize
context("itis_taxrank")


test_that("itis_taxrank returns the correct value", {
  skip_on_cran()

  temp <- itis_taxrank(query=202385, verbose=FALSE)

	expect_that(as.character(temp), matches("Subspecies"))

	expect_that(temp, is_a("character"))
})
