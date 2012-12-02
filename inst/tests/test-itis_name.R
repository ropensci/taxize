# tests for itis_name fxn in taxize
context("itis_name")

out <- itis_name(query="Helianthus annuus", get="family")

test_that("itis_name returns the correct value", {
	expect_that(out, matches("Asteraceae"))
})

test_that("itis_name returns the correct class", {
	expect_that(out, is_a("character"))
})
