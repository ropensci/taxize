# tests for fresh_codes fxn in taxize
context("fresh_codes")

spec <- c("Acentrella sinaica",
					"Acentria ephemerella",
					"Acilius sp.",
					"Acroloxus lacustris",
					"Allotrichi pallicornis")
a <- fresh_validate(spec)
b <- fresh_codes(a)

test_that("fresh_codes returns the correct value", {
	expect_that(b[1,3], matches("acensina"))
})

test_that("fresh_codes returns the correct class", {
	expect_that(b, is_a("data.frame"))
})
