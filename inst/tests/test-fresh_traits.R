# tests for fresh_traits fxn in taxize
context("fresh_traits")

spec <- c("Acentrella sinaica",
					"Acentria ephemerella",
					"Acilius sp.",
					"Acroloxus lacustris",
					"Allotrichi pallicornis")

a <- fresh_validate(spec)
a_traits <- fresh_traits(a)

test_that("fresh_traits returns the correct value", {
	expect_that(a_traits[1,1], matches("Acentrella"))
})

test_that("fresh_traits returns the correct class", {
	expect_that(a_traits, is_a("data.frame"))
})
