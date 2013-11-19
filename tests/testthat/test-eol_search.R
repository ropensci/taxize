# tests for eol_search fxn in taxize
context("eol_search")

test_that("eol_search returns the correct value", {
	expect_that(eol_search(terms='Ursus americanus luteolus')[[1]], equals(1273844))
})

test_that("eol_search returns the correct class", {
	expect_that(eol_search(terms='Salix')[[1]], is_a("numeric"))
	expect_that(eol_search('Homo'), is_a("data.frame"))
})