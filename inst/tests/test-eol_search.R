# tests for eol_search fxn in taxize
context("eol_search")

test_that("eol_search returns the correct value", {
	expect_that(eol_search('Homo')[[1]], matches("Encyclopedia of Life search: Homo"))
	expect_that(eol_search('Salix')$totalResults, equals("825"))
})

test_that("eol_search returns the correct class", {
	expect_that(eol_search('Homo'), is_a("list"))
})