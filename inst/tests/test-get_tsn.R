# tests for get_tsn fxn in taxize
context("get_tsn")

test_that("get_tsn returns the correct value", {
	expect_that(get_tsn("Accipiter erythronemius"), matches("558394"))
	expect_that(get_tsn(searchterm="polar bear", searchtype="comname"), matches("180542"))
})

test_that("get_tsn returns the correct class", {
	expect_that(get_tsn("Accipiter erythronemius"), is_a("tsn"))
})
