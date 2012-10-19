# tests for get_tsn fxn in taxize
context("get_tsn")

test_that("get_tsn returns the correct value", {
	expect_that(get_tsn("Quercus douglasii", "sciname"), matches("19322"))
})

test_that("get_tsn returns the correct class", {
	expect_that(getcommentdetailfromtsn(180543), is_a("tsn"))
})
