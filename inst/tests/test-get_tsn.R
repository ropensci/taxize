# tests for get_tsn fxn in taxize
context("get_tsn")

out <- get_tsn(searchterm="Quercus douglasii", searchtype="sciname")

test_that("get_tsn returns the correct value", {
	expect_that(out, matches("19322"))
})

test_that("get_tsn returns the correct class", {
	expect_that(out, is_a("tsn"))
})


