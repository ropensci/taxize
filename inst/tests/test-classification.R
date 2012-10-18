# tests for classification fxn in taxize
context("classification")

test_that("classification returns the correct value", {
	expect_that(classification(get_uid(c("Chironomus riparius", "aaa vva")))[[2]], equals(NA))
	expect_that(classification(get_tsn(c("Chironomus riparius", "aaa vva"), "sciname"))[[1]][1,2], matches("Animalia"))
})

test_that("classification fails and throws error", {
	expect_that(classification(315576), throws_error("Must specify Identifier!"))
})

test_that("classification returns the correct class", {
	expect_that(classification(315576, ID = "uid"), is_a("list"))
	expect_that(classification(315576, ID = "uid")[[1]], is_a("data.frame"))
})
