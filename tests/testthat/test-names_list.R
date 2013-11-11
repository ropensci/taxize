# tests for names_list fxn in taxize
context("names_list")

test_that("names_list returns the correct dimensions", {
	expect_that(length(names_list('species')), equals(10))
	expect_that(length(names_list('species', size=20)), equals(20))
	expect_that(length(names_list('order', size=2)), equals(2))
})

test_that("names_list returns the correct class", {
	expect_that(names_list('species'), is_a("character"))
})
