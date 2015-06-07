# tests for names_list fxn in taxize
context("names_list")

test_that("names_list returns the correct dimensions", {
	expect_equal(length(names_list('species')), 10)
	expect_equal(length(names_list('species', size=20)), 20)
	expect_equal(length(names_list('order', size=2)), 2)
})

test_that("names_list returns the correct class", {
	expect_is(names_list('species'), "character")
})
