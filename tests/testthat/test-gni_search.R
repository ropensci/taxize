# tests for gni_search fxn in taxize
context("gni_search")

test_that("gni_search returns the correct value", {
	expect_equal(gni_search(search_term = "ama*", per_page = 1)[,2], "22693003")
})

test_that("gni_search returns the correct class", {
	expect_is(gni_search(search_term = "ama*", per_page = 1), "data.frame")
})
