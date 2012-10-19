# tests for gni_search fxn in taxize
context("gni_search")

test_that("gni_search returns the correct value", {
	expect_that(gni_search(search_term = "ama*", per_page = 1)[,2], 
							matches("22693003"))
})

test_that("gni_search returns the correct class", {
	expect_that(gni_search(search_term = "ama*", per_page = 1), 
							is_a("data.frame"))
})
