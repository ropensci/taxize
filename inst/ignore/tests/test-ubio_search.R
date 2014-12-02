# tests for ubio_search fxn in taxize
context("ubio_search")

out <- ubio_search(searchName = 'elephant', sci = 1, vern = 0)

test_that("ubio_search returns the correct dimensions", {
	expect_that(dim(out$scientific), equals(c(1,8)))
})

test_that("ubio_search returns the correct class", {
	expect_that(out$scientific, is_a("data.frame"))
	expect_that(out$scientific$basionymunit, is_a("character"))
	expect_that(out$scientific$rankname, is_a("character"))
})
