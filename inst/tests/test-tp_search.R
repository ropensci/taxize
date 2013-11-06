# tests for tp_search fxn in taxize
context("tp_search")

ttt <- tp_search(id = 25509881)

test_that("tp_search returns the correct class", {
	expect_that(ttt, is_a("data.frame"))
})
