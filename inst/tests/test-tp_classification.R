# tests for tp_classification fxn in taxize
context("tp_classification")

ttt <- tp_classification(id = 25509881)

test_that("tp_classification returns the correct class", {
	expect_that(ttt, is_a("data.frame"))
})
