# tests for tp_classification fxn in taxize
context("tp_classification")

ttt <- tp_classification(id = 25509881)

test_that("tp_classification returns the correct class", {
	expect_that(ttt[[1]], is_a("data.frame"))
	expect_that(ttt[[1]]$ScientificName, is_a("factor"))
})

test_that("tp_classification returns NA when given an ID that doesn't exist", {
  expect_true(is.na(tp_classification(id = 34)[[1]][1,1]))
})