# tests for col_classification fxn in taxize
context("col_classification")

temp <- col_classification(name = "Apis")
	
test_that("col_classification returns the correct value", {
	expect_that(temp[[1]][1,2], matches("Kingdom"))
})

test_that("col_classification returns the correct class", {
	expect_that(temp, is_a("list"))
	expect_that(temp[[1]], is_a("data.frame"))
})
