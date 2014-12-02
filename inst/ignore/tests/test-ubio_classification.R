# tests for ubio_classification fxn in taxize
context("ubio_classification")

out <- ubio_classification(hierarchiesID = 2483153)

test_that("ubio_classification returns the correct value", {
	expect_that(names(out)[1], matches("data"))
	expect_that(as.character(out$data[,1]), matches("84"))
})

test_that("ubio_classification returns the correct dimensions", {
  expect_that(ncol(out$data), equals(8))
  expect_that(length(out), equals(5))
})

test_that("ubio_classification returns the correct class", {
	expect_that(out$data, is_a("data.frame"))
	expect_that(out, is_a("list"))
})
