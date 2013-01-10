# tests for classification fxn in taxize
context("classification")

test_that("classification returns the correct value", {
	expect_that(classification(get_uid(c("Chironomus riparius", "aaa vva")))[[2]], equals(NA))
})

test_that("classification returns the correct class", {
	expect_that(classification(315576, ID = "uid"), is_a("list"))
	expect_that(classification(315576, ID = "uid")[[1]], is_a("data.frame"))
})
