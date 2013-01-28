# tests for itis fxn in taxize
context("itis")

one <- itis(36616, "getfullhierarchyfromtsn")
two <- itis(203539, "getparenttsnfromtsn") 

test_that("itis returns the correct value", {
	expect_that(ncol(one), equals(5))
	expect_that(ncol(one), equals(2))
	expect_that(as.character(two[,2]), equals("203539"))
})

test_that("itis returns the correct class", {
	expect_that(one, is_a("data.frame"))
	expect_that(two, is_a("data.frame"))
})
