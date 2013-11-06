# tests for itis fxn in taxize
context("itis")

one <- getacceptednamesfromtsn('208527')
two <- gettaxonomicusagefromtsn(tsn = 526852)

test_that("itis returns the correct value", {
	expect_that(one, equals("208527"))
	expect_that(ncol(two), equals(2))
})

test_that("itis returns the correct class", {
	expect_that(one, is_a("character"))
	expect_that(two, is_a("data.frame"))
})
