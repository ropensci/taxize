# tests for getcommentdetailfromtsn fxn in taxize
context("getcommentdetailfromtsn")

test_that("getcommentdetailfromtsn returns the correct value", {
	expect_that(getcommentdetailfromtsn(180543)[1,3], matches("2007-08-20 15:06:38.0"))
	expect_that(as.character(getcommentdetailfromtsn(180541)$commentator), equals("Wilson & Reeder, eds. (2005)"))
})

test_that("getcommentdetailfromtsn returns the correct class", {
	expect_that(getcommentdetailfromtsn(180543), is_a("data.frame"))
	expect_that(getcommentdetailfromtsn(180541), is_a("data.frame"))
})
