# tests for get_uid fxn in taxize
context("get_uid")

test_that("get_uid returns the correct value", {
	expect_that(is.na(get_uid(c("Chironomus riparius", "aaa"))[2]), is_true())
})

test_that("get_uid returns the correct class", {
	expect_that(get_uid(c("Chironomus riparius", "Chaetopteryx")), is_a("uid"))
})


