# tests for get_uid fxn in taxize
context("get_uid")

test_that("get_uid returns the correct value", {
	expect_that(get_uid(c("Chironomus riparius", "Chaetopteryx")), matches(c("315576","492549")))
})

test_that("get_uid returns the correct class", {
	expect_that(get_uid(c("Chironomus riparius", "Chaetopteryx")), is_a("uid"))
})
