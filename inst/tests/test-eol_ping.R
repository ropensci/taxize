# tests for eol_ping fxn in taxize
context("eol_ping")

test_that("eol_ping returns the correct value", {
	expect_that(eol_ping(), matches("Success"))
})

test_that("eol_ping returns the correct class", {
	expect_that(eol_ping(), is_a("character"))
})
