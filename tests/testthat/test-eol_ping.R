# tests for eol_ping fxn in taxize
context("eol_ping")

test_that("eol_ping returns the correct value", {
	expect_true(eol_ping())
	expect_false(eol_ping(503))
	expect_true(eol_ping("content"))
})
