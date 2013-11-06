# tests for itis_acceptname fxn in taxize
context("itis_acceptname")

temp <- itis_acceptname(208527)

test_that("itis_acceptname returns the correct value", {
	expect_that(temp, matches("208527"))
})

test_that("itis_acceptname returns the correct class", {
	expect_that(temp, is_a("character"))
})
