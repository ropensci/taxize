# tests for ubio_id fxn in taxize
context("ubio_id")

out <- ubio_id(namebankID = 2483153)

test_that("ubio_id returns the correct dimensions", {
	expect_that(length(out), equals(4))
	expect_that(dim(out$data), equals(c(1,8)))
	expect_that(dim(out$synonyms), equals(c(7,3)))
	expect_that(dim(out$vernaculars), equals(c(27,5)))
	expect_that(dim(out$citations), equals(c(7,4)))
})

test_that("ubio_id returns the correct class", {
	expect_that(out, is_a("list"))
	expect_that(out$synonyms, is_a("data.frame"))
	expect_that(out$citations, is_a("data.frame"))
})