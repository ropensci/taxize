# tests for tp_synonyms fxn in taxize
context("tp_synonyms")

dat <- tp_synonyms(id = 25509881)

test_that("tp_synonyms returns the correct value", {
	expect_that(names(dat)[[1]], matches("variable"))
})

test_that("tp_synonyms returns the correct class", {
	expect_that(dat, is_a("data.frame"))
	expect_that(ncol(dat), equals(3))
})
