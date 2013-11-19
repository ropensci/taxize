# tests for tp_synonyms fxn in taxize
context("tp_synonyms")

dat <- tp_synonyms(id = 25509881, verbose=FALSE)

test_that("tp_synonyms returns the correct value", {
	expect_that(names(dat)[[1]], matches("accepted"))
})

test_that("tp_synonyms returns the correct class", {
	expect_that(dat, is_a("list"))
	expect_that(dat[[1]], is_a("data.frame"))
	expect_that(dat[[2]], is_a("data.frame"))
	expect_that(ncol(dat[[2]]), equals(4))
})
