# tests for tp_accnames fxn in taxize
context("tp_accnames")

out <- tp_accnames(id = 25503923)

test_that("tp_accnames returns the correct class", {
	expect_that(out, is_a("list"))
	expect_that(out$synonyms, is_a("data.frame"))
	expect_that(out$acceptednames, is_a("data.frame"))
	expect_that(out$reference, is_a("data.frame"))
  
	expect_that(out$reference$referenceid, is_a("numeric"))
	expect_that(out$reference$abbreviatedtitle, is_a("character"))
})
