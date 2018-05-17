# tests for tp_accnames fxn in taxize
context("tp_accnames")


test_that("tp_accnames returns the correct class", {
  skip_on_cran()

  out <- suppressMessages(tp_accnames(id = 25503923))

  if ("Error" %in% names(out)) skip("error in tp_accnames call - skipping")

	expect_that(out, is_a("list"))
	expect_that(out$synonyms, is_a("data.frame"))
	expect_that(out$acceptednames, is_a("data.frame"))
	expect_that(out$reference, is_a("data.frame"))

	expect_is(out$reference$referenceid, "integer")
	expect_that(out$reference$abbreviatedtitle, is_a("character"))
})
