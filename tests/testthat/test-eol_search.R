# tests for eol_search fxn in taxize
context("eol_search")

test_that("eol_search returns the correct value", {
  skip_on_cran()

	expect_that(eol_search(terms='Ursus americanus')$pageid[1], equals(328582))
})

test_that("eol_search returns the correct class", {
  skip_on_cran()

	expect_is(eol_search(terms='Salix')$pageid, "integer")
	expect_is(eol_search('Homo'), "data.frame")
})
