# tests for eol_search fxn in taxize
context("eol_search")

test_that("eol_search returns the correct value", {
  skip_on_cran()

  vcr::use_cassette("eol_search", {
    aa <- eol_search(terms='Ursus americanus')
  })

  expect_is(aa, "data.frame")
	expect_type(aa$pageid, "integer")
  expect_type(aa$name, "character")
  expect_type(aa$link, "character")
})
