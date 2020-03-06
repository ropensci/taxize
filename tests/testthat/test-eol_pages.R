context("eol_pages")

test_that("eol_pages returns the correct value and classes", {
  skip_on_cran()

  vcr::use_cassette("eol_pages", {
    pageid <- suppressMessages(eol_search('Pomatomus'))$pageid[1]
    pageid2 <- suppressMessages(eol_search('Helianthus'))$pageid[1]
    aa <- suppressMessages(eol_pages(taxonconceptID = pageid))
  })

  expect_is(aa, "list")
  expect_is(aa$scinames, "data.frame")
  expect_null(aa$synonyms)
  expect_named(aa, c('scinames', 'synonyms', 'vernacular', 'refs', 'data_objects'))

  vcr::use_cassette("eol_pages2", {
  	bb <- suppressMessages(eol_pages(taxonconceptID = pageid2))
  })

	expect_is(bb, "list")
	expect_is(bb$scinames, "data.frame")
	expect_null(bb$synonyms)
  expect_named(bb, c('scinames', 'synonyms', 'vernacular', 'refs', 'data_objects'))
})
