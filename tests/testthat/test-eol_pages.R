# tests for eol_pages fxn in taxize
context("eol_pages")

pageid <- suppressMessages(eol_search('Pomatomus'))$pageid[1]
pageid2 <- suppressMessages(eol_search('Helianthus'))$pageid[1]

test_that("eol_pages returns the correct value", {
  aa <- suppressMessages(eol_pages(taxonconceptID = pageid))

  expect_is(aa, "list")
  expect_is(aa$scinames, "data.frame")
  expect_is(aa$syns, "character")

	bb <- suppressMessages(eol_pages(taxonconceptID = pageid2))

	expect_is(bb, "list")
	expect_is(bb$scinames, "data.frame")
	expect_is(bb$syns, "character")
})

test_that("eol_pages returns the correct class", {
	expect_is(suppressMessages(eol_pages(taxonconceptID=pageid))$scinames, "data.frame")
})
