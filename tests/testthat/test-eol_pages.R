# tests for eol_pages fxn in taxize
context("eol_pages")

pageid <- suppressMessages(eol_search('Pomatomus'))$pageid[1]
pageid2 <- suppressMessages(eol_search('Helianthus'))$pageid[1]

test_that("eol_pages returns the correct value", {
	expect_equal(suppressMessages(eol_pages(taxonconceptID=pageid))$scinames[1,1], 60986524)
	expect_equal(suppressMessages(eol_pages(taxonconceptID=pageid2))$scinames[1,1], 32544232)
})

test_that("eol_pages returns the correct class", {
	expect_is(suppressMessages(eol_pages(taxonconceptID=pageid))$scinames, "data.frame")
})
