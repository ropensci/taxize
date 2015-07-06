# tests for eol_pages fxn in taxize
context("eol_pages")

pageid <- suppressMessages(eol_search('Pomatomus'))$pageid[1]
pageid2 <- suppressMessages(eol_search('Helianthus'))$pageid[1]

test_that("eol_pages returns the correct value", {
	expect_that(suppressMessages(eol_pages(taxonconceptID=pageid))$scinames[1,1], equals(52595367))
	expect_that(suppressMessages(eol_pages(taxonconceptID=pageid2))$scinames[1,1], equals(52926469 ))
})

test_that("eol_pages returns the correct class", {
	expect_that(suppressMessages(eol_pages(taxonconceptID=pageid))$scinames, is_a("data.frame"))
})
