# tests for eol_pages fxn in taxize
context("eol_pages")

pageid <- eol_search('Pomatomus')$id[1]
pageid2 <- eol_search('Helianthus')$id[1]

test_that("eol_pages returns the correct value", {
	expect_that(eol_pages(taxonconceptID=pageid)[1,1], equals(52595367))
	expect_that(eol_pages(taxonconceptID=pageid2)[1,1], equals(52926469 ))
})

test_that("eol_pages returns the correct class", {
	expect_that(eol_pages(taxonconceptID=pageid), is_a("data.frame"))
})
