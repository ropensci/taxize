# tests for eol_hierarchy fxn in taxize
context("eol_hierarchy")

pageid <- eol_search('Pomatomus')$pageid[1]
out <- eol_pages(taxonconceptID=pageid)$scinames

test_that("eol_hierarchy returns the correct class", {
	expect_that(eol_hierarchy(out[out$nameAccordingTo == "NCBI Taxonomy", "identifier"]), 
							is_a("data.frame"))
})