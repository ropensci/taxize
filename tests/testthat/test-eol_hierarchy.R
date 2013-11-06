# tests for eol_hierarchy fxn in taxize
context("eol_hierarchy")

pageid <- eol_search('Pomatomus')$id[1]
out <- eol_pages(taxonconceptID=pageid)

test_that("eol_hierarchy returns the correct class", {
	expect_that(eol_hierarchy(out[out$nameAccordingTo == "NCBI Taxonomy", "identifier"], returntype="data.frame"), 
							is_a("data.frame"))
	expect_that(eol_hierarchy(out[out$nameAccordingTo == "NCBI Taxonomy", "identifier"], returntype="list"), 
							is_a("list"))
})