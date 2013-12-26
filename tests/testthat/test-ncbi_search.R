# tests for ncbi_search fxn in taxize
context("ncbi_search")

out <- ncbi_search(taxa="Accipiter erythronemius", seqrange = "1:2000", getrelated=FALSE, verbose=FALSE)

test_that("ncbi_search returns the correct value", {
	expect_that(as.character(unique(out$genesavail)), is_a("character"))
	expect_that(as.character(out[grep("COI", out$genesavail, ignore.case=TRUE), ][1,1]), matches("Accipiter erythronemius"))
})

test_that("ncbi_search returns the correct class", {
	expect_that(out[grep("COI", out$genesavail, ignore.case=T), ], is_a("data.frame"))
	expect_that(as.character(unique(out$genesavail)), is_a("character"))
})