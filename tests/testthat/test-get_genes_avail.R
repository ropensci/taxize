# tests for get_genes_avail fxn in taxize
context("get_genes_avail")

out <- get_genes_avail(taxon_name="Accipiter erythronemius", seqrange = "1:2000", getrelated=FALSE, verbose=FALSE)

test_that("get_genes_avail returns the correct value", {
	expect_that(as.character(unique(out$genesavail)), is_a("character"))
	expect_that(as.character(out[grep("COI", out$genesavail, ignore.case=TRUE), ][1,1]), matches("Accipiter erythronemius"))
})

test_that("get_genes_avail returns the correct class", {
	expect_that(out[grep("COI", out$genesavail, ignore.case=T), ], is_a("data.frame"))
	expect_that(as.character(unique(out$genesavail)), is_a("character"))
})