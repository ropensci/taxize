# tests for get_genes fxn in taxize
context("get_genes")

out <- get_genes(ids="360040093", format="fasta", verbose=FALSE)

test_that("get_genes returns the correct class", {
	expect_that(out, is_a("data.frame"))
	expect_that(as.character(unique(out$ids)), is_a("character"))
})