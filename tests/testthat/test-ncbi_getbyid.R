# tests for ncbi_getbyid fxn in taxize
context("ncbi_getbyid")

out <- ncbi_getbyid(ids="360040093", format="fasta", verbose=FALSE)

test_that("ncbi_getbyid returns the correct class", {
	expect_that(out, is_a("data.frame"))
	expect_that(as.character(unique(out$ids)), is_a("character"))
})