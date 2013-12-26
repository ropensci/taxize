# tests for ncbi_getbyname fxn in taxize
context("ncbi_getbyname")

test_that("ncbi_getbyname returns the correct class", {
	expect_that(ncbi_getbyname(taxa="Acipenser brevirostrum", gene = c("coi", "co1"),
											 seqrange = "1:3000"), is_a("data.frame"))
})
