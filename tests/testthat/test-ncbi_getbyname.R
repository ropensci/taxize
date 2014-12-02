# tests for ncbi_getbyname fxn in taxize
context("ncbi_getbyname")

zz <- suppressWarnings(ncbi_getbyname(taxa="Acipenser brevirostrum", gene = c("coi", "co1"), seqrange = "1:3000", verbose = FALSE))

test_that("ncbi_getbyname returns the correct stuff", {
	expect_is(zz, "data.frame")
	expect_is(zz$sequence, "character")
	expect_equal(NCOL(zz), 7)
})
