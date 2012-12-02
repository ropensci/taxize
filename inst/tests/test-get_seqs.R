# tests for get_seqs fxn in taxize
context("get_seqs")

test_that("get_seqs returns the correct value", {
	expect_that(get_seqs(taxon_name="Acipenser brevirostrum", gene = c("coi", "co1"),
											 seqrange = "1:3000", getrelated=T, writetodf=F)[1,2], matches("Acipenser brevirostrum"))
	expect_that(get_seqs(taxon_name="Acipenser brevirostrum", gene = c("coi", "co1"),
							seqrange = "1:3000", getrelated=T, writetodf=F)[1,1], matches("Acipenser brevirostrum"))
})

test_that("get_seqs returns the correct class", {
	expect_that(get_seqs(taxon_name="Acipenser brevirostrum", gene = c("coi", "co1"),
											 seqrange = "1:3000", getrelated=T, writetodf=F), is_a("data.frame"))
})
