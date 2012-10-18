# tests for get_seqs fxn in taxize
context("get_seqs")

test_that("get_seqs returns the correct value", {
	expect_that(get_seqs(taxon_name="Bombus impatiens", gene = c("coi", "co1"), 
											 seqrange = "900:1300", getrelated=F, writetodf=F)[1,2], equals(NA))
	expect_that(get_seqs(taxon_name="Bombus impatiens", gene = c("coi", "co1"), 
											 seqrange = "900:1300", getrelated=T, writetodf=F)[1,3], equals(27902900))
})

test_that("get_seqs returns the correct class", {
	expect_that(get_seqs(taxon_name="Bombus impatiens", gene = c("coi", "co1"), 
											 seqrange = "900:1300", getrelated=T, writetodf=F), is_a("data.frame"))
})
