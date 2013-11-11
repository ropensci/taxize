# tests for tp_namedistributions fxn in taxize
context("tp_namedistributions")

tt <- tp_namedistributions(id = 25509881)

test_that("tp_namedistributions returns the correct class", {
	expect_that(tt, is_a("list"))
	expect_that(tt[['location']], is_a("data.frame"))
	expect_that(tt[['reference']], is_a("data.frame"))
	expect_that(names(tt[['reference']]), equals(c("ReferenceId","ArticleTitle","Collation","AbbreviatedTitle","TitlePageYear","FullCitation","YearPublished")))
})
