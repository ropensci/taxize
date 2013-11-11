# tests for gni_parse fxn in taxize
context("gni_parse")

tt <- gni_parse("Cyanistes caeruleus")

test_that("gni_parse returns the correct value", {
	expect_that(as.character(tt[,2]), matches("caeruleus"))
	expect_that(tt[,"position_genus"], equals(9))
})

test_that("gni_parse returns the correct class", {
	expect_that(tt, is_a("data.frame"))
})
