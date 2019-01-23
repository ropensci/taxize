# tests for gni_parse fxn in taxize
context("gni_parse")

test_that("gni_parse returns the correct value", {
  skip_on_cran()

  tt <- gni_parse("Cyanistes caeruleus")

  expect_match(as.character(tt[,2]), "caeruleus")
	expect_that(tt[,"position_genus"], equals(9))

	expect_that(tt, is_a("data.frame"))
})
