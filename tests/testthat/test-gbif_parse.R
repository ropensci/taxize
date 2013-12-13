# tests for gbif_parse fxn in taxize
context("gbif_parse")

tt <- gbif_parse(scientificname='x Agropogon littoralis')

test_that("gbif_parse returns the correct value", {
	expect_that(as.character(tt[1,1]), matches('x Agropogon littoralis'))
	expect_that(as.character(tt[1,2]), matches('SCINAME'))
})

test_that("gbif_parse returns the correct dimensions", {
  expect_that(dim(tt), equals(c(1,9)))
})

test_that("gbif_parse returns the correct class", {
	expect_that(tt, is_a("data.frame"))
	expect_that(tt$specificepithet, is_a("factor"))
	expect_that(tt$authorsparsed, is_a("logical"))
})
