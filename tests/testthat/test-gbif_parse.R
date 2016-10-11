context("gbif_parse")


test_that("gbif_parse returns the correct values and dimensions classes", {
  skip_on_cran()

  tt <- gbif_parse(scientificname = 'x Agropogon littoralis')

	expect_that(as.character(tt[1,1]), matches('x Agropogon littoralis'))
	expect_that(as.character(tt[1,2]), matches('SCIENTIFIC'))

  expect_that(dim(tt), equals(c(1,11)))

	expect_is(tt, "data.frame")
	expect_is(tt$specificepithet, "character")
	expect_is(tt$authorsparsed, "logical")
})
