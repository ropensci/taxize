context("gbif_parse")

test_that("gbif_parse returns the correct values and dimensions classes", {
  skip_on_cran()
  vcr::use_cassette("gbif_parse", {
    tt <- gbif_parse(scientificname = 'x Agropogon littoralis L.')
  }, preserve_exact_body_bytes = TRUE)

  expect_match(as.character(tt[1,1]), 'x Agropogon littoralis')
	expect_match(as.character(tt[1,2]), 'SCIENTIFIC')

  expect_that(dim(tt), equals(c(1,12)))

	expect_is(tt, "data.frame")
	expect_is(tt$specificepithet, "character")
	expect_is(tt$authorship, "character")
})
