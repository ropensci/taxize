test_that("gna_parse", {
  skip_on_cran()
  vcr::use_cassette("gna_parse", {
    tt <- gna_parse("Cyanistes caeruleus")
  })

  expect_is(tt, "data.frame")
  expect_is(tt, "tbl")
  expect_is(tt$normalized, "character")
  expect_is(tt$canonical_stemmed, "character")
})

test_that("gn_parse fails well", {
  skip_on_cran()
  
  expect_error(gna_parse())
  expect_error(gna_parse(5))
})
