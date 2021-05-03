context("gni_parse")

test_that("gni_parse returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("gni_parse", {
    tt <- gni_parse("Cyanistes caeruleus")
  })

  expect_match(as.character(tt[,2]), "caeruleus")
	expect_equal(tt[,"position_genus"], 9)
	expect_is(tt, "data.frame")
})

test_that("gni_parse fails well with names that can not be parsed", {
  skip_on_cran()
  vcr::use_cassette("gni_parse_not_parsed", {
    tt <- gni_parse("Nu aakhu")
  })

  expect_false(tt$parsed)
  expect_null(tt$canonical)
})
