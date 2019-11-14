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
