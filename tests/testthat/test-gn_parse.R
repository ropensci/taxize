test_that("gn_parse", {
  skip_on_cran()
  vcr::use_cassette("gn_parse", {
    tt <- gn_parse("Cyanistes caeruleus")
  })

  expect_is(tt, "data.frame")
  expect_is(tt, "tbl")
  expect_is(tt$normalized, "character")
  expect_is(tt$canonicalName, "data.frame")
  expect_is(tt$details, "list")
  expect_is(tt$positions, "list")
})

test_that("gn_parse fails well", {
  skip_on_cran()
  
  expect_error(gn_parse())
  expect_error(gn_parse(5))
})
