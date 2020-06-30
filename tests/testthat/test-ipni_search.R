context("ipni_search")

test_that("ipni_search works", {
  skip_on_cran()
  vcr::use_cassette("ipni_search", {
      aa <- ipni_search(genus = 'Brintonia', isapnirecord = TRUE,
                        isgcirecord = TRUE, isikrecord = TRUE)
      cc <- ipni_search(genus = 'Pinus', species = 'contorta')
  })

  expect_is(aa, "data.frame")
  expect_is(cc, "data.frame")
  expect_is(aa$id, "character")
  expect_is(aa$family, "character")

  expect_named(aa, c('id','version','family','full_name_without_family_and_authors','authors'))
  expect_is(aa$family, "character")
})

test_that("ipni_search works with different output formats", {
  skip_on_cran()
  vcr::use_cassette("ipni_search_output_formats", {
    aa <- ipni_search(genus = 'Brintonia')
    bb <- ipni_search(genus = 'Brintonia', output = 'short')
    cc <- ipni_search(genus = 'Brintonia', output = 'classic')
    dd <- ipni_search(genus = 'Brintonia', output = 'extended')
  })

  expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")
  expect_is(cc, "data.frame")
  expect_is(dd, "data.frame")

  expect_lt(NCOL(aa), NCOL(bb))
  expect_lt(NCOL(bb), NCOL(cc))
  expect_lt(NCOL(aa), NCOL(cc))
  expect_lt(NCOL(aa), NCOL(dd))
  expect_lt(NCOL(cc), NCOL(dd))
})

test_that("ipni_search fails correctly", {
  expect_error(ipni_search(output = "foobar"), "'arg' should be one of")

  skip_on_cran()
  vcr::use_cassette("ipni_search_no_results", {
    expect_warning(ipni_search(family = 5), "No data")
    expect_warning(ipni_search(genus = "adfasdfasffd"), "No data found")
  })
})
