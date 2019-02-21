context("itis_kingdomnames")

test_that("itis_kingdomnames - all", {
  vcr::use_cassette("itis_kingdomnames", {
    aa <- itis_kingdomnames()
  })

  expect_is(aa, "tbl_df")
  expect_named(aa, c('kingdomid', 'kingdomname', 'tsn'))
  expect_true(any(grepl("Animalia", aa$kingdomname)))
  expect_is(aa$kingdomname, "character")
})

test_that("itis_kingdomnames - with TSN's", {
  vcr::use_cassette("itis_kingdomnames_with_tsns", {
    one <- itis_kingdomnames(202385)
    two <- itis_kingdomnames(tsn = c(202385, 183833, 180543))
  })

  expect_match(one, "Animalia")
  expect_match(two[[1]], "Animalia")

  expect_that(one, is_a("character"))
  expect_that(two, is_a("character"))
})

test_that("itis_kingdomnames returns error when not found", {
  vcr::use_cassette("itis_kingdomnames_fail_well", {
    expect_error(length(itis_kingdomnames("stuff")[[1]]),
      "Bad Request \\(HTTP 400\\)")
  })
})
