# tests for itis_kingdomnames fxn in taxize
context("itis_kingdomnames")

test_that("itis_kingdomnames - all", {
  skip_on_cran()

  aa <- itis_kingdomnames()

  expect_is(aa, "tbl_df")
  expect_named(aa, c('kingdomid', 'kingdomname', 'tsn'))
  expect_true(any(grepl("Animalia", aa$kingdomname)))
  expect_is(aa$kingdomname, "character")
})

test_that("itis_kingdomnames - with TSN's", {
  skip_on_cran()

  one <- itis_kingdomnames(202385)
  two <- itis_kingdomnames(tsn=c(202385,183833,180543))

  expect_that(one, matches("Animalia"))
  expect_that(two[[1]], matches("Animalia"))

  expect_that(one, is_a("character"))
  expect_that(two, is_a("character"))
})

test_that("itis_kingdomnames returns empty character string when given nonsense", {
  skip_on_cran()

  expect_equal(length(itis_kingdomnames("stuff")[[1]]), 0)
})
