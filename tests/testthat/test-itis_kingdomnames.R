# tests for itis_kingdomnames fxn in taxize
context("itis_kingdomnames")


test_that("itis_kingdomnames returns the correct value", {
  skip_on_cran()

  one <- itis_kingdomnames(202385, verbose=FALSE)
  two <- itis_kingdomnames(tsn=c(202385,183833,180543), verbose=FALSE)

  expect_that(one, matches("Animalia"))
  expect_that(two[[1]], matches("Animalia"))

  expect_that(one, is_a("character"))
  expect_that(two, is_a("character"))
})

test_that("itis_kingdomnames returns empty character string when given nonsense", {
  skip_on_cran()

  expect_that(nchar(itis_kingdomnames("stuff", verbose=FALSE))[[1]], equals(0))
})
