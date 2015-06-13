# tests for sci2comm fxn in taxize
context("sci2comm")

tt <- sci2comm(scinames = 'Helianthus annuus', db = 'ncbi', verbose = FALSE)
uu <- sci2comm(scinames = 'Helianthus annuus', db = 'itis', verbose = FALSE)

test_that("sci2comm returns the correct value", {
  expect_that(names(tt), equals('Helianthus annuus'))
  expect_that(names(uu), equals('Helianthus annuus'))
})

test_that("sci2comm returns the correct class", {
  expect_that(tt, is_a("list"))
  expect_that(tt[[1]], is_a("character"))

  expect_that(uu, is_a("list"))
  expect_that(uu[[1]], is_a("character"))
})
