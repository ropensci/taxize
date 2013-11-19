# tests for sci2comm fxn in taxize
context("sci2comm")

tt <- sci2comm(scinames='Helianthus annuus')
uu <- sci2comm(scinames='Helianthus annuus', db='itis')

test_that("sci2comm returns the correct value", {
  expect_that(names(tt), equals('Helianthus annuus'))
  expect_that(as.character(tt[[1]][1,2]), equals('cy'))
  
  expect_that(names(uu), equals('Helianthus annuus'))
  expect_that(as.character(uu[[1]][1,2]), equals('English'))
})

test_that("sci2comm returns the correct class", {
  expect_that(tt, is_a("list"))
  expect_that(tt[[1]], is_a("data.frame"))
  
  expect_that(uu, is_a("list"))
  expect_that(uu[[1]], is_a("data.frame"))
})