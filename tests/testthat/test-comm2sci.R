# tests for comm2sci fxn in taxize
context("comm2sci")

tt <- comm2sci(commnames='black bear')
uu <- comm2sci(commnames='annual blue grass', db='tropicos')

test_that("comm2sci returns the correct value", {
  expect_that(names(tt), equals('black bear'))
  
  expect_that(names(uu), equals('annual blue grass'))
})

test_that("comm2sci returns the correct class", {
  expect_that(tt, is_a("list"))
  expect_that(tt[[1]], is_a("character"))
  
  expect_that(uu, is_a("list"))
  expect_that(uu[[1]], is_a("character"))
})