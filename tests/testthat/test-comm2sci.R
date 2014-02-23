# tests for comm2sci fxn in taxize
context("comm2sci")

tt <- comm2sci(commnames='black bear')
uu <- comm2sci(commnames='annual blue grass', db='tropicos')
temp1 <- comm2sci(commnames=c('black bear','roe deer'), db='eol')
temp2 <- comm2sci(commnames='black bear', db='tropicos')
temp3 <- comm2sci(commnames=c('black bear','roe deer'), db='eol')
temp4 <- comm2sci(commnames='', db='eol')

test_that("comm2sci returns the correct value", {
  expect_that(names(tt), equals('black bear'))  
  expect_that(names(uu), equals('annual blue grass'))
  expect_identical(comm2sci(commnames='bear', db='itis', itisby = "asfasdf")[[1]], character(0))
  expect_named(temp2)
  expect_named(temp3)
  expect_named(temp4)
})

test_that("comm2sci returns the correct class", {
  expect_that(tt, is_a("list"))
  expect_that(tt[[1]], is_a("character"))
  expect_that(uu, is_a("list"))
  expect_that(uu[[1]], is_a("character"))
  expect_is(comm2sci(commnames='black bear', db='itis', simplify = FALSE)[[1]], "data.frame")
})

test_that("errors when expected", {
  expect_error(comm2sci(commnames='', db='eol'))
  expect_error(comm2sci(commnames='bear', db='itis', simplify = FALSE))
})