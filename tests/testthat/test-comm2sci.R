# tests for comm2sci fxn in taxize
context("comm2sci")

test_that("comm2sci returns the correct values and classes", {
  skip_on_cran()

  tt <- suppressMessages(comm2sci(commnames='black bear'))
  uu <- suppressMessages(comm2sci(commnames='annual blue grass', db='tropicos'))
  temp1 <- suppressMessages(comm2sci(commnames=c('black bear','roe deer'), db='eol'))
  temp2 <- suppressMessages(comm2sci(commnames='black bear', db='tropicos'))
  temp3 <- suppressMessages(comm2sci(commnames=c('black bear','roe deer'), db='eol'))

  expect_that(names(tt), equals('black bear'))
  expect_that(names(uu), equals('annual blue grass'))
  expect_identical(suppressMessages(comm2sci(commnames='bear', db='itis', itisby = "asfasdf"))[[1]], character(0))
  expect_named(temp2)
  expect_named(temp3)

  expect_that(tt, is_a("list"))
  expect_that(tt[[1]], is_a("character"))
  expect_that(uu, is_a("list"))
  expect_that(uu[[1]], is_a("character"))
  expect_is(suppressMessages(comm2sci(commnames='black bear', db='itis', simplify = FALSE))[[1]], "data.frame")
})
