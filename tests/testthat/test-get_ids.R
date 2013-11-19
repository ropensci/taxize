# tests for get_ids fxn in taxize
context("get_ids")

tt <- get_ids(names="Chironomus riparius", db = 'ncbi', verbose=FALSE)

test_that("get_ids returns the correct value", {
  expect_equal(tt[[1]][[1]], "315576")
})

test_that("get_ids returns the correct class", {
  expect_that(tt, is_a("ids"))
  expect_that(tt[[1]], is_a("uid"))
  expect_that(tt[[1]][[1]], is_a("character"))
})

test_that("get_ids accepts ask and verbose arguments", {
  expect_that(is.na(get_ids(names="Poa annua", db = 'eol', ask=FALSE)[[1]][[1]]), 
              is_true())
  expect_message(get_ids(names="Poa annua", db = 'ncbi'))
  expect_that(get_ids(names="Poa annua", db = 'ncbi', verbose=FALSE), not(shows_message()))
})