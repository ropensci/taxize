# tests for get_boldid fxn in taxize
context("get_boldid")

test_that("get_boldid returns the correct value", {
  expect_equal(get_boldid(searchterm = 'Helianthus')[[1]], '125295')
  expect_that(is.na(get_boldid(searchterm='adsf asdf asdf', verbose=FALSE)[[1]]), 
              is_true())
})

test_that("get_boldid returns the correct class", {
  expect_that(get_boldid(c("Helianthus excubitor", "adsf asdf asdf"), verbose=FALSE), 
              is_a("boldid"))
})

test_that("get_boldid accepts ask-argument", {
  expect_that(is.na(get_boldid('adsf asdf asdf', ask=FALSE, verbose=FALSE)[[1]]), 
              is_true())
})
