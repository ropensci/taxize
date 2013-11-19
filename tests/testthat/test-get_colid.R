# tests for get_colid fxn in taxize
context("get_colid")

test_that("get_colid returns the correct value", {
  expect_that(get_colid(sciname='Helianthus')[[1]], is_identical_to("14894326"))
  expect_that(is.na(get_colid(sciname='adsf asdf asdf', verbose=FALSE)[[1]]), 
              is_true())
})

test_that("get_colid returns the correct class", {
  expect_that(get_colid(c("Helianthus excubitor", "adsf asdf asdf"), verbose=FALSE), 
              is_a("colid"))
})

test_that("get_colid accepts ask-argument", {
  expect_that(is.na(get_colid(sciname='adsf asdf asdf', ask=FALSE, verbose=FALSE)[[1]]), 
              is_true())
})
