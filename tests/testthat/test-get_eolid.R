# tests for get_eolid fxn in taxize
context("get_eolid")

test_that("get_eolid returns the correct value", {
  skip_on_cran()

  expect_that(is.na(get_eolid(sciname='adsf asdf asdf', verbose=FALSE)[[1]]),
              is_true())
})

test_that("get_eolid returns the correct class", {
  skip_on_cran()

  expect_that(get_eolid("adsf asdf asdf", verbose=FALSE),
              is_a("eolid"))
})

test_that("get_eolid accepts ask-argument", {
  skip_on_cran()

  expect_that(is.na(get_eolid(sciname='adsf asdf asdf', ask=FALSE, verbose=FALSE)[[1]]),
              is_true())
})
