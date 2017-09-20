# tests for get_colid fxn in taxize
context("get_colid")

test_that("get_colid returns the correct value", {
  skip_on_cran()

  expect_that(is.na(get_colid(sciname='adsf asdf asdf', verbose=FALSE)[[1]]),
              is_true())
})

test_that("get_colid returns the correct class", {
  skip_on_cran()

  expect_that(get_colid(c("Helianthus excubitor", "adsf asdf asdf"), verbose=FALSE),
              is_a("colid"))
})

test_that("get_colid accepts ask-argument", {
  skip_on_cran()

  expect_that(is.na(get_colid(sciname='adsf asdf asdf', ask=FALSE, verbose=FALSE)[[1]]),
              is_true())
})


test_that("get_colid fails as expected", {
  skip_on_cran()

  expect_error(get_colid(), "argument \"sciname\" is missing")
  expect_error(get_colid('Poa annua', ask = 4, verbose = FALSE),
               "ask must be of class logical")
  expect_error(
    get_colid("Satyrium", kingdom = 234, verbose = FALSE),
    "kingdom must be of class character")
  expect_error(
    get_colid("Satyrium", phylum = 234, verbose = FALSE),
    "phylum must be of class character")
  expect_error(
    get_colid("Satyrium", class = 234, verbose = FALSE),
    "class must be of class character")
  expect_error(
    get_colid("Satyrium", order = 234, verbose = FALSE),
    "order must be of class character")
  expect_error(
    get_colid("Satyrium", family = 234, verbose = FALSE),
    "family must be of class character")
  expect_error(
    get_colid("Satyrium", rank = 234, verbose = FALSE),
    "rank must be of class character")

  # rows param
  expect_error(get_colid("Satyrium", rows = "foobar", verbose = FALSE),
               "'rows' must be numeric or NA")
  expect_error(get_colid("Satyrium", rows = 0, verbose = FALSE),
               "'rows' value must be an integer 1 or greater")
})
