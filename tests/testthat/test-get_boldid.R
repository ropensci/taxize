# tests for get_boldid fxn in taxize
context("get_boldid")

test_that("get_boldid returns the correct value", {
  skip_on_cran()

  expect_equal(get_boldid(searchterm = 'Helianthus', verbose = FALSE)[[1]], '125295')
  expect_that(is.na(get_boldid(searchterm='adsf asdf asdf', verbose=FALSE)[[1]]),
              is_true())
})

test_that("get_boldid returns the correct class", {
  skip_on_cran()

  expect_that(get_boldid(c("Helianthus excubitor", "adsf asdf asdf"), verbose=FALSE),
              is_a("boldid"))
})

test_that("get_boldid accepts ask-argument", {
  skip_on_cran()

  expect_that(is.na(get_boldid('adsf asdf asdf', ask=FALSE, verbose=FALSE)[[1]]),
              is_true())
})

test_that("get_boldid fails as expected", {
  skip_on_cran()

  expect_error(get_boldid(), "argument \"searchterm\" is missing")
  expect_error(get_boldid("Satyrium", ask = 4, verbose = FALSE),
               "ask must be of class logical")
  expect_error(
    get_boldid("Osmi", fuzzy=4, verbose = FALSE),
    "fuzzy must be of class logical")
  expect_error(
    get_boldid("Osmi", dataTypes = 4, verbose = FALSE),
    "dataTypes must be of class character")
  expect_error(
    get_boldid("Osmi", includeTree = 4, verbose = FALSE),
    "includeTree must be of class logical")
  expect_error(
    get_boldid("Osmi", rank = 4, verbose = FALSE),
    "rank must be of class character")
  expect_error(
    get_boldid("Osmi", division = 4, verbose = FALSE),
    "division must be of class character")
  expect_error(
    get_boldid("Osmi", parent = 4, verbose = FALSE),
    "parent must be of class character")

  # rows param
  expect_error(get_boldid("Achlya", rows = "foobar", verbose = FALSE),
               "'rows' must be numeric or NA")
  expect_error(get_boldid("Achlya", rows = 0, verbose = FALSE),
               "'rows' value must be an integer 1 or greater")
})
