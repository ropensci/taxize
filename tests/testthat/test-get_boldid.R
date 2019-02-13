# tests for get_boldid fxn in taxize
context("get_boldid")

vcr::use_cassette("get_boldid", {
  test_that("get_boldid returns the correct value", {
    expect_equal(get_boldid(searchterm = 'Helianthus', verbose = FALSE)[[1]], '125295')
    expect_true(is.na(get_boldid(searchterm='adsf asdf asdf', verbose=FALSE)[[1]]))
    expect_is(get_boldid(c("Helianthus excubitor", "adsf asdf asdf"), verbose=FALSE), 
      "boldid")
  })
})

test_that("get_boldid accepts ask-argument", {
  vcr::use_cassette("get_boldid_ask_false", {
    expect_true(is.na(
      get_boldid('adsf asdf asdf', ask=FALSE, verbose=FALSE)[[1]]))
  })
})

test_that("get_boldid works when there's no parent name", {
  vcr::use_cassette("get_boldid_no_parent_name", {
    x <- get_boldid("Chordata", verbose = FALSE)
  })

  expect_is(x, "boldid")
  expect_equal(x[1], "18")
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
               "rows must be of class numeric, integer")
  expect_error(get_boldid("Achlya", rows = 0, verbose = FALSE),
               "rows > 0 is not TRUE")
})
