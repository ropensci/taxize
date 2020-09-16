# tests for get_bold fxn in taxize
context("get_bold")

test_that("get_bold returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("get_bold", {
    expect_equal(txidac(get_bold(sci = 'Helianthus', messages = FALSE)), '125295')
    expect_true(is.na(txidac(get_bold(sci='adsf asdf asdf', messages=FALSE))))
    expect_is(get_bold(c("Helianthus excubitor", "adsf asdf asdf"), messages=FALSE), 
      "bold")
  })
})

test_that("get_bold accepts ask-argument", {
  skip_on_cran()
  vcr::use_cassette("get_bold_ask_false", {
    expect_true(is.na(
      txidac(get_bold('adsf asdf asdf', ask=FALSE, messages=FALSE))))
  })
})

test_that("get_bold works when there's no parent name", {
  skip_on_cran()
  vcr::use_cassette("get_bold_no_parent_name", {
    x <- get_bold("Chordata", messages = FALSE)
  })
  expect_is(x, "txid")
  expect_is(x, "bold")
  expect_equal(txidac(x), "18")
})

test_that("get_bold fails as expected", {
  skip_on_cran()

  expect_error(get_bold(), "argument \"sci\" is missing")
  expect_error(get_bold("Satyrium", ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_bold("Osmi", fuzzy=4, messages = FALSE),
    "fuzzy must be of class logical")
  expect_error(
    get_bold("Osmi", dataTypes = 4, messages = FALSE),
    "dataTypes must be of class character")
  expect_error(
    get_bold("Osmi", includeTree = 4, messages = FALSE),
    "includeTree must be of class logical")
  expect_error(
    get_bold("Osmi", rank = 4, messages = FALSE),
    "rank must be of class character")
  expect_error(
    get_bold("Osmi", division = 4, messages = FALSE),
    "division must be of class character")
  expect_error(
    get_bold("Osmi", parent = 4, messages = FALSE),
    "parent must be of class character")

  # rows param
  expect_error(get_bold("Achlya", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_bold("Achlya", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")

  # skip('BOLD API down right now, SSL issue')
  # filter param doesn't match any values - returns NA and warns
  expect_warning(
    (z=get_bold("Satyrium", division = "Plants", messages = FALSE)),
    "check spelling")
  expect_equal(txidac(z), NA_character_)
})
