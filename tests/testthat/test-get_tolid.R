context("get_tol")

test_that("get_tol returns the correct value", {
  vcr::use_cassette("get_tol", {
    x <- get_tol("Quercus douglasii", messages = FALSE)
    z <- get_tol(c("Chironomus", "Chaetopteryx"),
      messages = FALSE)
  })

  expect_is(x, "txid")
  expect_is(x, "tol")
  expect_is(txidac(x), "character")
  expect_match(txz_uri(x), "opentreeoflife")
  expect_equal(length(x), 1)

  expect_is(z, "txid")
  expect_is(z, "tol")
  expect_is(txidac(z), "character")
  expect_match(txz_uri(z), "opentreeoflife")
  expect_equal(length(z), 2)
})

test_that("get_tol accepts ask-argument", {
  vcr::use_cassette("get_tol_ask_arg", {
    x <- sw(get_tol("Dugesia", ask = FALSE, messages = FALSE))
  })
  expect_true(is.na(x))
})

test_that("get_tol fails as expected", {
  skip_on_cran()

  expect_error(get_tol(), "argument \"sci\" is missing")
  expect_error(get_tol("Satyrium", ask = 4, messages = FALSE),
               "ask must be of class logical")

  # rows param
  expect_error(get_tol("Achlya", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_tol("Achlya", rows = 0, messages = FALSE),
               "rows > 0 is not TRUE")
})
