context("get_pow")

test_that("get_pow: not found", {
  skip_on_cran()
  vcr::use_cassette("get_pow", {
    z <- get_pow("adsf asdf asdf", messages = FALSE)[[1]]
    w <- get_pow(c("Helianthus excubitor", "adsf asdf asdf"), 
      messages = FALSE)
  })

  expect_true(is.na(z))
  expect_is(w, "pow")
})

test_that("get_pow accepts ask-argument", {
  skip_on_cran()
  vcr::use_cassette("get_pow_ask_arg", {
    z <- get_pow("adsf asdf asdf", ask = FALSE,
      messages = FALSE)[[1]]
    expect_true(is.na(z))
  })
})

test_that("get_pow fails as expected", {
  skip_on_cran()

  expect_error(get_pow(), "argument \"sci_com\" is missing")
  expect_error(get_pow("Poa annua", ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_pow("Satyrium", family_filter = 234, messages = FALSE),
    "family_filter must be of class character")
  expect_error(
    get_pow("Satyrium", rank_filter = 234, messages = FALSE),
    "rank_filter must be of class character")

  # rows param
  expect_error(get_pow("Satyrium", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_pow("Satyrium", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
