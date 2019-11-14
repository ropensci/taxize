context("get_colid")

test_that("get_colid: not found", {
  skip_on_cran()
  vcr::use_cassette("get_colid", {
    z <- get_colid(sciname='adsf asdf asdf', messages=FALSE)[[1]]
    w <- get_colid(c("Helianthus excubitor", "adsf asdf asdf"), 
      messages=FALSE)
  })

  expect_true(is.na(z))
  expect_is(w, "colid")
})

test_that("get_colid accepts ask-argument", {
  skip_on_cran()
  vcr::use_cassette("get_colid_ask_arg", {
    z <- get_colid(sciname='adsf asdf asdf', ask=FALSE, messages=FALSE)[[1]]
    expect_true(is.na(z))
  })
})

test_that("get_colid fails as expected", {
  skip_on_cran()

  expect_error(get_colid(), "argument \"sciname\" is missing")
  expect_error(get_colid('Poa annua', ask = 4, messages = FALSE),
               "ask must be of class logical")
  expect_error(
    get_colid("Satyrium", kingdom = 234, messages = FALSE),
    "kingdom must be of class character")
  expect_error(
    get_colid("Satyrium", phylum = 234, messages = FALSE),
    "phylum must be of class character")
  expect_error(
    get_colid("Satyrium", class = 234, messages = FALSE),
    "class must be of class character")
  expect_error(
    get_colid("Satyrium", order = 234, messages = FALSE),
    "order must be of class character")
  expect_error(
    get_colid("Satyrium", family = 234, messages = FALSE),
    "family must be of class character")
  expect_error(
    get_colid("Satyrium", rank = 234, messages = FALSE),
    "rank must be of class character")

  # rows param
  expect_error(get_colid("Satyrium", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_colid("Satyrium", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
