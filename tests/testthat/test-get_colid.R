context("get_colid")

test_that("get_colid: not found", {
  vcr::use_cassette("get_colid", {
    z <- get_colid(sciname='adsf asdf asdf', verbose=FALSE)[[1]]
    w <- get_colid(c("Helianthus excubitor", "adsf asdf asdf"), 
      verbose=FALSE)
  })

  expect_true(is.na(z))
  expect_is(w, "colid")
})

test_that("get_colid accepts ask-argument", {
  vcr::use_cassette("get_colid_ask_arg", {
    z <- get_colid(sciname='adsf asdf asdf', ask=FALSE, verbose=FALSE)[[1]]
    expect_true(is.na(z))
  })
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
               "rows must be of class numeric, integer")
  expect_error(get_colid("Satyrium", rows = 0, verbose = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
