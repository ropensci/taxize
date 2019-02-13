# tests for get_eolid fxn in taxize
context("get_eolid")

# FIXME: uncomment these once EOL is sorted out
test_that("get_eolid returns the correct value", {
  skip_on_cran()

  expect_true(is.na(get_eolid(sciname='adsf asdf asdf', 
    verbose=FALSE)[[1]]))
  Sys.sleep(3)
})

test_that("get_eolid returns the correct class", {
  skip_on_cran()

  expect_that(get_eolid("adsf asdf asdf", verbose=FALSE),
              is_a("eolid"))
  Sys.sleep(1)
})

Sys.sleep(1)

test_that("get_eolid accepts ask-argument", {
  skip_on_cran()

  expect_that(is.na(get_eolid(sciname='adsf asdf asdf', ask=FALSE, verbose=FALSE)[[1]]),
              is_true())
})

test_that("get_eolid fails as expected", {
  skip_on_cran()

  expect_error(get_eolid(), "argument \"sciname\" is missing")
  expect_error(get_eolid("Poa annua", ask = 4, verbose = FALSE),
               "ask must be of class logical")

  Sys.sleep(1)
  # rows param
  expect_error(get_eolid("Poa annua", rows = "foobar", verbose = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_eolid("Poa annua", rows = 0, verbose = FALSE),
               "rows > 0 is not TRUE")
})
