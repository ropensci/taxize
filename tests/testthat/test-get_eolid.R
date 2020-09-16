context("get_eol")

test_that("get_eol works", {
  skip_on_cran()

  vcr::use_cassette("get_eol", {
    a <- get_eol(sci_com = "Puma concolor", messages = FALSE, rows = 1)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(a, "txid")
  expect_is(a, "eol")
  expect_is(txidac(a), "character")
  # expect_is(attr(a, "pageid"), "character") # for now, not included
  # expect_is(attr(a, "provider"), "character") # for now, not included
  expect_is(txz_match(a), "character")
  expect_true(txz_mm(a))
  expect_true(txz_pm(a))
  expect_is(txz_uri(a), "character")
})

test_that("get_eol accepts ask-argument", {
  skip_on_cran()

  vcr::use_cassette("get_eol_ask_param", {
    expect_true(is.na(get_eol(sci_com = 'adsf asdf asdf', ask = FALSE,
      messages = FALSE)))
  })
})

test_that("get_eol fails as expected", {
  skip_on_cran()

  expect_error(get_eol(), "argument \"sci_com\" is missing")
  expect_error(get_eol("Poa annua", ask = 4, messages = FALSE),
               "ask must be of class logical")

  Sys.sleep(1)
  # rows param
  expect_error(get_eol("Poa annua", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_eol("Poa annua", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
