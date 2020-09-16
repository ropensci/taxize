context("get_tps")

test_that("get_tps returns the correct value", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_tps", {
    a <- get_tps(sci='Helianthus excubitor', messages=FALSE)
    b <- get_tps(sci='adsf asdf asdf', messages=FALSE)
    d <- get_tps(c("Helianthus excubitor", "adsf asdf asdf"),
      messages=FALSE)
  })
  
  expect_equal(txidac(a), "50230899")
  expect_true(is.na(b))
  expect_is(d, "tps")
})

test_that("get_tps accepts ask-argument", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_tps_ask_arg", {
    a <- get_tps(sci='adsf asdf asdf', ask=FALSE, messages=FALSE)
  })

  expect_true(is.na(a))
})

test_that("get_tps behaves correctly on dot inputs", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_tps_warnings_dots", {
    expect_that(get_tps('Pinus contorta var. yukonensis', messages=FALSE),
                gives_warning("detected, being URL encoded"))
    expect_warning(get_tps('Pinus contorta yukonensis'), NA)
  }, preserve_exact_body_bytes = TRUE)
})

test_that("get_tps behaves correctly on subspecific inputs", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_tps_warnings_subspecific", {
    expect_that(get_tps('Poa annua var annua', messages=FALSE),
                gives_warning("Tropicos doesn't like"))
    expect_that(get_tps('Poa annua var. annua', messages=FALSE),
                gives_warning("Tropicos doesn't like"))
    expect_that(get_tps('Poa annua sp. annua', messages=FALSE),
                gives_warning("Tropicos doesn't like"))
    expect_that(get_tps('Poa annua ssp. annua', messages=FALSE),
                gives_warning("Tropicos doesn't like"))
    expect_that(get_tps('Poa annua subspecies annua', messages=FALSE),
                gives_warning("Tropicos doesn't like"))

    expect_warning(get_tps('Poa annua foo bar annua', messages=FALSE),
      NA)
  })
})

test_that("get_tps fails as expected", {
  skip_on_cran()

  expect_error(get_tps(), "argument \"sci\" is missing")
  expect_error(get_tps('Poa annua', ask = 4, messages = FALSE),
               "ask must be of class logical")

  expect_error(
    get_tps("Poa annua", family = TRUE, messages = FALSE),
    "family must be of class character")
  expect_error(
    get_tps("Poa annua", rank = TRUE, messages = FALSE),
    "rank must be of class character")

  # rows param
  expect_error(get_tps("Poa annua", rows = "foobar", messages = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_tps("Poa annua", rows = 0, messages = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
