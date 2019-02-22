context("get_tpsid")

test_that("get_tpsid returns the correct value", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_tpsid", {
    a <- get_tpsid(sciname='Helianthus excubitor', messages=FALSE)[[1]]
    b <- get_tpsid(sciname='adsf asdf asdf', messages=FALSE)[[1]]
    d <- get_tpsid(c("Helianthus excubitor", "adsf asdf asdf"), messages=FALSE)
  })
  
  expect_equal(a, "50230899")
  expect_true(is.na(b))
  expect_is(d, "tpsid")
})

test_that("get_tpsid accepts ask-argument", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_tpsid_ask_arg", {
    a <- get_tpsid(sciname='adsf asdf asdf', ask=FALSE, messages=FALSE)[[1]]
  })

  expect_true(is.na(a))
})

test_that("get_tpsid behaves correctly on dot inputs", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_tpsid_warnings_dots", {
    expect_that(get_tpsid('Pinus contorta var. yukonensis'),
                gives_warning("detected, being URL encoded"))
    expect_warning(get_tpsid('Pinus contorta yukonensis'), NA)
  })
})

test_that("get_tpsid behaves correctly on subspecific inputs", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("get_tpsid_warnings_subspecific", {
    expect_that(get_tpsid('Poa annua var annua'),
                gives_warning("Tropicos doesn't like"))
    expect_that(get_tpsid('Poa annua var. annua'),
                gives_warning("Tropicos doesn't like"))
    expect_that(get_tpsid('Poa annua sp. annua'),
                gives_warning("Tropicos doesn't like"))
    expect_that(get_tpsid('Poa annua ssp. annua'),
                gives_warning("Tropicos doesn't like"))
    expect_that(get_tpsid('Poa annua subspecies annua'),
                gives_warning("Tropicos doesn't like"))

    expect_warning(get_tpsid('Poa annua foo bar annua'), NA)
  })
})

test_that("get_tpsid fails as expected", {
  skip_on_cran()

  expect_error(get_tpsid(), "argument \"sciname\" is missing")
  expect_error(get_tpsid('Poa annua', ask = 4, verbose = FALSE),
               "ask must be of class logical")

  expect_error(
    get_tpsid("Poa annua", family = TRUE, verbose = FALSE),
    "family must be of class character")
  expect_error(
    get_tpsid("Poa annua", rank = TRUE, verbose = FALSE),
    "rank must be of class character")

  # rows param
  expect_error(get_tpsid("Poa annua", rows = "foobar", verbose = FALSE),
               "rows must be of class numeric, integer")
  expect_error(get_tpsid("Poa annua", rows = 0, verbose = FALSE),
               "all\\(rows > 0\\) is not TRUE")
})
