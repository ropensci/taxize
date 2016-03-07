# tests for get_tpsid fxn in taxize
context("get_tpsid")

test_that("get_tpsid returns the correct value", {
  expect_equal(get_tpsid(sciname='Helianthus excubitor', verbose=FALSE)[[1]], 50230899)
  expect_that(is.na(get_tpsid(sciname='adsf asdf asdf', verbose=FALSE)[[1]]),
              is_true())
})

test_that("get_tpsid returns the correct class", {
  expect_that(get_tpsid(c("Helianthus excubitor", "adsf asdf asdf"), verbose=FALSE),
              is_a("tpsid"))
})

test_that("get_tpsid accepts ask-argument", {
  expect_that(is.na(get_tpsid(sciname='adsf asdf asdf', ask=FALSE, verbose=FALSE)[[1]]),
              is_true())
})

test_that("get_tpsid behaves correctly on dot inputs", {
  expect_that(get_tpsid('Pinus contorta var. yukonensis'),
              gives_warning("detected, being URL encoded"))
  expect_warning(get_tpsid('Pinus contorta yukonensis'), NA)
})

test_that("get_tpsid behaves correctly on subspecific inputs", {
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
