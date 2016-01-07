# tests for tp_search fxn in taxize
context("tp_search")

ttt <- suppressMessages(tp_search(name = 'Poa annua'))
uuu <- suppressMessages(tp_search(name = 'stuff things'))

test_that("tp_search returns the correct class", {
	expect_that(ttt, is_a("data.frame"))
})

test_that("tp_search returns a data.frame with one raw starting none found", {
  expect_that(uuu, is_a("data.frame"))
  expect_that(names(uuu), equals("error"))
  expect_that(as.character(uuu[1,1]), equals("No names were found"))
})

test_that("tp_search behaves correctly on dot inputs", {
  expect_that(tp_search('Poa annua .annua'),
              gives_warning("detected, being URL encoded"))
  expect_that(tp_search('Poa annua annua'),
                 not(gives_warning()))
})

test_that("tp_search behaves correctly on subspecific inputs", {
  expect_that(tp_search('Poa annua var annua'),
              gives_warning("Tropicos doesn't like"))
  expect_that(tp_search('Poa annua var. annua'),
              gives_warning("Tropicos doesn't like"))
  expect_that(tp_search('Poa annua sp. annua'),
              gives_warning("Tropicos doesn't like"))
  expect_that(tp_search('Poa annua ssp. annua'),
              gives_warning("Tropicos doesn't like"))
  expect_that(tp_search('Poa annua subspecies annua'),
              gives_warning("Tropicos doesn't like"))

  expect_that(tp_search('Poa annua foo bar annua'),
              not(gives_warning()))
})
