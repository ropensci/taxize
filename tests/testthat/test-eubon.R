# tests for eubon fxn in taxize
context("eubon")

test_that("eubon works", {
  skip_on_cran()

  aa <- eubon("Prionus")
  bb <- eubon("Salmo", providers = 'worms')
  cc <- eubon("Salmo", providers = c('pesi', 'worms'))

  expect_is(aa, "list")
  expect_is(bb, "list")
  expect_is(cc, "list")

  expect_is(aa$query, "list")
  expect_equal(length(aa$query), 1)
  expect_equal(length(aa$query[[1]]$response), 1)
  expect_is(aa$query[[1]]$request, "list")
  expect_equal(aa$query[[1]]$request$queryString, "Prionus")

  expect_is(bb$query, "list")
  expect_equal(length(bb$query), 1)
  expect_equal(length(bb$query[[1]]$response), 1)
  expect_is(bb$query[[1]]$request, "list")
  expect_equal(bb$query[[1]]$request$queryString, "Salmo")

  expect_is(cc$query, "list")
  expect_equal(length(cc$query), 1)
  expect_equal(length(cc$query[[1]]$response), 2)
  expect_is(cc$query[[1]]$request, "list")
  expect_equal(cc$query[[1]]$request$queryString, "Salmo")
})

test_that("eubon fails well", {
  skip_on_cran()

  expect_error(eubon("Salmo", 'asdfdf'),
               "Error 400 invalid value for request parameter 'providers'")

  expect_error(eubon("Salmo", searchMode = "adfdf"),
               "Error 400 Bad Request")
})
