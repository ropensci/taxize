context("apg* functions")

test_that("get_apg_orders works", {
  skip_on_cran()
  vcr::use_cassette("get_apg_orders", {
    orders <- get_apg_orders()
  })

  expect_is(orders, "data.frame")
  expect_is(orders$order, "character")
  expect_is(orders$accepted, "logical")
  expect_equal(NCOL(orders), 4)
})

test_that("get_apg_families works", {
  skip_on_cran()
  vcr::use_cassette("get_apg_families", {
    families <- get_apg_families()
  })

  expect_is(families, "data.frame")
  expect_is(families$family, "character")
  expect_is(families$accepted, "logical")
  expect_equal(NCOL(families), 5)
})
