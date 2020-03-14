context("apg* functions")

test_that("apgOrders works", {
  skip_on_cran()
  vcr::use_cassette("apgOrders", {
    orders <- apgOrders()
  })

  expect_is(orders, "data.frame")
  expect_is(orders$order, "character")
  expect_is(orders$accepted, "logical")
  expect_equal(NCOL(orders), 4)
})

test_that("apgFamilies works", {
  skip_on_cran()
  vcr::use_cassette("apgFamilies", {
    families <- apgFamilies()
  })

  expect_is(families, "data.frame")
  expect_is(families$family, "character")
  expect_is(families$accepted, "logical")
  expect_equal(NCOL(families), 5)
})
