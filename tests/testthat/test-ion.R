# tests for ion fxn in taxize
context("ion")

test_that("ion returns the correct value", {
  skip_on_cran()

  aa <- ion(155166)
  bb <- ion(298678)
  cc <- ion(4796748) # ursus americanus
  dd <- ion(1280626) # puma concolor

  expect_is(aa, 'data.frame')
  expect_is(bb, 'data.frame')
  expect_is(cc, 'data.frame')
  expect_is(dd, 'data.frame')

  expect_named(aa, c('identifier', 'title', 'namecomplete'))
})

test_that("ion fails well", {
  skip_on_cran()

  expect_error(ion(), "argument \"x\" is missing")
  expect_error(ion(2343434434434), "Internal Server Error")
  expect_error(ion("asdfasfs"), "Internal Server Error")
})
