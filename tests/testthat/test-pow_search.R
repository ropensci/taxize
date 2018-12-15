context("pow_search")

test_that("pow_search returns the correct class", {
  skip_on_cran()

  one <- pow_search(q = "Quercus")

  expect_is(one, "list")
  expect_named(one, c('meta', 'data'))
  expect_is(one$meta, "list")
  expect_is(one$meta$cursor, "character")
  expect_is(one$data, "data.frame")
})

test_that("pow_search returns the correct class", {
  skip_on_cran()

  expect_error(
    pow_lookup(id = 'urn:lsid:ipni.org:names:320035-2', include = "foo"),
    "'include' must be one of"
  )
})
