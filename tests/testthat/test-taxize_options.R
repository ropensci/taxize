skip_on_cran()

test_that("taxize_options", {
  taxize_options(quiet=TRUE) # reset
  expect_is(taxize_env, "environment")
  expect_is(taxize_env$options, "list")
  expect_length(taxize_env$options, 0)
  # fxn returns null itself
  expect_null(taxize_options(quiet=TRUE))
})

test_that("taxize_options fails well", {
  # unused arg
  expect_error(taxize_options(a=5, quiet=TRUE), "unused argument")

  # wrong types
  expect_error(taxize_options(taxon_state_messages = 5, quiet=TRUE), "logical")
  expect_error(taxize_options(ncbi_sleep = 'foo', quiet=TRUE), "numeric")

  # sleep min value
  expect_error(taxize_options(ncbi_sleep = 0.01, quiet=TRUE),
    "must be >")
})
