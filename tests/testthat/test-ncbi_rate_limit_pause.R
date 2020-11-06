skip_on_cran()

took <- function(x) round(system.time(x)[[3]], 1)

test_that("ncbi_rate_limit_pause", {
  taxize_options(quiet=TRUE) # reset
  
  # key is missing
  expect_error(ncbi_rate_limit_pause(), "missing")

  # ncbi_sleep NOT set, key IS NULL: should be around 0.3
  expect_equal(took(ncbi_rate_limit_pause(NULL)), 0.3)

  # ncbi_sleep NOT set, key NOT NULL: should be around 0.1
  expect_equal(took(ncbi_rate_limit_pause("foobar")), 0.1)

  # ncbi_sleep IS set, key IS NULL: should be around 0.3
  taxize_options(ncbi_sleep = 0.6)
  expect_equal(took(ncbi_rate_limit_pause(NULL)), 0.6)

  # ncbi_sleep IS set, key NOT NULL: should be around 0.1
  taxize_options(ncbi_sleep = 0.7)
  expect_equal(took(ncbi_rate_limit_pause("foobar")), 0.7)
})
