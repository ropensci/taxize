context("test-key_helpers")

test_that("use_tropicos produces expected URL and message", {
  skip_on_cran()
  
  expect_equal(use_tropicos(), "http://services.tropicos.org/help?requestkey")
  expect_message(use_tropicos(), "After getting your key set")
})

test_that("use_eol is defunct", {
  expect_error(use_eol())
})

test_that("use_entrez produces expected URL and message", {
  skip_on_cran()
  
  expect_equal(use_entrez(), "https://www.ncbi.nlm.nih.gov/account/")
  expect_message(use_entrez(), "Create your key")
})
