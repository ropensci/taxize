context("test-key_helpers")

test_that("use_iucn produces expected URL and message", {
  expect_equal(use_iucn(), "http://apiv3.iucnredlist.org/api/v3/token")
  expect_message(use_iucn(), "After getting your key set")
})


test_that("use_tropicos produces expected URL and message", {
  expect_equal(use_tropicos(), "http://services.tropicos.org/help?requestkey")
  expect_message(use_tropicos(), "After getting your key set")
})


test_that("use_eol produces expected URL and message", {
  expect_equal(use_eol(), "http://eol.org/info/api_overview")
  expect_message(use_eol(), "Generate your key")
})

test_that("use_entrez produces expected URL and message", {
  expect_equal(use_eol(), "https://www.ncbi.nlm.nih.gov/account/")
  expect_message(use_eol(), "Create your key")
})