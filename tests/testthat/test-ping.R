# tests for eol_ping fxn in taxize
context("ping")

test_that("ncbi_ping returns the correct value", {
  skip_on_cran()

  expect_true(ncbi_ping())
  expect_false(ncbi_ping(503))
  expect_true(ncbi_ping("content"))
})

test_that("trpicos_ping returns the correct value", {
  skip_on_cran()

  expect_true(tropicos_ping())
  expect_false(tropicos_ping(503))
  expect_true(tropicos_ping("content"))
})

test_that("nbn_ping returns the correct value", {
  skip_on_cran()

  expect_true(nbn_ping())
  expect_false(nbn_ping(503))
  expect_true(nbn_ping("content"))
})

test_that("gbif_ping returns the correct value", {
  skip_on_cran()

  expect_true(gbif_ping())
  expect_false(gbif_ping(503))
  expect_true(gbif_ping("content"))
})

test_that("bold_ping returns the correct value", {
  skip_on_cran()

  expect_true(bold_ping())
  expect_false(bold_ping(503))
  expect_true(bold_ping("content"))
})

# test_that("ipni_ping returns the correct value", {
#   skip_on_cran()
#
#   expect_true(ipni_ping())
#   expect_false(ipni_ping(503))
#   expect_true(ipni_ping("content"))
# })

test_that("vascan_ping returns the correct value", {
  skip_on_cran()

  expect_true(vascan_ping())
  expect_false(vascan_ping(503))
  expect_true(vascan_ping("content"))
})
