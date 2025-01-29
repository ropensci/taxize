context("ping")

test_that("ncbi_ping returns the correct value", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("ncbi_ping", {
    expect_true(ncbi_ping())
    Sys.sleep(1)
    expect_false(ncbi_ping(503))
    Sys.sleep(1)
    expect_true(ncbi_ping("content"))
  })
})

test_that("trpicos_ping returns the correct value", {
  skip_on_cran() # uses secrets
  skip_on_ci()
  vcr::use_cassette("trpicos_ping", {
    expect_true(tropicos_ping())
    expect_true(tropicos_ping("content"))
  }, preserve_exact_body_bytes = TRUE)
})

test_that("nbn_ping returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("nbn_ping", {
    expect_true(nbn_ping())
    expect_false(nbn_ping(503))
    expect_true(nbn_ping("content"))
  })
})

test_that("gbif_ping returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("gbif_ping", {
    expect_true(gbif_ping())
    expect_false(gbif_ping(503))
    expect_true(gbif_ping("content"))
  })
})

test_that("bold_ping returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("bold_ping", {
    expect_true(bold_ping())
    expect_false(bold_ping(503))
    expect_true(bold_ping("content"))
  }, match_requests_on = c("method", "query"))
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
  vcr::use_cassette("vascan_ping", {
    expect_true(vascan_ping())
    expect_false(vascan_ping(503))
    expect_true(vascan_ping("content"))
  }, preserve_exact_body_bytes = TRUE, match_requests_on = c("method", "query"))
})
