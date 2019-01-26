context("ping")

vcr::use_cassette("ncbi_ping", {
  test_that("ncbi_ping returns the correct value", {
    expect_true(ncbi_ping())
    Sys.sleep(1)
    expect_false(ncbi_ping(503))
    Sys.sleep(1)
    expect_true(ncbi_ping("content"))
  })
})

vcr::use_cassette("trpicos_ping", {
  test_that("trpicos_ping returns the correct value", {
    expect_true(tropicos_ping())
    expect_true(tropicos_ping("content"))
  })
})

vcr::use_cassette("nbn_ping", {
  test_that("nbn_ping returns the correct value", {
    expect_true(nbn_ping())
    expect_false(nbn_ping(503))
    expect_true(nbn_ping("content"))
  })
})

vcr::use_cassette("gbif_ping", {
  test_that("gbif_ping returns the correct value", {
    expect_true(gbif_ping())
    expect_false(gbif_ping(503))
    expect_true(gbif_ping("content"))
  })
})

vcr::use_cassette("bold_ping", {
  test_that("bold_ping returns the correct value", {
    expect_true(bold_ping())
    expect_false(bold_ping(503))
    expect_true(bold_ping("content"))
  })
})

# test_that("ipni_ping returns the correct value", {
#   skip_on_cran()
#
#   expect_true(ipni_ping())
#   expect_false(ipni_ping(503))
#   expect_true(ipni_ping("content"))
# })

vcr::use_cassette("vascan_ping", {
  test_that("vascan_ping returns the correct value", {
    expect_true(vascan_ping())
    expect_false(vascan_ping(503))
    expect_true(vascan_ping("content"))
  })
})
