context("iucn_id")

test_that("iucn_id returns the correct class", {
  skip_on_cran()

  if (Sys.getenv('IUCN_REDLIST_KEY') == "") {
    skip("No IUCN api key so test not run.")
  }

  vcr::use_cassette("iucn_id", {
    aa <- iucn_id("Branta canadensis")
    bb <- iucn_id("Panthera uncia")
    cc <- iucn_id("Lynx lynx")
  },
    preserve_exact_body_bytes = TRUE,
    match_requests_on = c("method", "query")
  )

  expect_is(aa, "integer")
  expect_is(bb, "integer")
  expect_is(cc, "integer")

  expect_equal(aa, 22679935)
  expect_equal(bb, 22732)
  expect_equal(cc, 12519)
})

test_that("iucn_id fails well", {
  skip_on_cran()
  
  if (Sys.getenv('IUCN_REDLIST_KEY') == "") {
    skip("No IUCN api key so test not run.")
  }

  expect_error(iucn_id(), "argument \"sciname\" is missing")
  
  vcr::use_cassette("iucn_id_fail", {
    expect_equal(suppressWarnings(iucn_id("foo bar")), NA)
  }, match_requests_on = c("method", "query"))
})
