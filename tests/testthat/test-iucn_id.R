context("iucn_id")


test_that("iucn_id returns the correct class", {
  skip_on_cran()
  if (is.na(getkey(NA, service = "iucn"))) {
    skip("No IUCN api key so test not run.")
  }

  aa <- iucn_id("Branta canadensis")
  bb <- iucn_id("Panthera uncia")
  cc <- iucn_id("Lynx lynx")

  expect_is(aa, "integer")
  expect_is(bb, "integer")
  expect_is(cc, "integer")

  expect_equal(aa, 22679935)
  expect_equal(bb, 22732)
  expect_equal(cc, 12519)
})

test_that("iucn_id fails well", {
  skip_on_cran()
  if (is.na(getkey(NA, service = "iucn"))) {
    skip("No IUCN api key so test not run.")
  }
  
  expect_error(iucn_id(), "argument \"sciname\" is missing")
  expect_equal(suppressWarnings(iucn_id("foo bar")), NA)
})
