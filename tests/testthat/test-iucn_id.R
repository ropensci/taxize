context("iucn_id")

aa <- iucn_id("Branta canadensis")
bb <- iucn_id("Panthera uncia")
cc <- iucn_id("Lynx lynx")

test_that("iucn_id returns the correct class", {
  expect_is(aa, "numeric")
  expect_is(bb, "numeric")
  expect_is(cc, "numeric")
})

test_that("iucn_id returns the correct value", {
  expect_equal(aa, 22679935)
  expect_equal(bb, 22732)
  expect_equal(cc, 68986842)
})

test_that("iucn_id fails well", {
  expect_error(iucn_id(), "argument \"sciname\" is missing")
  expect_equal(suppressWarnings(iucn_id("foo bar")), NA)
})
