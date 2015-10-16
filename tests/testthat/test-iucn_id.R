context("iucn_id")

aa <- iucn_id("Branta canadensis")
bb <- iucn_id("Panthera uncia")
cc <- iucn_id("Lynx lynx")

test_that("iucn_id returns the correct class", {
  expect_is(aa, "list")
  expect_is(aa[[1]], "character")
  expect_is(bb, "list")
  expect_is(bb[[1]], "character")
  expect_is(cc, "list")
  expect_is(cc[[1]], "character")
})

test_that("iucn_id returns the correct value", {
  expect_equal(aa[[1]], "141453")
  expect_equal(bb[[1]], "22732")
  expect_equal(cc[[1]], "12519")
})

test_that("iucn_id fails well", {
  expect_error(iucn_id(), "argument \"sciname\" is missing")
  expect_equal(length(iucn_id("foo bar")), 0)
})
