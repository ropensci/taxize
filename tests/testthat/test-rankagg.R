# tests for rankagg fxn in taxize
context("rankagg")
data(dune.taxon, package = 'vegan')
dat <- dune.taxon
set.seed(1234)
dat$abundance <- round(rlnorm(n = nrow(dat), meanlog = 5, sdlog = 2), 0)
out <- rankagg(data = dat, datacol = "abundance", rank = "Genus")


test_that("rankagg throws error", {
  expect_error(rankagg(datacol = "abundance", rank = "Genus"))
  expect_error(rankagg(data = dat, datacol = "abundance"))
})


test_that("Correct result", {
  expect_is(out, "data.frame")
  expect_equal(out[1, 2], 13)
})
