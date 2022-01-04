context("rankagg")

test_that("rankagg throws error", {
  skip_on_cran()
  skip_if_not_installed("vegan")

  data(dune.taxon, package = 'vegan')
  dat <- dune.taxon
  set.seed(1234)
  dat$abundance <- round(rlnorm(n = nrow(dat), meanlog = 5, sdlog = 2), 0)
  out <- rankagg(data = dat, datacol = "abundance", rank = "Genus")

  expect_error(rankagg(datacol = "abundance", rank = "Genus"))
  expect_error(rankagg(data = dat, datacol = "abundance"))

  expect_is(out, "data.frame")
  expect_equal(out[1, 2, drop = TRUE], 13)
})
