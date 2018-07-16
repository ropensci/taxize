# tests for ncbi_get_taxon_summary

context("ncbi_get_taxon_summary")

test_that("ncbi_get_taxon_summary returns correct class and result", {
  skip_on_cran()

  tt <- ncbi_get_taxon_summary(c(4751))
  tt2 <- ncbi_get_taxon_summary(NA)
  tt3 <- ncbi_get_taxon_summary(id = NULL)

  expect_is(tt, "data.frame")
  expect_equal(ncol(tt), 3)
  expect_equal(tt[1, 3], 'kingdom')
  expect_equal(tt2, NA)
  expect_equal(tt3, NULL)
})


test_that("ncbi_get_taxon_summary behaves correctly when very large ID vector", {
  skip_on_cran()

  # short UID's are okay at larger quantities
  ids <- rep(4751, 550)
  expect_is(ncbi_get_taxon_summary(id = ids), "data.frame")

  # but longer IDs add up of course
  ids <- rep(1430660, 1100)
  expect_message(ncbi_get_taxon_summary(id = ids), "splitting up")
})
