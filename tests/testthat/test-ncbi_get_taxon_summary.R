# tests for ncbi_get_taxon_summary

context("ncbi_get_taxon_summary")


tt <- ncbi_get_taxon_summary(c(4751))
tt2 <- ncbi_get_taxon_summary(NA)
tt3 <- ncbi_get_taxon_summary(id = NULL)

test_that("ncbi_get_taxon_summary returns correct class and result", {
  expect_is(tt, "data.frame")
  expect_equal(ncol(tt), 3)
  expect_equal(tt[1, 3], 'kingdom')
  expect_equal(tt2, NA)
  expect_equal(tt3, NULL)
})

