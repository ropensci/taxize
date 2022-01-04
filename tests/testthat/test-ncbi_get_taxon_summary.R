context("ncbi_get_taxon_summary")

test_that("ncbi_get_taxon_summary returns correct class and result", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("ncbi_get_taxon_summary", {
    tt <- ncbi_get_taxon_summary(c(4751))
    tt2 <- ncbi_get_taxon_summary(NA)
    tt3 <- ncbi_get_taxon_summary(id = NULL)
  })

  expect_is(tt, "data.frame")
  expect_equal(ncol(tt), 3)
  expect_equal(tt[1, 3, drop = TRUE], 'kingdom')
  expect_equal(tt2, NA)
  expect_equal(tt3, NULL)
})


test_that("ncbi_get_taxon_summary behaves correctly when very large ID vector", {
  skip_on_cran() # uses secrets

  # short UID's are okay at larger quantities
  ids <- rep(4751, 550)
  vcr::use_cassette("ncbi_get_taxon_summary_many_ids_short", {
    x <- ncbi_get_taxon_summary(id = ids)
  })
  expect_is(x, "data.frame")

  # but longer IDs add up of course
  ids <- rep(1430660, 1100)
  vcr::use_cassette("ncbi_get_taxon_summary_many_ids_long", {
    expect_message(ncbi_get_taxon_summary(id = ids), "splitting up")
  })
})
