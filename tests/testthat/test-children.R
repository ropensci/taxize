context("children")

test_that("children returns the correct values and classes", {
  skip_on_cran()

  ch_ncbi <- children("Salmo", db = 'ncbi')

  expect_is(ch_ncbi, "children")
  expect_equal(attr(ch_ncbi, "db"), "ncbi")
  expect_named(ch_ncbi, "Salmo")
  expect_is(ch_ncbi$Salmo, "data.frame")
  expect_named(ch_ncbi$Salmo, c('childtaxa_id', 'childtaxa_name', 'childtaxa_rank'))
})

test_that("passing in an id works", {
  skip_on_cran()

  ch_ncbi <- children(8028, db = 'ncbi')

  expect_is(ch_ncbi, "children")
  expect_equal(attr(ch_ncbi, "db"), "ncbi")
  expect_named(ch_ncbi, '8028')
  expect_is(ch_ncbi$`8028`, "data.frame")
  expect_named(ch_ncbi$`8028`, c('childtaxa_id', 'childtaxa_name', 'childtaxa_rank'))
})

test_that("queries with no results fail well", {
  skip_on_cran()

  aa <- children(x = "Saurauia", db = "itis", verbose = FALSE)
  expect_equal(NROW(aa[[1]]), 0)
})

test_that("rows parameter, when used, works", {
  skip_on_cran()

  expect_is(children("Asdfafsfd", db = 'ncbi', rows = 1, verbose = FALSE), "children")
})
