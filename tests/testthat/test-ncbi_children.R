# tests for ncbi_get_taxon_summary

context("ncbi_children")

tt <- ncbi_children(id = 4751)
tt2 <- ncbi_children(id = 4751, out_type = 'uid')


test_that("ncbi_childres returns correct class and result", {
  expect_is(tt, "list")
  expect_is(tt[[1]], "data.frame")
  expect_equal(ncol(tt[[1]]), 3)
  expect_is(tt1, "list")
  expect_is(tt1[[1]], "vector")
  expect_error(ncbi_children(name = 'Ilex', id = 4751))
  expect_equal(ncbi_children(name = NA)[[1]], NA)
})
