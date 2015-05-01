# tests for tax_agg fxn in taxize
context("tax_agg")

suppressPackageStartupMessages(library("vegan"))
data(dune, package='vegan')
take <- dune[ ,1:5]
species <- c("Bellis perennis", "Empetrum nigrum", "Juncus bufonius", "Juncus articulatus",
"Aira praecox")
colnames(take) <- species
out_ncbi <- tax_agg(take, rank = 'family', db = 'ncbi', verbose = FALSE)
take2 <- take
colnames(take2) <- NULL



test_that("tax_agg returns the correct class", {
  expect_that(out_ncbi, is_a("tax_agg"))
  expect_that(length(out_ncbi), equals(4))
  expect_equal(class(out_ncbi), "tax_agg")
  expect_is(out_ncbi$x, "data.frame")
})

test_that("tax_agg returns the correct value", {
  expect_that(nrow(out_ncbi$x), equals(nrow(take)))
  expect_that(nrow(out_ncbi$by), equals(length(unique(colnames(take)))))
  expect_that(out_ncbi$n_pre, equals(length(unique(colnames(take)))))
  expect_error(tax_agg(as.matrix(take2), rank = 'family', db = 'ncbi', verbose = FALSE))
})


## Moving this test to the long running tests branch since it takes a long time
# take2 <- take
# colnames(take2)[4] <- 'xxxxx'
# out_itis <- tax_agg(take2, rank = 'family', db = 'itis', verbose = FALSE)
#
#
# test_that("Handles NAs", {
#   expect_true(is.na(out_itis$by$agg[4]))
#   expect_that(ncol(out_itis$x), equals(ncol(take2)))
# })
