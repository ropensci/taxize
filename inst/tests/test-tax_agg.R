# tests for tax_agg fxn in taxize
context("tax_agg")

data(dune, package='vegan')
take <- dune[ ,1:5]
species <- c("Bellis perennis", "Empetrum nigrum", "Juncus bufonius", "Juncus articulatus", 
"Aira praecox")
colnames(take) <- species
out_ncbi <- tax_agg(take, rank = 'family', db = 'ncbi')


test_that("tax_agg returns the correct class", {
  expect_that(out_ncbi, is_a("list"))
  expect_that(length(out_ncbi), equals(4))
  expect_equal(class(out_ncbi), "tax_agg")
  expect_that(out_ncbi$x, is_a("data.frame"))
})

test_that("tax_agg returns the correct value", {
  expect_that(out_ncbi$x$Juncaceae[13], equals(8))
  expect_that(nrow(out_ncbi$x), equals(nrow(take)))
  expect_that(nrow(out_ncbi$by), equals(length(unique(colnames(take)))))
  expect_that(out_ncbi$n_pre, equals(length(unique(colnames(take)))))
})


take2 <- take
colnames(take2)[4] <- 'xxxxx'
out_itis <- tax_agg(take2, rank = 'family', db = 'itis')

#test_that("Handles NAs", {
#
#})