# tests for col_downstream fxn in taxize
context("col_downstream")

temp1 <- col_downstream(name="Apis", downto="Species")
temp2 <- col_downstream(name="Puma", "Species")
temp3 <- col_downstream(name="Helianthus", "Species")
temp4 <- col_downstream(name="Animalia", "Phylum")
temp5 <- col_downstream(name="Plantae", "Phylum")
temp6 <- col_downstream(name="Salicaceae", "Genus")
temp8 <- col_downstream(name="Poa", "Species")
temp9 <- col_downstream(name="Ursus", "Species")
temp10 <- col_downstream(name="Accipiter", "Species")
temp12 <- col_downstream(name=c("Apis","Accipiter","Collomia","Buteo"), "Species")

test_that("col_downstream returns the correct value", {
	expect_that(as.character(temp1[[1]][1,2]), equals("Apis andreniformis"))
})

test_that("col_downstream returns the correct class", {
  expect_that(temp1, is_a("list"))
  expect_that(temp3, is_a("list"))
  expect_that(temp4, is_a("list"))
  expect_that(temp5, is_a("list"))
  expect_that(temp6, is_a("list"))
  expect_that(temp7, is_a("list"))
  expect_that(temp8, is_a("list"))
  expect_that(temp9, is_a("list"))
  expect_that(temp10, is_a("list"))
  expect_that(temp12, is_a("list"))
  expect_that(temp2[[1]], is_a("data.frame"))
  expect_named(temp3)
  expect_identical(temp3, col_downstream(name="Helianthus", "Species"))
  expect_equal(as.character(temp4[[1]][,3][[1]]), "Phylum")
})

test_that("gives what's expected on input errors", {
  library(plyr)
  expect_null(col_downstream(name="Pinus contorta", "Species")[[1]])
  expect_is(col_downstream(name=c("Buteo","Puma"), "Family"), "list")
})