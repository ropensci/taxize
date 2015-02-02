# tests for col_downstream fxn in taxize
context("col_downstream")

temp1 <- col_downstream(name="Apis", downto="Species", verbose = FALSE)
temp3 <- col_downstream(name="Helianthus", downto = "Species", verbose = FALSE)
temp4 <- col_downstream(name="Animalia", downto = "Phylum", verbose = FALSE)
temp5 <- col_downstream(name="Plantae", downto = "Phylum", verbose = FALSE)
temp6 <- col_downstream(name="Salicaceae", downto = "Genus", verbose = FALSE)
temp12 <- col_downstream(name=c("Apis","Accipiter","Collomia","Buteo"), downto = "Species", verbose = FALSE)

test_that("col_downstream returns the correct value", {
	expect_that(as.character(temp1[[1]][1,2]), equals("Apis andreniformis"))
})

test_that("col_downstream returns the correct class", {
  expect_that(temp1, is_a("list"))
  expect_that(temp3, is_a("list"))
  expect_that(temp4, is_a("list"))
  expect_that(temp5, is_a("list"))
  expect_that(temp6, is_a("list"))
  expect_that(temp12, is_a("list"))
  expect_that(temp1[[1]], is_a("data.frame"))
  expect_named(temp3)
  expect_identical(temp3, col_downstream(name="Helianthus", downto = "Species", verbose = FALSE))
  expect_equal(as.character(temp4[[1]][,3][[1]]), "Phylum")
})

test_that("gives what's expected on input errors", {
  library("plyr")
  expect_message(col_downstream(name="Pinus contorta", downto = "Species")[[1]], "Try adjusting")
#   expect_is(col_downstream(name=c("Buteo","Puma"), downto = "Family"), "list")
#   expect_message(col_downstream(name=c("Buteo","Puma"), downto = "Family"), "Try adjusting")
})
