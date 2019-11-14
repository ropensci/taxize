context("col_downstream")

test_that("col_downstream returns the correct class", {
  skip_on_cran()
  vcr::use_cassette("col_downstream", {
    temp4 <- col_downstream(name="Animalia", downto = "Phylum", verbose = FALSE)
    temp5 <- col_downstream(name="Plantae", downto = "Phylum", verbose = FALSE)
    temp6 <- col_downstream(name="Salicaceae", downto = "Genus", verbose = FALSE)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(temp4, "list")
  expect_is(temp5, "list")
  expect_is(temp6, "list")
  expect_is(temp4$Animalia, "data.frame")
  expect_is(temp5[[1]], "data.frame")
  expect_equal(as.character(temp4[[1]][,3][[1]]), "phylum")
})

test_that("gives what's expected on input errors", {
  skip_on_cran()
  library("plyr")
  expect_message(col_downstream(name="Pinus contorta", downto = "Species")[[1]], 
    "Try adjusting")
})
