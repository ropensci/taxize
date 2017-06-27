# tests for itis fxn in taxize
context("tnrs")


test_that("tnrs works", {
  skip_on_cran()

  mynames <- c("Panthera tigris", "Eutamias minimus")
  out <- tnrs(query = mynames, verbose = FALSE)

  expect_that(ncol(out), equals(7))

  expect_that(out, is_a("data.frame"))
})


test_that("tnrs returns user supplied order and row.names NULLed", {
  skip_on_cran()

  xxx <- c("Abies concolor", "Abies lasiocarpa",
           "Acer sp.", "Acer campestre",
           "Artemisia borealis", "Artemisia cana",
           "Brassica napus", "Brassica oleracea")
  out <- tnrs(xxx, source = "iPlant_TNRS", verbose = FALSE)

  expect_equal(NCOL(out), 7)
  expect_equal(NROW(out), 8)
  expect_is(out, "data.frame")

  # row.names are sequential
  expect_equal(as.numeric(row.names(out)), 1:8)
})
