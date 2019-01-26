context("tnrs")

test_that("tnrs works", {
  mynames <- c("Helianthus annuus", "Poa annua", "Mimulus bicolor")
  vcr::use_cassette("tnrs", {
    out <- tnrs(query = mynames, source = "iPlant_TNRS", messages = FALSE)
  })

  expect_equal(ncol(out), 7)
  expect_is(out, "data.frame")
})

test_that("tnrs returns user supplied order and row.names NULLed", {
  xxx <- c("Abies concolor", "Abies lasiocarpa",
           "Acer sp.", "Acer campestre",
           "Artemisia borealis", "Artemisia cana",
           "Brassica napus", "Brassica oleracea")
  vcr::use_cassette("tnrs_order_row_names", {
    out <- tnrs(xxx, source = "iPlant_TNRS", messages = FALSE)
  })

  expect_equal(NCOL(out), 7)
  expect_equal(NROW(out), 8)
  expect_is(out, "data.frame")

  # row.names are sequential
  expect_equal(as.numeric(row.names(out)), 1:8)
})
