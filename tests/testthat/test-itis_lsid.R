# tests for itis_lsid fxn in taxize
context("itis_lsid")

one <- itis_lsid("urn:lsid:itis.gov:itis_tsn:180543", verbose=FALSE)
two <- itis_lsid("urn:lsid:itis.gov:itis_tsn:180543", "record", verbose=FALSE)
three <- itis_lsid("urn:lsid:itis.gov:itis_tsn:180543", "fullrecord", verbose=FALSE)
four <- itis_lsid(202385, verbose=FALSE)

test_that("itis_lsid returns the correct value", {
  expect_that(one, equals(180543))
  expect_that(as.character(two[1,2]), matches("Ursus"))
  expect_that(three$acceptedNameList$tsn, matches("180543"))
  expect_that(four, matches("invalid TSN"))
})

test_that("itis_lsid returns the correct class", {
  expect_that(one, is_a("numeric"))
  expect_that(two, is_a("data.frame"))
  expect_that(three, is_a("list"))
  expect_that(four, is_a("character"))
})

test_that("itis_lsid returns correct error when given nonsense", {
  expect_message(itis_lsid("urn:lsid:itis.gov:itis_tsn:180543", verbose=TRUE))
})