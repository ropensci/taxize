# tests for itis_lsid fxn in taxize
context("itis_lsid")


test_that("itis_lsid returns the correct value", {
  skip_on_cran()

  one <- itis_lsid("urn:lsid:itis.gov:itis_tsn:180543")
  two <- itis_lsid("urn:lsid:itis.gov:itis_tsn:180543", "record")
  three <- itis_lsid("urn:lsid:itis.gov:itis_tsn:180543", "fullrecord")
  four <- itis_lsid(202385)

  expect_equal(one, "180543")
  expect_match(as.character(two[1, "genuspart"]), "Ursus")
  expect_match(three$acceptedNameList$tsn, "180543")
  expect_null(four)

  expect_is(one, "character")
  expect_is(two, "data.frame")
  expect_is(three, "list")
})
