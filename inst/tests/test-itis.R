# tests for itis fxn in taxize
context("itis")

require(ritis)
test_that("itis returns the correct value", {
	expect_that(itis(202420, 'getlsidfromtsn')[[1]], matches("urn:lsid:itis.gov:itis_tsn:202420"))
	expect_that(as.character(itis(36616, "getfullhierarchyfromtsn")[[1]][2,1]), matches("Plantae"))
})

test_that("itis returns the correct class", {
	expect_that(itis(36616, "getfullhierarchyfromtsn")[[1]], is_a("data.frame"))
})
