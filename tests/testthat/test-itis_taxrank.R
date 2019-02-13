context("itis_taxrank")

test_that("itis_taxrank returns the correct value", {
  vcr::use_cassette("itis_taxrank", {
    temp <- itis_taxrank(query=202385, verbose=FALSE)
  })

  expect_match(as.character(temp), "Subspecies")
	expect_is(temp, "character")
})
