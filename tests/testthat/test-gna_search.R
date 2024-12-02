context("gna_search")

test_that("gna_search returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("gna_search", {
    x <- gna_search('n:B. bubo ds:1,2 au:Linn. y:1700-') 
  })

	expect_is(x, "data.frame")
  expect_equal(x$id[1], "4431a0f3-e901-519a-886f-9b97e0c99d8e")
})
