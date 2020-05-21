context("gni_search")

test_that("gni_search returns the correct value", {
  skip_on_cran()
  vcr::use_cassette("gni_search", {
    x <- gni_search(sci = "ama*", per_page = 1) 
  })

	expect_is(x, "data.frame")
  expect_equal(x[,2], "22693003")
})
