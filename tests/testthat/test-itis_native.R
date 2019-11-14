context("itis_native")

test_that("itis_native returns the correct class", {
  skip_on_cran()
  vcr::use_cassette("itis_native", {
    one <- itis_native(what="values")
    two <- itis_native(what="originvalues")
    three <- itis_native(tsn=180543)
    four <- itis_native(tsn=c(180543,41074,36616))
  })

  expect_is(one, "character")
  expect_is(two, "data.frame")
  expect_is(two, "tbl_df")
  expect_is(three, "data.frame")
  expect_is(three, "tbl_df")
  expect_is(four, "list")
  expect_is(four[[1]], "data.frame")
  expect_is(four[[1]], "tbl_df")
})
