context("itis_refs")

test_that("itis_refs returns the correct class", {
  vcr::use_cassette("itis_refs", {
    one <- itis_refs(202385, verbose=FALSE)
    two <- itis_refs(c(202385,70340), verbose=FALSE)
  })

  expect_that(one, is_a("list"))
  expect_that(one[[1]], is_a("data.frame"))
  expect_that(two, is_a("list"))
  expect_that(two[[1]], is_a("data.frame"))
})
