context("iucn_getname")

test_that("iucn_getname returns the correct value", {
  skip_on_cran()
      
  vcr::use_cassette("iucn_getname", {
    temp <- sm(iucn_getname("Cyanistes caeruleus", verbose = FALSE))
  })

  expect_equal(temp, "Cyanistes caeruleus")
  expect_is(temp, "character")
  expect_equal(length(temp), 1)
})
