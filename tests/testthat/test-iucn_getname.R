context("iucn_getname")

temp <- iucn_getname(name = "Cyanistes caeruleus", verbose = FALSE)

test_that("iucn_getname returns the correct value", {
	expect_equal(temp, "Parus caeruleus")
})

test_that("iucn_getname returns the correct class", {
	expect_is(temp, "character")
})

test_that("iucn_getname gets the right dimensions", {
  expect_equal(length(temp), 1)
})
