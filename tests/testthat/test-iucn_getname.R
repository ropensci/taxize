context("iucn_getname")

temp <- iucn_getname(name = "Cyanistes caeruleus", verbose = FALSE)

test_that("iucn_getname returns the correct value", {
	expect_that(temp, equals("Parus caeruleus"))
})

test_that("iucn_getname returns the correct class", {
	expect_that(temp, is_a("character"))
})

test_that("iucn_getname gets the right dimensions", {
  expect_that(length(temp), equals(1))
})
