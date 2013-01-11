# tests for fresh_validate fxn in taxize
context("fresh_validate")

spec <- c("Acentrella sinaica",
 "Acentria ephemerella",
 "Acilius sp.",
 "Acroloxus lacustris",
 "Allotrichi pallicornis",
          "xxxx")
  
a <- fresh_validate(spec)

test_that("fresh_validate returns the correct value", {
	expect_that(a[[1]][1,2], matches("Acentrella"))
  expect_that(a[[1]]$Status[6], matches("not valid"))
})

test_that("classification returns the correct class", {
	expect_that(a, is_a("tvt"))
	expect_that(a[[1]], is_a("data.frame"))
})

