context("plantminer")

test_that("plantminer returns the correct value", {
  plants <- c("Myrcia lingua", "Myrcia bella", "Ocotea pulchella",
              "Miconia", "Coffea arabica var. amarella", "Bleh")
  df <- plantminer(plants, verbose = FALSE)

	expect_is(df, "data.frame")
	expect_is(df$id, "character")

	expect_equal(df$original.search[1], plants[1])
})

test_that("plantminer fails well", {
  expect_error(plantminer(), "argument \"plants\" is missing")
  expect_equal(plantminer("foo bar", verbose=FALSE)$note, "not found")
})
