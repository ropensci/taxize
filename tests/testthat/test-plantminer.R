context("plantminer")

test_that("plantminer returns the correct value", {
  vcr::use_cassette("plantminer", {
    plants <- c("Myrcia lingua", "Myrcia bella", "Ocotea pulchella",
                "Miconia", "Coffea arabica var. amarella", "Bleh")
    df <- plantminer(plants, messages = FALSE)
  })

	expect_is(df, "data.frame")
	expect_is(df$id, "character")

	expect_equal(df$original.search[1], plants[1])
})

test_that("plantminer fails well", {
  expect_error(plantminer(), "argument \"plants\" is missing")
  
  vcr::use_cassette("plantminer_not_found", {
    expect_equal(plantminer("foo bar", messages=FALSE)$note, "not found")
  })
})
