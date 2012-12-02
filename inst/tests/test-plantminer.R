# tests for plantminer fxn in taxize
context("plantminer")

plants <- c("Myrcia lingua", "Myrcia bella", "Ocotea pulchella", 
 		"Miconia", "Coffea arabica var. amarella", "Bleh")
df <- plantminer(plants)

test_that("plantminer returns the correct value", {
	expect_that(df[1,1], matches("Myrtaceae"))
	expect_that(as.numeric(as.character(df[5,6])), equals(100170231))
})

test_that("plantminer fails and throws error", {
	expect_that(plantminer("adf"), throws_error())
})

test_that("plantminer returns the correct class", {
	expect_that(df, is_a("data.frame"))
})
