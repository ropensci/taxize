# tests for plantminer fxn in taxize
context("plantminer")

plants <- c("Myrcia lingua", "Myrcia bella")
df <- plantminer(plants, verbose=FALSE)

test_that("plantminer returns the correct value", {
	expect_that(as.character(df[1,1]), matches("Myrtaceae"))
})

test_that("plantminer returns the correct class", {
	expect_that(df, is_a("data.frame"))
})
