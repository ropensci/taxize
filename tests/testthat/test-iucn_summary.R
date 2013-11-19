context("iucn_summary")

temp <- iucn_summary(c("Panthera uncia", "Lynx lynx"))

test_that("iucn_summary returns the correct value", {
	expect_that(length(temp[[1]]), equals(4))
})

test_that("iucn_summary returns the correct class", {
	expect_that(temp, is_a("iucn"))
})

test_that("iucn_status", {
  expect_that(length(iucn_status(temp)), equals(2))
})