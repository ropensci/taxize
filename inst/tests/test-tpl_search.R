# tests for tpl_search fxn in taxize
context("tpl_search")

splist <- c("Heliathus annuus","Abies procera","Poa annua","Platanus occidentalis")
df <- tpl_search(taxon = splist)

test_that("tpl_search returns the correct value", {
	expect_that(df[1,1], matches("Heliathus"))
	expect_that(df[4,8], matches("occidentalis"))
})

test_that("tpl_search returns the correct class", {
	expect_that(df, is_a("data.frame"))
})
