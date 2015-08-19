# tests for gnr_resolve fxn in taxize
context("gnr_resolve")

tmp <- gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"))

test_that("gnr_resolve returns the correct value", {
	expect_that(ncol(tmp[[1]]), equals(4))
})

test_that("gnr_resolve returns the correct class", {
	expect_is(tmp, "list")
	expect_is(tmp[[1]], "data.frame")
	expect_is(tmp[[1]]$matched_name, "character")
	expect_null(tmp[[2]])
})

test_that("best_match_only works correctly", {
  x <- 'Aconitum degeni subsp. paniculatum'
  a <- gnr_resolve(names = x, best_match_only = TRUE)
  b <- gnr_resolve(x, best_match_only = FALSE)

  expect_is(a, "list")
  expect_is(b, "list")
  expect_is(a$results, "character")
  expect_is(b$results, "data.frame")
})

test_that("canonical works correctly", {
  y <- "Helianthus annuus"
  w <- gnr_resolve(y, canonical = TRUE)
  z <- gnr_resolve(y, canonical = FALSE)

  expect_is(w, "list")
  expect_is(z, "list")
  expect_named(w$results, c("submitted_name", "data_source_title", "score", "matched_name2"))
  expect_named(z$results, c("submitted_name", "matched_name", "data_source_title", "score"))
})
