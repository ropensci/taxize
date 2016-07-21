context("gnr_resolve")

test_that("gnr_resolve returns the correct value", {
  skip_on_cran()

  tmp <- gnr_resolve(names = c("Helianthus annuus", "Homo sapiens"))

  expect_equal(NCOL(tmp), 5)

	expect_is(tmp, "data.frame")
	expect_is(tmp$matched_name, "character")
})

test_that("best_match_only works correctly", {
  skip_on_cran()

  x <- 'Aconitum degeni subsp. paniculatum'
  a <- gnr_resolve(names = x, best_match_only = TRUE)
  b <- gnr_resolve(x, best_match_only = FALSE)

  expect_is(a, "data.frame")
  expect_is(b, "data.frame")
  expect_equal(NROW(a), 0)
  expect_equal(attributes(a)$not_known, x)
  expect_named(attributes(a), c("names", "row.names", "class", "not_known"))
  expect_is(b$data_source_title, "character")
})

test_that("canonical works correctly", {
  skip_on_cran()

  # x = a canse where no canonical names is found
  x <- gnr_resolve("Metzgeria", data_source_ids = c(12), canonical = TRUE)
  y <- "Helianthus annuus"
  w <- gnr_resolve(y, canonical = TRUE)
  z <- gnr_resolve(y, canonical = FALSE)

  expect_is(w, "data.frame")
  expect_is(z, "data.frame")
  expect_named(w, c("user_supplied_name", "submitted_name", "data_source_title", "score", "matched_name2"))
  expect_named(z, c("user_supplied_name", "submitted_name", "matched_name", "data_source_title", "score"))
  expect_equal(NROW(x), 2)
  expect_true(is.na(x$matched_name2[2]))
})

test_that("fields parameter works correctly", {
  skip_on_cran()

  tmp1 <- gnr_resolve(names = c("Asteraceae", "Plantae"), fields = 'all')
  tmp2 <- gnr_resolve(names = c("Asteraceae", "Plantae"), fields = 'minimal')

  expect_is(tmp1, "data.frame")
  expect_is(tmp1$matched_name, "character")
  expect_true(any(grepl("Asteraceae", tmp1$matched_name)))
  expect_is(tmp2, "data.frame")
  expect_is(tmp2$matched_name, "character")
  expect_true(any(grepl("Asteraceae", tmp2$matched_name)))
  expect_true(identical(tmp1$matched_name, tmp2$matched_name))

  expect_lt(NCOL(tmp2), NCOL(tmp1))
})

test_that("works correctly when no data found for preferred data source", {
  skip_on_cran()

  aa <- gnr_resolve("Scabiosa triandra", preferred_data_sources = c(3,12), best_match_only = TRUE)

  expect_is(aa, "data.frame")
  expect_equal(NROW(aa), 1)
  expect_equal(length(attributes(aa)$not_known), 0)
})
