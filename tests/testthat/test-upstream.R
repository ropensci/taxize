context("upstream")

test_that("upstream basic usage works", {
  skip_on_cran()
  vcr::use_cassette("upstream", {
    aa <- upstream("Pinus contorta", db = 'itis', upto = 'genus',
        messages = FALSE)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(aa, "upstream")
  expect_named(aa, "Pinus contorta")
  expect_is(aa$`Pinus contorta`$tsn, "character")
  expect_is(aa$`Pinus contorta`$taxonname, "character")
  expect_is(aa$`Pinus contorta`$rankid, "character")
})

# FIXME: no longer needed as only 1 option now
# test_that("warn on mismatch 'db'", {
#   skip_on_cran()
#   expect_warning(
#     upstream(
#       get_tsn('Pinus contorta', messages = FALSE), db = 'col',
#       upto = 'genus'))
# })
