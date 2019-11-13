context("upstream")

test_that("upstream basic usage works", {
  vcr::use_cassette("upstream", {
    aa <- upstream("Pinus contorta", db = 'col', upto = 'genus',
        messages = FALSE)
  }, preserve_exact_body_bytes = TRUE)

  expect_is(aa, "upstream")
  expect_named(aa, "Pinus contorta")
  expect_is(aa$`Pinus contorta`$childtaxa_id, "character")
  expect_is(aa$`Pinus contorta`$childtaxa_name, "character")
  expect_is(aa$`Pinus contorta`$childtaxa_extinct, "logical")
})

test_that("warn on mismatch 'db'", {
  expect_warning(
    upstream(
      get_tsn('Pinus contorta', messages = FALSE), db = 'col',
      upto = 'genus'))
})
