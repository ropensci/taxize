context("sci2comm")

test_that("sci2comm returns the correct value", {
  skip_on_cran() # uses secrets
  vcr::use_cassette("sci2comm", {
    tt <- sci2comm(sci = 'Helianthus annuus', db = 'ncbi', messages = FALSE)
    uu <- sw(sci2comm(sci = 'Helianthus annuus', db = 'itis', rows = 1,
                      messages = FALSE))
    zz <- sci2comm(sci = 'Pomatomus saltatrix', db = 'worms',
                   messages = FALSE)
  })

  expect_that(names(tt), equals('Helianthus annuus'))
  expect_that(names(uu), equals('Helianthus annuus'))
  expect_that(names(zz), equals('Pomatomus saltatrix'))

  expect_that(tt, is_a("list"))
  expect_that(tt[[1]], is_a("character"))

  expect_that(uu, is_a("list"))
  expect_that(uu[[1]], is_a("character"))

  expect_that(zz, is_a("list"))
  expect_that(zz[[1]], is_a("character"))
})

test_that("warn on mismatch 'db'", {
  skip_on_cran()
  # vcr::use_cassette("sci2comm_warn_on_db_mismatch", {
  #   expect_warning(
  #     sci2comm(
  #       get_tsn('Helianthus annuus', messages = FALSE), db = "ncbi"))
  # })
})
